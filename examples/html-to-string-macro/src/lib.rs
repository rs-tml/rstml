use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::{quote, quote_spanned, ToTokens};
use rstml::{
    node::{Node, NodeAttribute, NodeName},
    visitor::{visit_attributes, visit_nodes, Visitor},
    Infallible, Parser, ParserConfig,
};
use syn::spanned::Spanned;
// mod escape;
#[derive(Default)]
struct WalkNodesOutput {
    static_format: String,
    // Use proc_macro2::TokenStream instead of syn::Expr
    // to provide more errors to the end user.
    values: Vec<proc_macro2::TokenStream>,
    // Additional diagnostic messages.
    diagnostics: Vec<proc_macro2::TokenStream>,
    // Collect elements to provide semantic highlight based on element tag.
    // No differences between open tag and closed tag.
    // Also multiple tags with same name can be present,
    // because we need to mark each of them.
    collected_elements: Vec<NodeName>,
}
struct WalkNodes<'a> {
    empty_elements: &'a HashSet<&'a str>,
    output: WalkNodesOutput,
}
impl<'a> WalkNodes<'a> {
    fn child_output(&self) -> Self {
        Self {
            empty_elements: self.empty_elements,
            output: WalkNodesOutput::default(),
        }
    }
}

impl WalkNodesOutput {
    fn extend(&mut self, other: WalkNodesOutput) {
        self.static_format.push_str(&other.static_format);
        self.values.extend(other.values);
        self.diagnostics.extend(other.diagnostics);
        self.collected_elements.extend(other.collected_elements);
    }
}
impl<'a> syn::visit_mut::VisitMut for WalkNodes<'a> {}

impl<'a> Visitor for WalkNodes<'a> {
    type Custom = Infallible;

    fn visit_doctype(&mut self, doctype: &mut rstml::node::NodeDoctype) -> bool {
        let value = &doctype.value.to_token_stream_string();
        self.output
            .static_format
            .push_str(&format!("<!DOCTYPE {}>", value));
        false
    }
    fn visit_text_node(&mut self, node: &mut rstml::node::NodeText) -> bool {
        self.output.static_format.push_str(&node.value_string());
        false
    }
    fn visit_raw_node<OtherC: rstml::node::CustomNode>(
        &mut self,
        node: &mut rstml::node::RawText<OtherC>,
    ) -> bool {
        self.output.static_format.push_str(&node.to_string_best());
        false
    }
    fn visit_fragment(&mut self, fragment: &mut rstml::node::NodeFragment<Self::Custom>) -> bool {
        let visitor = self.child_output();
        let child_output = visit_nodes(&mut fragment.children, visitor);
        self.output.extend(child_output.output);
        false
    }

    fn visit_comment(&mut self, comment: &mut rstml::node::NodeComment) -> bool {
        self.output
            .static_format
            .push_str(&format!("<!-- {} -->", comment.value.value()));
        false
    }
    fn visit_block(&mut self, block: &mut rstml::node::NodeBlock) -> bool {
        self.output.static_format.push_str("{}");
        self.output.values.push(block.to_token_stream());
        false
    }
    fn visit_element(&mut self, element: &mut rstml::node::NodeElement<Self::Custom>) -> bool {
        let name = element.name().to_string();
        self.output.static_format.push_str(&format!("<{}", name));
        self.output
            .collected_elements
            .push(element.open_tag.name.clone());
        if let Some(e) = &element.close_tag {
            self.output.collected_elements.push(e.name.clone())
        }

        let visitor = self.child_output();
        let attribute_visitor = visit_attributes(element.attributes_mut(), visitor);
        self.output.extend(attribute_visitor.output);

        self.output.static_format.push('>');

        // Ignore childs of special Empty elements
        if self
            .empty_elements
            .contains(element.open_tag.name.to_string().as_str())
        {
            self.output
                .static_format
                .push_str(&format!("/</{}>", element.open_tag.name));
            if !element.children.is_empty() {
                let warning = proc_macro2_diagnostics::Diagnostic::spanned(
                    element.open_tag.name.span(),
                    proc_macro2_diagnostics::Level::Warning,
                    "Element is processed as empty, and cannot have any child",
                );
                self.output.diagnostics.push(warning.emit_as_expr_tokens())
            }

            return false;
        }
        // children

        let visitor = self.child_output();
        let child_output = visit_nodes(&mut element.children, visitor);
        self.output.extend(child_output.output);
        self.output.static_format.push_str(&format!("</{}>", name));
        false
    }
    fn visit_attribute(&mut self, attribute: &mut NodeAttribute) -> bool {
        // attributes
        match attribute {
            NodeAttribute::Block(block) => {
                // If the nodes parent is an attribute we prefix with whitespace
                self.output.static_format.push(' ');
                self.output.static_format.push_str("{}");
                self.output.values.push(block.to_token_stream());
            }
            NodeAttribute::Attribute(attribute) => {
                self.output
                    .static_format
                    .push_str(&format!(" {}", attribute.key));
                if let Some(value) = attribute.value() {
                    self.output.static_format.push_str(r#"="{}""#);
                    self.output.values.push(value.to_token_stream());
                }
            }
        }
        false
    }
}
fn walk_nodes<'a>(empty_elements: &'a HashSet<&'a str>, nodes: &'a mut [Node]) -> WalkNodesOutput {
    let visitor = WalkNodes {
        empty_elements,
        output: WalkNodesOutput::default(),
    };
    let mut nodes = nodes.to_vec();
    let output = visit_nodes(&mut nodes, visitor);
    output.output
}

/// Converts HTML to `String`.
///
/// Values returned from braced blocks `{}` are expected to return something
/// that implements `Display`.
///
/// See [rstml docs](https://docs.rs/rstml/) for supported tags and syntax.
///
/// # Example
///
/// ```
/// use rstml_to_string_macro::html;
/// // using this macro, one should write docs module on top level of crate.
/// // Macro will link html tags to them.
/// pub mod docs {
///     /// Element has open and close tags, content and attributes.
///     pub fn element() {}
/// }
/// # fn main (){
///
/// let world = "planet";
/// assert_eq!(html!(<div>"hello "{world}</div>), "<div>hello planet</div>");
/// # }
/// ```
#[proc_macro]
pub fn html(tokens: TokenStream) -> TokenStream {
    html_inner(tokens, false)
}

/// Same as html but also emit IDE helper statements.
/// Open tests.rs in ide to see semantic highlight/goto def and docs.
#[proc_macro]
pub fn html_ide(tokens: TokenStream) -> TokenStream {
    html_inner(tokens, true)
}

fn html_inner(tokens: TokenStream, ide_helper: bool) -> TokenStream {
    // https://developer.mozilla.org/en-US/docs/Glossary/Empty_element
    let empty_elements: HashSet<_> = [
        "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param",
        "source", "track", "wbr",
    ]
    .into_iter()
    .collect();
    let config = ParserConfig::new()
        .recover_block(true)
        .always_self_closed_elements(empty_elements.clone())
        .raw_text_elements(["script", "style"].into_iter().collect())
        .macro_call_pattern(quote!(html! {%%}));

    let parser = Parser::new(config);
    let (mut nodes, errors) = parser.parse_recoverable(tokens).split_vec();

    let WalkNodesOutput {
        static_format: html_string,
        values,
        collected_elements: elements,
        diagnostics,
    } = walk_nodes(&empty_elements, &mut nodes);
    let docs = if ide_helper {
        generate_tags_docs(&elements)
    } else {
        vec![]
    };
    let errors = errors
        .into_iter()
        .map(|e| e.emit_as_expr_tokens())
        .chain(diagnostics);
    quote! {
        {
            // Make sure that "compile_error!(..);"  can be used in this context.
            #(#errors;)*
            // Make sure that "enum x{};" and "let _x = crate::element;"  can be used in this context
            #(#docs;)*
            format!(#html_string, #(#values),*)
        }
    }
    .into()
}

fn generate_tags_docs(elements: &[NodeName]) -> Vec<proc_macro2::TokenStream> {
    // Mark some of elements as type,
    // and other as elements as fn in crate::docs,
    // to give an example how to link tag with docs.
    let elements_as_type: HashSet<&'static str> = vec!["html", "head", "meta", "link", "body"]
        .into_iter()
        .collect();

    elements
        .into_iter()
        .map(|e| {
            if elements_as_type.contains(&*e.to_string()) {
                let element = quote_spanned!(e.span() => enum);
                quote!({#element X{}})
            } else {
                // let _ = crate::docs::element;
                let element = quote_spanned!(e.span() => element);
                quote!(let _ = crate::docs::#element)
            }
        })
        .collect()
}
