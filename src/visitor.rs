use std::{marker::PhantomData, convert::Infallible, fmt::Debug};

use crate::atoms::{OpenTag, CloseTag};

use super::node::*;


/// Enum that represents the different types with valid Rust code that can be visited using `syn::Visitor`.
/// Usually `syn::Block` or `syn::Expr`.
pub enum RustCode<'a> {
    Block(&'a mut syn::Block),
    Expr(&'a mut syn::Expr),
    LitStr(&'a mut syn::LitStr),
    Pat(&'a mut syn::Pat),
}
/// Visitor api provide a way to traverse the node tree and modify its components.
/// The api allows modification of all types of nodes, and some atoms like InvalidBlock or NodeName.
/// 
/// Each method returns a bool that indicates if the visitor should continue to traverse the tree.
/// If the method returns false, the visitor will stop traversing the tree.
pub trait Visitor {
    type Custom: CustomNode;
    // Visit node types
    fn visit_node(&mut self, _node: &mut  Node<Self::Custom>) -> bool {true}
    fn visit_block(&mut self, _node: &mut NodeBlock) -> bool {true}
    fn visit_comment(&mut self, _node: &mut NodeComment)  -> bool {true}
    fn visit_doctype(&mut self, _node: &mut NodeDoctype)  -> bool {true}
    fn visit_raw_node<OtherC: CustomNode + Clone + Debug>(&mut self, _node: &mut RawText<OtherC>)  -> bool {true}
    fn visit_custom(&mut self, _node: &mut Self::Custom)  -> bool {true}
    fn visit_text_node(&mut self, _node: &mut NodeText) -> bool {true}
    fn visit_element(
        &mut self,
        _node: &mut NodeElement<Self::Custom>,
    ) -> bool {true}
    fn visit_fragment(
        &mut self,
        _node: &mut NodeFragment<Self::Custom>,
    )  -> bool {true}

    // Visit atoms
    fn visit_rust_code(&mut self, _code: RustCode) -> bool {true}
    fn visit_invalid_block(&mut self, _block: &mut InvalidBlock)  -> bool {true}
    fn visit_node_name(&mut self, _name: &mut NodeName)  -> bool {true}

    fn visit_open_tag(&mut self, _open_tag: &mut OpenTag)  -> bool {true}
    fn visit_close_tag(&mut self, _closed_tag: &mut CloseTag)  -> bool {true}
    // Visit Attributes
    fn visit_attribute(&mut self, _attribute: &mut NodeAttribute)  -> bool {true}
    fn visit_keyed_attribute(&mut self, _attribute: &mut KeyedAttribute)  -> bool {true}
    fn visit_attribute_flag(&mut self, _key: &mut NodeName)  -> bool {true}
    fn visit_attribute_binding(&mut self, _key: &mut NodeName, _value: &mut FnBinding) -> bool {true}
    fn visit_attribute_value(&mut self, _key: &mut NodeName, _value: &mut AttributeValueExpr) -> bool {true}

}

macro_rules! visit_child {
    ($self:ident.$visitor:ident.$method:ident($($tokens:tt)*)) => {
        if !$self.$visitor.$method($($tokens)*) {
            return false;
        }
    };
}

macro_rules! try_visit {
    ($self:ident.$method:ident($($tokens:tt)*)) => {
        if !$self.$method($($tokens)*) {
            return false;
        }
    };
}

/// Wrapper for visitor that calls inner visitors. 
/// Inner visitor should implement `Visitor` trait and `syn::visit_mut::VisitMut`.
pub struct VisitorWithDefault<C: CustomNode, V: Visitor + syn::visit_mut::VisitMut > {
    custom: PhantomData<C>,
    visitor: V
}

impl<C, V> Visitor for VisitorWithDefault<C, V> 
where 
C: CustomNode + Debug + Clone,
V: Visitor<Custom = C> + syn::visit_mut::VisitMut 
{
    type Custom = C;
    fn visit_node(&mut self, node: &mut  Node<Self::Custom>) -> bool{
        visit_child!(self.visitor.visit_node(node));

        match node {
            Node::Block(b) => self.visit_block(b),
            Node::Comment(c) => self.visit_comment(c),
            Node::Doctype(d) => self.visit_doctype(d),
            Node::Element(e) => self.visit_element(e),
            Node::Fragment(f) => self.visit_fragment(f),
            Node::Text(t) => self.visit_text_node(t),
            Node::RawText(r) => self.visit_raw_node(r),
            Node::Custom(c) => self.visit_custom(c),
        }
    }
    fn visit_block(&mut self, node: &mut NodeBlock) -> bool {
        visit_child!(self.visitor.visit_block(node));

        match node {
            NodeBlock::Invalid(b) => 
                self.visit_invalid_block(b),
            NodeBlock::ValidBlock(b) => 
                self.visit_rust_code(RustCode::Block(b)),
            
        }
    }
    fn visit_comment(&mut self, node: &mut NodeComment)-> bool {
        visit_child!(self.visitor.visit_comment(node));

        self.visit_rust_code(RustCode::LitStr(&mut node.value))
        
    }
    fn visit_doctype(&mut self, node: &mut NodeDoctype)-> bool {
        visit_child!(self.visitor.visit_doctype(node));

        self.visit_raw_node(&mut node.value)
    }
    fn visit_raw_node<OtherC: CustomNode + Clone + Debug>(&mut self, node: &mut RawText<OtherC>) -> bool{
        visit_child!(self.visitor.visit_raw_node(node));

        true
    }
    fn visit_custom(&mut self, node: &mut Self::Custom)-> bool {
        visit_child!(self.visitor.visit_custom(node));

        true
    }
    fn visit_text_node(&mut self, node: &mut NodeText)-> bool {
        visit_child!(self.visitor.visit_text_node(node));

        self.visit_rust_code(RustCode::LitStr(&mut node.value))
    }
    fn visit_element(
        &mut self,
        node: &mut NodeElement<Self::Custom>,
    )-> bool {
        visit_child!(self.visitor.visit_element(node));

        try_visit!(self.visit_open_tag(&mut node.open_tag));

        for attribute in node.attributes_mut() {
            try_visit!(self.visit_attribute(attribute))
        }
        for child in node.children_mut() {
            try_visit!(self.visit_node(child))
        }

        if let Some(close_tag) = &mut node.close_tag {
            try_visit!(self.visit_close_tag(close_tag));
        }
        true
    }
    fn visit_fragment(
        &mut self,
        node: &mut NodeFragment<Self::Custom>,
    )-> bool {
        visit_child!(self.visitor.visit_fragment(node));

        for child in node.children_mut() {
            try_visit!(self.visit_node(child))
        }
        true
    }

    fn visit_open_tag(&mut self, open_tag: &mut OpenTag)  -> bool {
        visit_child!(self.visitor.visit_open_tag(open_tag));

        try_visit!(self.visit_node_name(&mut open_tag.name));

        true
    }
    fn visit_close_tag(&mut self, closed_tag: &mut CloseTag)  -> bool {
        visit_child!(self.visitor.visit_close_tag(closed_tag));

        try_visit!(self.visit_node_name(&mut closed_tag.name));

        true
    }

    fn visit_attribute(&mut self, attribute: &mut NodeAttribute)-> bool {
        visit_child!(self.visitor.visit_attribute(attribute));

        match attribute {
            NodeAttribute::Attribute(a) => self.visit_keyed_attribute(a),
            NodeAttribute::Block(b) => self.visit_block(b),
        }
    }
    fn visit_keyed_attribute(&mut self, attribute: &mut KeyedAttribute)-> bool {
        visit_child!(self.visitor.visit_keyed_attribute(attribute));

        match &mut attribute.possible_value {
            KeyedAttributeValue::None => self.visit_attribute_flag(&mut attribute.key),
            KeyedAttributeValue::Binding(b) => self.visit_attribute_binding(&mut attribute.key, b),
            KeyedAttributeValue::Value(v) => self.visit_attribute_value(&mut attribute.key, v),
        }
    }
    fn visit_attribute_flag(&mut self, key: &mut NodeName)-> bool {
        visit_child!(self.visitor.visit_attribute_flag(key));
        true
    }
    fn visit_attribute_binding(&mut self, key: &mut NodeName, value: &mut FnBinding)-> bool {
        visit_child!(self.visitor.visit_attribute_binding(key, value));

        for input in value.inputs.iter_mut(){
            try_visit!(self.visit_rust_code(RustCode::Pat(input)))
        }
        true
        
    }
    fn visit_attribute_value(&mut self, key: &mut NodeName, value: &mut AttributeValueExpr)-> bool {
        visit_child!(self.visitor.visit_attribute_value(key, value));

        self.visit_node_name(key);
        self.visit_rust_code(RustCode::Expr(&mut value.value))
    }

    fn visit_invalid_block(&mut self, block: &mut InvalidBlock)-> bool {
        visit_child!(self.visitor.visit_invalid_block(block));

        true
    }
    fn visit_node_name(&mut self, name: &mut NodeName)-> bool {
        visit_child!(self.visitor.visit_node_name(name));

        true
        
    }
    fn visit_rust_code(&mut self, mut code: RustCode)-> bool {
        {
            // use rewrap because enum `RustCode` is not Copy
            let rewrap = match &mut code {
                RustCode::Block(b) => RustCode::Block(b),
                RustCode::Expr(e) => RustCode::Expr(e),
                RustCode::LitStr(l) => RustCode::LitStr(l),
                RustCode::Pat(p) => RustCode::Pat(p),
            };
            visit_child!(self.visitor.visit_rust_code(rewrap));
        }

        match code {
            RustCode::Block(b) => self.visitor.visit_block_mut(b),
            RustCode::Expr(e) => self.visitor.visit_expr_mut(e),
            RustCode::LitStr(l) => self.visitor.visit_lit_str_mut(l),
            RustCode::Pat(p) => self.visitor.visit_pat_mut(p),
        }

        true
    }
    
}

pub fn visit_nodes<V> (
    nodes: &mut [Node],
    visitor: V,
) -> V
where V: Visitor<Custom = Infallible> + syn::visit_mut::VisitMut
{
    let mut visitor = VisitorWithDefault {
        custom: PhantomData,
        visitor,
    };
    for node in nodes {
        visitor.visit_node(node);
    }
    visitor.visitor
}


pub fn visit_attributes<V> (
    attributes: &mut [NodeAttribute],
    visitor: V,
) -> V
where V: Visitor<Custom = Infallible> + syn::visit_mut::VisitMut
{
    let mut visitor = VisitorWithDefault {
        custom: PhantomData,
        visitor,
    };
    for attribute in attributes {
        visitor.visit_attribute(attribute);
    }
    visitor.visitor
}
#[cfg(test)]
mod tests {
    use std::convert::Infallible;

    use quote::{quote, ToTokens};

    use super::*;
    #[test]
    fn collect_node_names() {
        #[derive(Default)]
        struct TestVisitor {
            collected_names: Vec<NodeName> 
        }
        impl Visitor for TestVisitor {
            type Custom = Infallible;
            fn visit_node_name(&mut self, name: &mut NodeName) -> bool {
                self.collected_names.push(name.clone());
                true
            }
        }
        // empty impl
        impl syn::visit_mut::VisitMut for TestVisitor {

        }

        let stream = quote! {
            <div>
                <span></span>
                <span></span>
            </div>
            <!-- "comment" --> 
            <foo attr key=value> </foo>
        };
        let mut nodes = crate::parse2(stream).unwrap();
        let visitor = visit_nodes(&mut nodes, TestVisitor::default());
        // convert node_names to string;
        let node_names = visitor.collected_names.iter().map(|name| name.to_string()).collect::<Vec<_>>();

        assert_eq!(node_names, vec![
            "div", "span", "span", "span", "span", "div", "foo", "key", "foo"
        ]);
    }

    #[test]
    fn collect_rust_blocks () {
        #[derive(Default)]
        struct TestVisitor {
            collected_blocks: Vec<syn::Block> 
        }
        // empty impl
        impl Visitor for TestVisitor {
            type Custom = Infallible;
        }
        impl syn::visit_mut::VisitMut for TestVisitor {
            fn visit_block_mut(&mut self, i: &mut syn::Block) {
                self.collected_blocks.push(i.clone());
            
            }

        }

        let stream = quote! {
            <div>
            { let block = "in node position"; }
                <span { block_in_attr_position = foo }></span>
                <span var = {block_in_value}></span>
            </div>
            <!-- "comment" --> 
        };
        let mut nodes = crate::parse2(stream).unwrap();
        let visitor = visit_nodes(&mut nodes, TestVisitor::default());
        // convert node_names to string;
        let blocks = visitor.collected_blocks.iter().map(|block| block.to_token_stream().to_string()).collect::<Vec<_>>();

        assert_eq!(blocks, vec![
            "{ let block = \"in node position\" ; }",
            "{ block_in_attr_position = foo }",
            "{ block_in_value }",
        ]);
    }

    #[test] 
    fn collect_raw_text() {
        #[derive(Default)]
        struct TestVisitor {
            collected_raw_text: Vec<RawText<Infallible>> 
        }
        impl Visitor for TestVisitor {
            type Custom = Infallible;
        
            fn visit_raw_node<OtherC: CustomNode + Clone + Debug>(&mut self, node: &mut RawText<OtherC>) -> bool{
                let raw = node.clone().convert_custom::<Infallible>();
                self.collected_raw_text.push(raw);
                true
            }
        }
        // empty impl
        impl syn::visit_mut::VisitMut for TestVisitor {

        }

        let stream = quote! {
            <!Doctype Other raw text >
            <div>
                <span>Some raw text</span>
                <span></span> And text after span
            </div>
            <!-- "comment" --> 
            <foo attr key=value> </foo>
        };
        let mut nodes = crate::parse2(stream).unwrap();
        let visitor = visit_nodes(&mut nodes, TestVisitor::default());
        // convert collected_raw_text to string;
        let raw_text = visitor.collected_raw_text.iter().map(|raw| raw.to_string_best()).collect::<Vec<_>>();

        assert_eq!(raw_text, vec![
            "Other raw text",
            "Some raw text",
            "And text after span",
        ]);
        
            
    }

    #[test] 
    fn collect_string_literals() {
        #[derive(Default)]
        struct TestVisitor {
            collected_literals: Vec<syn::LitStr> 
        }
        impl Visitor for TestVisitor {
            type Custom = Infallible;
        
        }
        impl syn::visit_mut::VisitMut for TestVisitor {
            fn visit_lit_str_mut(&mut self, i: &mut syn::LitStr) {
                self.collected_literals.push(i.clone());
            
            }

        }

        let stream = quote! {
            <!Doctype Other raw text >
            <div>
                <span>"Some raw text"</span>
                <span></span>"And text after span"
            </div>
            <!-- "comment" --> 
            <foo attr key=value> </foo>
        };
        let mut nodes = crate::parse2(stream).unwrap();
        let visitor = visit_nodes(&mut nodes, TestVisitor::default());
        // convert collected_literals to string;
        let literals = visitor.collected_literals.iter().map(|lit| lit.value()).collect::<Vec<_>>();

        assert_eq!(literals, vec![
            "Some raw text",
            "And text after span",
            "comment"
        ]);
    }
}