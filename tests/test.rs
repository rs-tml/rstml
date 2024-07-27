use std::{convert::TryFrom, str::FromStr};

use eyre::Result;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use rstml::{
    node::{
        CustomNode, KVAttributeValue, KeyedAttribute, KeyedAttributeValue, Node, NodeAttribute,
        NodeElement, NodeName, NodeType,
    },
    parse2,
    recoverable::{ParseRecoverable, RecoverableContext},
    Parser, ParserConfig,
};
use syn::{
    bracketed,
    parse::ParseStream,
    parse_quote,
    token::{Bracket, Colon},
    Block, LifetimeParam, Pat, PatType, Token, TypeParam,
};

#[test]
fn test_single_empty_element() -> Result<()> {
    let tokens = quote! {
        <foo></foo>
    };
    let nodes = parse2(tokens)?;
    let element = get_element(&nodes, 0);

    assert_eq!(element.name().to_string(), "foo");

    Ok(())
}

#[test]
fn test_single_element_with_attributes() -> Result<()> {
    let tokens = quote! {
        <foo bar="moo" baz="42"></foo>
    };
    let nodes = parse2(tokens)?;

    let attribute = get_element_attribute(&nodes, 0, 0);

    assert_eq!(attribute.key.to_string(), "bar");
    assert_eq!(attribute.value_literal_string().expect("value"), "moo");

    Ok(())
}

#[test]
fn test_single_element_with_text() -> Result<()> {
    let tokens = quote! {
        <foo>"bar"</foo>
    };

    let nodes = parse2(tokens)?;
    let Node::Text(child) = get_element_child(&nodes, 0, 0) else {
        panic!("expected child")
    };

    assert_eq!(child.value.value(), "bar");

    Ok(())
}

#[test]
fn test_single_element_with_unquoted_text_simple() -> Result<()> {
    let tokens = quote! {
        // Note two spaces between bar and baz
        <foo> bar  baz </foo>
    };

    let nodes = parse2(tokens)?;
    let Node::RawText(child) = get_element_child(&nodes, 0, 0) else {
        panic!("expected child")
    };

    // We can't use source text if token stream was created with quote!.
    assert_eq!(child.to_token_stream_string(), "bar baz");
    assert_eq!(child.to_token_stream_string(), child.to_string_best());
    Ok(())
}

#[test]
fn test_css_selector_unquoted_text() -> Result<()> {
    let tokens = quote! {
        // Note two spaces between bar and baz
        --css-selector & with @strange + .puncts
    };

    let nodes = parse2(tokens)?;
    let Node::RawText(child) = &nodes[0] else {
        panic!("expected child")
    };

    // We can't use source text if token stream was created with quote!.
    assert_eq!(
        child.to_token_stream_string(),
        "- - css - selector & with @ strange + . puncts"
    );
    assert_eq!(child.to_token_stream_string(), child.to_string_best());
    Ok(())
}

#[test]
fn test_css_selector_unquoted_text_string() -> Result<()> {
    let tokens = TokenStream::from_str(
        r#"
        <style> --css-selector & with @strange + .puncts </style> 
        "#,
    )
    .unwrap();

    let nodes = parse2(tokens)?;
    let Node::RawText(child) = get_element_child(&nodes, 0, 0) else {
        panic!("expected child")
    };

    // source text should be available
    assert_eq!(
        child.to_source_text(true).unwrap(),
        " --css-selector & with @strange + .puncts "
    );
    assert_eq!(
        child.to_source_text(false).unwrap(),
        "--css-selector & with @strange + .puncts"
    );

    // without source text - it will return invalid css
    assert_eq!(
        child.to_token_stream_string(),
        "-- css - selector & with @ strange + . puncts"
    );
    // When source is available, best should
    assert_eq!(child.to_string_best(), child.to_source_text(true).unwrap());
    Ok(())
}

#[test]
fn test_single_element_with_unquoted_text_advance() -> Result<()> {
    let tokens = TokenStream::from_str(
        r#"
        <foo> bar  baz </foo>
        "#,
    )
    .unwrap();

    let nodes = parse2(tokens)?;
    let Node::RawText(child) = get_element_child(&nodes, 0, 0) else {
        panic!("expected child")
    };

    // source text should be available
    assert_eq!(child.to_source_text(true).unwrap(), " bar  baz ");
    assert_eq!(child.to_source_text(false).unwrap(), "bar  baz");
    assert_eq!(child.to_token_stream_string(), "bar baz");
    // When source is available, best should
    assert_eq!(child.to_string_best(), child.to_source_text(true).unwrap());
    Ok(())
}

#[derive(Clone, Debug)]
struct TestCustomNode {
    bracket: Bracket,
    data: TokenStream,
}

impl ToTokens for TestCustomNode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.bracket.surround(tokens, |c| self.data.to_tokens(c))
    }
}

impl ParseRecoverable for TestCustomNode {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        let inner_parser = |_parser: &mut RecoverableContext, input: ParseStream| {
            let content;
            let bracket = bracketed!(content in input);
            Ok(Some(TestCustomNode {
                bracket,
                data: content.parse()?,
            }))
        };
        parser.parse_mixed_fn(input, inner_parser)?
    }
}

impl CustomNode for TestCustomNode {
    fn peek_element(input: ParseStream) -> bool {
        input.peek(Bracket)
    }
}

macro_rules! test_unquoted {
    ($($name:ident => $constructor: expr => Node::$getter:ident($bind:ident) => $check:expr ;)* )  => {
        $(
            mod $name {
                use super::*;
                #[test]
                fn test_unquoted_text_mixed() -> Result<()> {
                    let tokens = TokenStream::from_str(
                        concat!("<foo> bar bar ", $constructor, " baz baz </foo>")
                    ).unwrap();
                    let nodes = Parser::new(ParserConfig::default().custom_node::<TestCustomNode>()).parse_simple(tokens)?;
                    let Node::RawText(child1) = get_element_child(&*nodes, 0, 0) else { panic!("expected unquoted child") };

                    let Node::$getter($bind) = get_element_child(&*nodes, 0, 1) else { panic!("expected matcher child") };

                    let Node::RawText(child3) = get_element_child(&*nodes, 0, 2) else { panic!("expected unquoted child") };

                    // source text should be available
                    assert_eq!(child1.to_source_text(true).unwrap(), " bar bar ");
                    assert_eq!(child1.to_source_text(false).unwrap(), "bar bar");
                    assert_eq!(child1.to_token_stream_string(), "bar bar");

                    // When source is available, best should
                    assert_eq!(child1.to_string_best(), child1.to_source_text(true).unwrap());

                    assert_eq!(child3.to_source_text(true).unwrap(), " baz baz ");
                    assert_eq!(child3.to_source_text(false).unwrap(), "baz baz");
                    assert_eq!(child3.to_token_stream_string(), "baz baz");
                    assert_eq!(child3.to_string_best(), child3.to_source_text(true).unwrap());
                    $check;
                    Ok(())
                }
            }
        )*
    }
}

// unified way to check that unquoted text will work within mixed content,
// For example unquoted text before quoted, before element, fragment, or block
test_unquoted!(
    text => "\"text\"" => Node::Text(v) => assert_eq!(v.value_string(), "text");

    empty_element => "<div/>" => Node::Element(v) => {
        assert!(v.close_tag.is_none());
        assert_eq!(v.open_tag.name.to_string(), "div");

        assert!(v.attributes().is_empty() );
        assert!(v.children.is_empty() );
    };
    block => "{ x + 1 }" => Node::Block(v) => {
        // check that block valid
        assert!(v.try_block().is_some());
    };

    few_elements => "<div> <basd/> </div>" => Node::Element(v) => {
        assert!(v.close_tag.is_some());
        assert_eq!(v.open_tag.name.to_string(), "div");

        assert!(v.attributes().is_empty() );
        assert!(v.children.len() == 1 );
        let Node::Element(child) = v.children[0].clone() else {
            panic!("Not a element")
        };
        assert!(child.attributes().is_empty() );
        assert!(child.children.is_empty() );
        assert_eq!(child.open_tag.name.to_string(), "basd");
        assert!(child.close_tag.is_none());
    };

    custom_node => "[bracketed text]" => Node::Custom(v) => {
        assert_eq!(v.data.to_string(), "bracketed text");
    };
);

#[test]
#[should_panic = "Parsing error: LexError"]
fn test_unqouted_unfinished_quote_failing() {
    // Quote should be finished.
    let _ = TokenStream::from_str(
        r#"
        <foo> bar\"  baz </foo>
        "#,
    )
    .expect("Parsing error");
}

#[test]
#[should_panic = "Parsing error: LexError"]
fn test_unqouted_unfinished_brace_failing() {
    // Brace should be finished.
    let _ = TokenStream::from_str(
        r#"
        <foo> bar{  baz </foo>
        "#,
    )
    .expect("Parsing error");
}

#[test]
fn test_reserved_keyword_attributes() -> Result<()> {
    let tokens = quote! {
        <input type="foo" />
    };
    let nodes = parse2(tokens)?;
    let element = get_element(&nodes, 0);
    let Some(NodeAttribute::Attribute(attribute)) = element.attributes().get(0) else {
        panic!("expected attribute")
    };

    assert_eq!(element.name().to_string(), "input");
    assert_eq!(attribute.key.to_string(), "type");

    Ok(())
}

#[test]
fn test_block_node() -> Result<()> {
    let tokens = quote! {
        <div>{hello}</div>
    };
    let nodes = parse2(tokens)?;
    let element = get_element(&nodes, 0);

    assert_eq!(element.children.len(), 1);

    Ok(())
}

#[test]
fn test_flat_tree() -> Result<()> {
    let config = ParserConfig::new().flat_tree();

    let tokens = quote! {
        <div>
            <div>
                <div>{hello}</div>
                <div>"world"</div>
            </div>
        </div>
        <div />
    };
    let nodes = Parser::new(config).parse_simple(tokens)?;

    assert_eq!(nodes.len(), 7);

    Ok(())
}

#[test]
fn test_path_as_tag_name() -> Result<()> {
    let tokens = quote! {
        <some::path />
    };

    let nodes = parse2(tokens)?;
    let element = get_element(&nodes, 0);

    assert_eq!(element.name().to_string(), "some::path");

    Ok(())
}

#[test]
fn test_block_as_tag_name() -> Result<()> {
    let tokens = quote! {
        <{some_logic(block)} />
    };

    let nodes = parse2(tokens)?;
    let element = get_element(&nodes, 0);

    assert!(Block::try_from(element.name()).is_ok());

    Ok(())
}

// TODO: Is it really needed?
#[test]
fn test_block_as_tag_name_with_closing_tag() -> Result<()> {
    let tokens = quote! {
        <{some_logic(block)}>"Test"</{some_logic(block)}>
    };

    let nodes = parse2(tokens)?;
    let element = get_element(&nodes, 0);

    assert!(Block::try_from(element.name()).is_ok());

    Ok(())
}

#[test]
fn test_dashed_attribute_name() -> Result<()> {
    let tokens = quote! {
        <div data-foo="bar" />
    };

    let nodes = parse2(tokens)?;
    let attribute = get_element_attribute(&nodes, 0, 0);

    assert_eq!(attribute.key.to_string(), "data-foo");

    Ok(())
}

#[test]
#[should_panic = "Name must start with latin character"]
fn test_dashed_attribute_name_integers_not_supported_at_beginning() {
    let tokens = quote! {
        <div 12-foo="bar" />
    };

    let _ = parse2(tokens).unwrap();
}

#[test]
fn test_dashed_attribute_name_with_long_integer_suffixes() -> Result<()> {
    let tokens = quote! {
        <div data-14-32px-32mzxksq="bar" />
    };

    let nodes = parse2(tokens)?;
    let attribute = get_element_attribute(&nodes, 0, 0);

    assert_eq!(attribute.key.to_string(), "data-14-32px-32mzxksq");

    Ok(())
}

#[test]
fn test_coloned_attribute_name() -> Result<()> {
    let tokens = quote! {
        <div on:click={foo} />
    };

    let nodes = parse2(tokens)?;
    let attribute = get_element_attribute(&nodes, 0, 0);

    assert_eq!(attribute.key.to_string(), "on:click");

    Ok(())
}

#[test]
fn test_fn_bind_in_attribute() -> Result<()> {
    let tokens = quote! {
        <div bind:var(x)/>
    };

    let nodes = parse2(tokens)?;
    let attribute = get_element_attribute(&nodes, 0, 0);

    assert_eq!(attribute.key.to_string(), "bind:var");

    let binding = match &attribute.possible_value {
        KeyedAttributeValue::Binding(b) => b,
        _ => unreachable!(),
    };
    assert_eq!(binding.inputs.len(), 1);
    match binding.inputs.first().unwrap() {
        syn::Pat::Ident(x) => assert_eq!(x.ident.to_string(), "x"),
        _ => unreachable!(),
    }

    Ok(())
}

#[test]
fn test_fn_bind_with_type() -> Result<()> {
    let tokens = quote! {
        <div bind:var(x:Y)/>
    };

    let nodes = parse2(tokens)?;
    let attribute = get_element_attribute(&nodes, 0, 0);

    assert_eq!(attribute.key.to_string(), "bind:var");

    let binding = match &attribute.possible_value {
        KeyedAttributeValue::Binding(b) => b,
        _ => unreachable!(),
    };
    assert_eq!(binding.inputs.len(), 1);
    match binding.inputs.first().unwrap() {
        syn::Pat::Type(x) => {
            assert!(matches!(&*x.pat, syn::Pat::Ident(x) if x.ident == "x"));
            match &*x.ty {
                syn::Type::Path(p) => {
                    assert_eq!(p.path.segments.first().unwrap().ident.to_string(), "Y")
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }

    Ok(())
}

#[test]
fn test_fn_bind_mixed_types() -> Result<()> {
    let tokens = quote! {
        <div bind:var(x:Y, z)/>
    };

    let nodes = parse2(tokens)?;
    let attribute = get_element_attribute(&nodes, 0, 0);

    assert_eq!(attribute.key.to_string(), "bind:var");

    let binding = match &attribute.possible_value {
        KeyedAttributeValue::Binding(b) => b,
        _ => unreachable!(),
    };

    assert_eq!(binding.to_token_stream().to_string(), "(x : Y , z)");

    Ok(())
}

#[test]
fn test_mixed_colon_and_dash_attribute_name() -> Result<()> {
    let tokens = quote! {
        <div on:ce-click={foo} />
    };

    let nodes = parse2(tokens)?;
    let attribute = get_element_attribute(&nodes, 0, 0);

    assert_eq!(attribute.key.to_string(), "on:ce-click");

    Ok(())
}

#[test]
fn test_block_as_attribute() -> Result<()> {
    let tokens = quote! {
        <div {attribute} />
    };

    let nodes = parse2(tokens)?;
    let element = get_element(&nodes, 0);

    assert_eq!(element.attributes().len(), 1);

    Ok(())
}

#[test]
fn test_number_of_top_level_nodes() -> Result<()> {
    let tokens = quote! {
        <div />
        <div />
        <div />
    };

    let nodes = Parser::new(ParserConfig::new().number_of_top_level_nodes(2)).parse_simple(tokens);
    assert!(nodes.is_err());

    let tokens = quote! {
        <div>
            <div />
        </div>
        <div />
    };
    let nodes = Parser::new(ParserConfig::new().number_of_top_level_nodes(2).flat_tree())
        .parse_simple(tokens);
    assert!(nodes.is_ok());

    let tokens = quote! {
        <div />
    };
    let nodes = Parser::new(ParserConfig::new().number_of_top_level_nodes(2)).parse_simple(tokens);
    assert!(nodes.is_err());

    Ok(())
}

#[test]
fn test_type_of_top_level_nodes() -> Result<()> {
    let tokens = quote! {
        "foo"
    };
    let nodes = Parser::new(ParserConfig::new().type_of_top_level_nodes(NodeType::Element))
        .parse_simple(tokens);

    assert!(nodes.is_err());

    Ok(())
}

#[test]
fn test_transform_block_some() -> Result<()> {
    use syn::{Expr, Lit, Stmt, Token};

    let tokens = quote! {
        <div>{%}</div>
    };

    let config = ParserConfig::new().transform_block(|input| {
        input.parse::<Token![%]>()?;
        Ok(Some(quote! { "percent" }))
    });

    let nodes = Parser::new(config).parse_simple(tokens)?;
    let Node::Block(block) = get_element_child(&nodes, 0, 0) else {
        panic!("expected block")
    };

    assert_eq!(
        match block.try_block().as_ref() {
            Some(block) => {
                match &block.stmts[0] {
                    Stmt::Expr(Expr::Lit(expr), None) => match &expr.lit {
                        Lit::Str(lit_str) => Some(lit_str.value()),
                        _ => None,
                    },
                    _ => None,
                }
            }
            _ => None,
        },
        Some("percent".to_owned())
    );

    Ok(())
}

#[test]
fn test_transform_block_none() -> Result<()> {
    let tokens = quote! {
        <div>{"foo"}</div>
    };

    let config = ParserConfig::new().transform_block(|_| Ok(None));
    let nodes = Parser::new(config).parse_simple(tokens);

    assert!(nodes.is_ok());

    Ok(())
}

#[test]
fn test_doctype() -> Result<()> {
    let tokens = quote! {
        <!DOCTYPE html>
        <html>
        </html>
    };

    let nodes = parse2(tokens)?;
    let Some(Node::Doctype(doctype)) = nodes.get(0) else {
        panic!("expected doctype")
    };

    assert_eq!(doctype.value.to_token_stream_string(), "html");

    Ok(())
}

#[test]
fn test_doctype_empty() -> Result<()> {
    let tokens = quote! {
        <!DOCTYPE>
        <html>
        </html>
    };

    let nodes = parse2(tokens)?;
    let Some(Node::Doctype(doctype)) = nodes.get(0) else {
        panic!("expected doctype")
    };

    assert_eq!(doctype.value.to_token_stream_string(), "");

    Ok(())
}

#[test]
fn test_comment() -> Result<()> {
    let tokens = quote! {
        <!-- "comment1" -->
        <div>
            <!-- "comment2" -->
            <div />
        </div>
    };

    let nodes = parse2(tokens)?;
    let Some(Node::Comment(comment1)) = nodes.get(0) else {
        panic!("expected comment")
    };
    let Node::Comment(comment2) = get_element_child(&nodes, 1, 0) else {
        panic!("expected comment")
    };

    assert_eq!(comment1.value.value(), "comment1");
    assert_eq!(comment2.value.value(), "comment2");

    Ok(())
}

#[test]
fn test_fragment() -> Result<()> {
    let tokens = quote! {
        <>
            <div />
        </>
    };

    let nodes = parse2(tokens)?;
    let Some(Node::Fragment(fragment)) = nodes.get(0) else {
        panic!("expected fragment")
    };

    assert_eq!(fragment.children.len(), 1);

    Ok(())
}

#[test]
fn test_generics() -> Result<()> {
    let tokens = quote! {
        <foo<Bar>/>
    };
    let nodes = parse2(tokens)?;
    let element = get_element(&nodes, 0);

    assert_eq!(element.name().to_string(), "foo");
    let param: TypeParam = parse_quote!(Bar);
    assert_eq!(element.open_tag.generics.type_params().next(), Some(&param));

    Ok(())
}

#[test]
fn test_generics_lifetime() -> Result<()> {
    let tokens = quote! {
        <foo<'bar: 'a + 'b>/>
    };
    let nodes = parse2(tokens)?;
    let element = get_element(&nodes, 0);

    assert_eq!(element.name().to_string(), "foo");
    let param: LifetimeParam = parse_quote!('bar: 'a + 'b);
    assert_eq!(element.open_tag.generics.lifetimes().next(), Some(&param));

    Ok(())
}

#[test]
fn test_generics_closed() -> Result<()> {
    let tokens = quote! {
        <foo<'a, Bar>> </foo<'a, Bar>>
    };
    let nodes = parse2(tokens)?;
    let element = get_element(&nodes, 0);

    assert_eq!(element.name().to_string(), "foo");
    let param: TypeParam = parse_quote!(Bar);
    let lf_param: LifetimeParam = parse_quote!('a);
    assert_eq!(
        element
            .close_tag
            .as_ref()
            .unwrap()
            .generics
            .type_params()
            .next(),
        Some(&param)
    );
    assert_eq!(
        element
            .close_tag
            .as_ref()
            .unwrap()
            .generics
            .lifetimes()
            .next(),
        Some(&lf_param)
    );

    Ok(())
}

#[test]
fn test_generics_closed_not_match() -> Result<()> {
    let tokens = quote! {
        <foo<Bar>> </foo<Baz>>
    };
    let e = parse2(tokens).unwrap_err();
    assert_eq!(e.to_string(), "close tag generics missmatch");
    Ok(())
}

#[test]
fn test_attribute_fn_binding() -> Result<()> {
    let tokens = quote! {
        <tag attribute(x:u32, // bind variable
                Foo{y: Bar}:Foo, // destructing
                baz // variable without type (like in closure)
            ) />
    };

    let nodes = parse2(tokens)?;

    assert_eq!(nodes.len(), 1);
    let Node::Element(e) = &nodes[0] else {
        unreachable!()
    };
    assert_eq!(e.attributes().len(), 1);
    let NodeAttribute::Attribute(a) = &e.open_tag.attributes[0] else {
        unreachable!()
    };
    assert_eq!(a.key.to_string(), "attribute");
    let KeyedAttributeValue::Binding(b) = &a.possible_value else {
        unreachable!()
    };
    assert_eq!(b.inputs.len(), 3);
    let pat = &b.inputs[0];

    let expected: PatType = PatType {
        pat: parse_quote!(x),
        colon_token: Colon::default(),
        ty: parse_quote!(u32),
        attrs: vec![],
    };
    assert_eq!(pat, &Pat::Type(expected));

    let pat = &b.inputs[1];

    let expected: PatType = PatType {
        pat: parse_quote!(Foo { y: Bar }),
        colon_token: Colon::default(),
        ty: parse_quote!(Foo),
        attrs: vec![],
    };
    assert_eq!(pat, &Pat::Type(expected));

    let pat = &b.inputs[2];
    assert_eq!(pat, &parse_quote!(baz));
    Ok(())
}

#[test]
// during parsing ="foo"
#[should_panic = "invalid tag name or attribute key"]
fn test_attribute_fn_binding_with_value_is_not_expected() {
    let tokens = quote! {
        <tag attribute(x:u32)="foo" />
    };

    parse2(tokens).unwrap();
}

#[test]
fn test_reserved_keywords() -> Result<()> {
    let tokens = quote! {
        <tag::type attribute::type />
        <tag:type attribute:type />
        <tag-type attribute-type />
    };

    let nodes = parse2(tokens)?;

    assert_eq!(nodes.len(), 3);

    Ok(())
}

#[test]
fn test_name_getter() -> Result<()> {
    let tokens = quote! {
        <{"block_element_name"} />
        <dast-seperated />
        <_ /> // wildcard
    };

    let nodes = parse2(tokens)?;

    assert_eq!(nodes.len(), 3);
    assert!(get_element(&nodes, 0).open_tag.name.is_block());
    assert!(get_element(&nodes, 1).open_tag.name.is_dashed());
    assert!(get_element(&nodes, 2).open_tag.name.is_wildcard());

    Ok(())
}

#[test]
#[should_panic = "wrong close tag found"]
fn test_default_wildcard_failed_to_parse_block() {
    let tokens = quote! {
        <Foo> </ _>
    };

    let config = ParserConfig::new().element_close_use_default_wildcard_ident(true);
    let _ = Parser::new(config).parse_simple(tokens).unwrap();
}

#[test]
fn test_default_wildcard() -> Result<()> {
    let tokens = quote! {
        <{"block_element_name"}> </ _>
    };

    let config = ParserConfig::new().element_close_use_default_wildcard_ident(true);
    let nodes = Parser::new(config).parse_simple(tokens).unwrap();
    assert!(get_element(&nodes, 0)
        .close_tag
        .as_ref()
        .unwrap()
        .name
        .is_wildcard());
    Ok(())
}

#[test]
fn test_default_wildcard_for_regular_element() -> Result<()> {
    let tokens = quote! {
        <Foo> </ _>
    };

    let config = ParserConfig::new().element_close_use_default_wildcard_ident(false);
    let nodes = Parser::new(config).parse_simple(tokens).unwrap();
    assert!(get_element(&nodes, 0)
        .close_tag
        .as_ref()
        .unwrap()
        .name
        .is_wildcard());
    Ok(())
}

#[test]
fn test_custom_wildcard() -> Result<()> {
    let tokens = quote! {
        <Foo> </ WILDCARD>
    };

    let config = ParserConfig::new()
        .element_close_wildcard(|_, close_tag| close_tag.name.to_string() == "WILDCARD");
    let nodes = Parser::new(config).parse_simple(tokens).unwrap();
    assert!(!get_element(&nodes, 0)
        .close_tag
        .as_ref()
        .unwrap()
        .name
        .is_wildcard());
    Ok(())
}
#[test]
fn test_single_element_with_different_attributes() -> Result<()> {
    let tokens = quote! {
        <foo bar="moo" baz=0x10 bax=true bay=0.1 foz='c' foy={x} fo1=b'c'></foo>
    };
    let nodes = parse2(tokens)?;

    let valid_values = vec![
        ("bar", "moo"),
        ("baz", "16"),
        ("bax", "true"),
        ("bay", "0.1"),
        ("foz", "c"),
    ];
    let valid_values_len = valid_values.len();
    for (ix, (name, value)) in valid_values.into_iter().enumerate() {
        let attribute = get_element_attribute(&nodes, 0, ix);

        assert_eq!(attribute.key.to_string(), name);
        assert_eq!(attribute.value_literal_string().expect("value"), value);
    }
    let values = vec!["foy", "fo1"];
    for (ix, name) in values.into_iter().enumerate() {
        let attribute = get_element_attribute(&nodes, 0, valid_values_len + ix);

        assert_eq!(attribute.key.to_string(), name);
        assert!(attribute.value_literal_string().is_none());
    }

    Ok(())
}

#[test]
fn test_invalid_blocks() -> Result<()> {
    // test that invalid blocks can be parsed in recoverable mode
    // usefull for IDEs
    let tokens = quote! {
        <foo>{block.} </foo>
    };

    let config = ParserConfig::new().recover_block(true);
    let (nodes, diagnostics) = Parser::new(config)
        .parse_recoverable(tokens.clone())
        .split_vec();

    let Node::Block(block) = get_element_child(&nodes, 0, 0) else {
        panic!("expected block")
    };

    assert_eq!(block.to_token_stream().to_string(), "{ block . }");
    assert_eq!(diagnostics.len(), 1);
    let dbg_diag = format!("{:?}", diagnostics[0]);
    assert!(dbg_diag.contains("unexpected end of input, expected identifier or integer"));
    // same should not work if recover_block = false
    let config = ParserConfig::new();
    let (nodes, diagnostics) = Parser::new(config).parse_recoverable(tokens).split_vec();
    let node = get_element(&nodes, 0);
    assert!(node.children.is_empty());
    // TODO: Cleanup errors
    assert!(diagnostics.len() > 1);
    Ok(())
}

#[test]
fn test_invalid_blocks_in_attr() -> Result<()> {
    // test that invalid blocks can be parsed in recoverable mode
    // usefull for IDEs
    let tokens = quote! {
        <foo foo={block.}> </foo>
    };

    let config = ParserConfig::new().recover_block(true);
    let (nodes, diagnostics) = Parser::new(config)
        .parse_recoverable(tokens.clone())
        .split_vec();

    let attr = get_element_attribute(&nodes, 0, 0);
    let KeyedAttributeValue::Value(eq_val) = &attr.possible_value else {
        panic!("expected value")
    };

    let KVAttributeValue::InvalidBraced(block) = &eq_val.value else {
        panic!("expected invalid block")
    };

    assert_eq!(block.to_token_stream().to_string(), "{ block . }");

    assert_eq!(diagnostics.len(), 1);
    let dbg_diag = format!("{:?}", diagnostics[0]);
    assert!(dbg_diag.contains("unexpected end of input, expected identifier or integer"));
    // same should not work if recover_block = false
    let config = ParserConfig::new();
    let (nodes, diagnostics) = Parser::new(config).parse_recoverable(tokens).split_vec();
    let node = get_element(&nodes, 0);
    assert!(node.attributes().is_empty());
    assert_eq!(diagnostics.len(), 1);
    Ok(())
}

#[test]
fn test_empty_input() -> Result<()> {
    let tokens = quote! {};
    let nodes = parse2(tokens)?;

    assert!(nodes.is_empty());

    Ok(())
}

#[test]
fn test_consecutive_puncts_in_name() {
    let name: NodeName = parse_quote! { a--::..d };
    assert_eq!(name.to_string(), "a--::..d");
}

fn get_element<C: CustomNode>(nodes: &[Node<C>], element_index: usize) -> &NodeElement<C> {
    let Some(Node::Element(element)) = nodes.get(element_index) else {
        panic!("expected element")
    };
    element
}

fn get_element_attribute<C: CustomNode>(
    nodes: &[Node<C>],
    element_index: usize,
    attribute_index: usize,
) -> &KeyedAttribute {
    let Some(Node::Element(element)) = nodes.get(element_index) else {
        panic!("expected element")
    };
    let Some(NodeAttribute::Attribute(attribute)) = element.attributes().get(attribute_index)
    else {
        panic!("expected attribute")
    };

    attribute
}

fn get_element_child<C: CustomNode>(
    nodes: &[Node<C>],
    element_index: usize,
    child_index: usize,
) -> &Node<C> {
    let Some(Node::Element(element)) = nodes.get(element_index) else {
        panic!("expected element")
    };
    element.children.get(child_index).expect("child")
}
