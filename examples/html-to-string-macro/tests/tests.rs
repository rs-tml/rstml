use std::collections::HashSet;

use html_to_string_macro::html;
use quote::quote;
use rstml::{node::component_tree::Components, Parser, ParserConfig};
use termtree::Tree;

// Using this parser, one can write docs and link html tags to them.
// if this macro would be independent, it would be nicer to have docs in
// separate crate.
pub mod docs {
    /// Element has open and close tags, content and attributes.
    pub fn element() {}
}

#[test]
fn test_basic() {
    let nightly_unqoted = " Hello  world with spaces ";
    let stable_unqoted = "Hello world with spaces";
    let unquoted_text = if cfg!(feature = "nightly") {
        nightly_unqoted
    } else {
        stable_unqoted
    };
    let world = "planet";
    assert_eq!(
        html! {
            <!DOCTYPE html>
            <html>
                <head>
                    <title>"Example"</title>
                </head>
                <body>
                    <!-- "comment" -->
                    <div hello={world} />
                    <>
                        <div>"1"</div>
                        <div> Hello  world with spaces </div>
                        <div>"3"</div>
                        <div {"some-attribute-from-rust-block"}/>
                    </>
                </body>
            </html>
        },
        format!(
            r#"
            <!DOCTYPE html>
            <html>
                <head>
                    <title>Example</title>
                </head>
                <body>
                    <!-- comment -->
                    <div hello="planet"/>
                    <div>1</div>
                    <div>{}</div>
                    <div>3</div>
                    <div some-attribute-from-rust-block/>
                </body>
            </html>
        "#,
            unquoted_text
        )
        .split('\n')
        .map(|line| line.trim())
        .collect::<Vec<&str>>()
        .join("")
    );
}

#[test]
fn test_raw_text_involved() {
    let world = "planet";
    assert_eq!(
        html! {
                <head>
                    <title>raw text, first child node " Text : second child node "<third-element-child/> raw text, fourth child node {world}</title>
                    <script>one single " Monolithic "<included-in-raw-text/> {world} raw child</script>
                </head>
        },
            r#"
                <head>
                    <title>raw text, first child node Text : second child node <third-element-child/>raw text, fourth child nodeplanet</title>
                    <script>one single " Monolithic " < included - in - raw - text / > { world } raw child</script>
                </head>
        "#
        .split('\n')
        .map(|line| line.trim())
        .collect::<Vec<&str>>()
        .join("")
    );
}

#[test]
fn test_nodes_tree_into_components() {
    let tokens = quote! {
            <head>
                <title>raw text, first child node " Text : second child node "<third-element-child/> raw text, fourth child node {world}</title>
                <script>one single " Monolithic "<included-in-raw-text/> {world} raw child</script>
            </head>
    };

    let self_closed: HashSet<&'static str> = [
        "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param",
        "source", "track", "wbr",
    ]
    .iter()
    .cloned()
    .collect();
    let config = ParserConfig::new()
        .recover_block(true)
        // https://developer.mozilla.org/en-US/docs/Glossary/Empty_element
        .always_self_closed_elements(self_closed)
        .raw_text_elements(["script", "style"].iter().cloned().collect());

    let parser = Parser::new(config);
    let (nodes, errors) = parser.parse_recoverable(tokens).split_vec();
    for node in nodes {
        println!("{}", Into::<Tree<Components>>::into(node));
    }

    assert!(errors.is_empty());
}
