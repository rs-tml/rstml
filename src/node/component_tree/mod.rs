use derive_more::Display;
use termtree::Tree;

pub mod node_block;
pub mod node_comment;
pub mod node_doctype;
pub mod node_element;
pub mod node_fragment;
pub mod node_rawtext;
pub mod node_text;

use super::Node;

#[derive(Display)]
pub enum Components {
    #[display(fmt = "{}", "_0")]
    SelfTitle(String),
    #[display(fmt = "vec: {}", "_0")]
    Children(String),
}

/// This may help in debugging, if one needs to verify how a complex token
/// stream got parsed
///
///    ```ignore
///    let tokens = quote! {
///        bar  baz
///    };
///    
///    let nodes = parse2(tokens)?;
///    for node in nodes {
///        println!("{}", Into::<Tree<Components>>::into(node));
///    }
///    ```

impl Into<Tree<Components>> for Node {
    fn into(self) -> Tree<Components> {
        match self {
            Node::Comment(comment) => comment.into(),

            Node::Doctype(doctype) => doctype.into(),

            Node::RawText(raw_text) => raw_text.into(),

            Node::Fragment(fragment) => fragment.into(),

            Node::Block(block) => block.into(),
            Node::Text(text) => text.into(),
            Node::Element(element) => element.into(),
        }
    }
}
#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use eyre::Result;
    use proc_macro2::TokenStream;
    use quote::quote;
    use termtree::Tree;

    use crate::{node::component_tree::Components, parse2, Parser, ParserConfig};

    #[test]
    fn test_comment_into_components() -> Result<()> {
        let tokens = quote! {
            <!-- "comment1" -->
        };

        let nodes = parse2(tokens)?;
        for node in nodes {
            println!("{}", Into::<Tree<Components>>::into(node));
        }

        Ok(())
    }

    #[test]
    fn test_doctype_into_components() -> Result<()> {
        let tokens = quote! {
            <!DOCTYPE html>
        };

        let nodes = parse2(tokens)?;
        for node in nodes {
            println!("{}", Into::<Tree<Components>>::into(node));
        }

        Ok(())
    }

    #[test]
    fn test_raw_text_into_components() -> Result<()> {
        let tokens = quote! {
            bar  baz
        };

        let nodes = parse2(tokens)?;
        for node in nodes {
            println!("{}", Into::<Tree<Components>>::into(node));
        }
        Ok(())
    }

    #[test]
    fn test_fragment_into_components() -> Result<()> {
        let tokens = quote! {
            <>
                <!-- "comment 17" -->
            </>
        };

        let nodes = parse2(tokens)?;
        for node in nodes {
            println!("{}", Into::<Tree<Components>>::into(node));
        }

        Ok(())
    }

    #[test]
    fn test_invalid_block_node_into_components() -> Result<()> {
        let config = ParserConfig::new().recover_block(true);
        let parser = Parser::new(config);
        let tokens = quote! {
            {hello ./}
        };

        let (nodes, _errors) = parser.parse_recoverable(tokens).split_vec();
        for node in nodes {
            println!("{}", Into::<Tree<Components>>::into(node));
        }

        Ok(())
    }

    #[test]
    fn test_valid_block_node_into_components() -> Result<()> {
        let config = ParserConfig::new().recover_block(true);
        let parser = Parser::new(config);
        let tokens = quote! {
            {hello }
        };

        let (nodes, _errors) = parser.parse_recoverable(tokens).split_vec();
        for node in nodes {
            println!("{}", Into::<Tree<Components>>::into(node));
        }

        Ok(())
    }

    #[test]
    fn test_text_into_components() -> Result<()> {
        let config = ParserConfig::new().recover_block(true);
        let parser = Parser::new(config);
        let tokens = quote! {
            "Example text"
        };

        let (nodes, _errors) = parser.parse_recoverable(tokens).split_vec();
        for node in nodes {
            println!("{}", Into::<Tree<Components>>::into(node));
        }

        Ok(())
    }

    #[test]
    fn test_node_element_into_components() -> Result<()> {
        let config = ParserConfig::new().recover_block(true);
        let parser = Parser::new(config);
        let token_streams = {
            let tokens1 = TokenStream::from_str(
                "<foo {x.} />", // dot is not allowed
            )
            .unwrap();

            let tokens2 = quote! {
                <foo></foo>
            };
            let tokens3 = quote! {
                <foo bar="moo" baz="42"></foo>
            };
            let tokens4 = quote! {
                <foo>bar baz <div/> baz baz</foo>
            };
            let tokens5 = quote! {
                <input:bro type="foo" />
            };

            let tokens6 = quote! {
                <some::path />
            };

            let tokens7 = quote! {
                <{some_logic(block)} />
            };

            let tokens8 = quote! {
                <div data-foo="bar" />
            };

            let tokens9 = quote! {
                <div on:click={foo} />
            };

            let tokens10 = quote! {
                <div {attribute} bax=true />
            };
            vec![
                tokens1, tokens2, tokens3, tokens4, tokens5, tokens6, tokens7, tokens8, tokens9,
                tokens10,
            ]
        };

        for tokens in token_streams {
            let (nodes, _errors) = parser.parse_recoverable(tokens).split_vec();
            for node in nodes {
                println!();
                println!("{}", Into::<Tree<Components>>::into(node));
            }
        }

        Ok(())
    }
}
