//! RSX Parser

use std::vec;

use proc_macro2::TokenStream;
use proc_macro2_diagnostics::Diagnostic;
use syn::{parse::ParseStream, spanned::Spanned, Result};

pub mod recoverable;

use self::recoverable::{ParseRecoverable, ParsingResult, RecoverableContext};
use crate::{node::*, ParserConfig};

///
/// Primary library interface to RSX Parser
///
/// Allows customization through `ParserConfig`.
/// Support recovery after parsing invalid token.

pub struct Parser {
    config: ParserConfig,
}

impl Parser {
    /// Create a new parser with the given [`ParserConfig`].
    pub fn new(config: ParserConfig) -> Parser {
        Parser { config }
    }

    /// Parse the given [`proc-macro2::TokenStream`] or
    /// [`proc-macro::TokenStream`] into a [`Node`] tree.
    ///
    /// [`proc-macro2::TokenStream`]: https://docs.rs/proc-macro2/latest/proc_macro2/struct.TokenStream.html
    /// [`proc-macro::TokenStream`]: https://doc.rust-lang.org/proc_macro/struct.TokenStream.html
    /// [`Node`]: struct.Node.html
    pub fn parse_simple(&self, v: impl Into<TokenStream>) -> Result<Vec<Node>> {
        self.parse_recoverable(v).into_result()
    }

    /// Advance version of `parse_simple` that returns array of errors in case
    /// of partial parsing.
    pub fn parse_recoverable(&self, v: impl Into<TokenStream>) -> ParsingResult<Vec<Node>> {
        use syn::parse::Parser as _;
        let parser = move |input: ParseStream| Ok(self.parse_syn_stream(input));
        let res = parser.parse2(v.into());
        res.expect("No errors from parser")
    }

    /// Parse a given [`ParseStream`].
    pub fn parse_syn_stream(&self, input: ParseStream) -> ParsingResult<Vec<Node>> {
        let mut nodes = vec![];
        let mut top_level_nodes = 0;

        let mut parser = RecoverableContext::new(self.config.clone().into());
        while !input.cursor().eof() {
            let Some(parsed_node) = Node::parse_recoverable(&mut parser, input) else {
                parser.push_diagnostic(input.error("Node parse failed".to_string()));
                break;
            };

            if let Some(type_of_top_level_nodes) = &self.config.type_of_top_level_nodes {
                if &parsed_node.r#type() != type_of_top_level_nodes {
                    parser.push_diagnostic(input.error(format!(
                        "top level nodes need to be of type {}",
                        type_of_top_level_nodes
                    )));
                    break;
                }
            }

            top_level_nodes += 1;
            nodes.push(parsed_node)
        }

        // its important to skip tokens, to avoid Unexpected tokens errors.
        if !input.is_empty() {
            let tts = input
                .parse::<TokenStream>()
                .expect("No error in parsing token stream");
            parser.push_diagnostic(Diagnostic::spanned(
                tts.span(),
                proc_macro2_diagnostics::Level::Error,
                "Tokens was skipped after incorrect parsing",
            ));
        }

        if let Some(number_of_top_level_nodes) = &self.config.number_of_top_level_nodes {
            if &top_level_nodes != number_of_top_level_nodes {
                parser.push_diagnostic(input.error(format!(
                    "saw {} top level nodes but exactly {} are required",
                    top_level_nodes, number_of_top_level_nodes
                )))
            }
        }

        let nodes = if self.config.flat_tree {
            nodes.into_iter().flat_map(Node::flatten).collect()
        } else {
            nodes
        };

        let errors = parser.diagnostics;

        let nodes = if nodes.is_empty() { None } else { Some(nodes) };
        ParsingResult::from_parts(nodes, errors)
    }
}
