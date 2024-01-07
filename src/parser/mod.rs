//! RSX Parser

use std::vec;

use proc_macro2::TokenStream;
use proc_macro2_diagnostics::Diagnostic;
use syn::{parse::ParseStream, spanned::Spanned, Result};

pub mod recoverable;

#[cfg(feature = "rawtext-stable-hack")]
use {proc_macro2::Span, std::str::FromStr};

use self::recoverable::{ParseRecoverable, ParsingResult, RecoverableContext};
#[cfg(feature = "rawtext-stable-hack")]
use crate::rawtext_stable_hack;
use crate::{node::*, ParserConfig};
///
/// Primary library interface to RSX Parser
///
/// Allows customization through `ParserConfig`.
/// Support recovery after parsing invalid token.

pub struct Parser<C> {
    config: ParserConfig<C>,
}

impl<C: CustomNode> Parser<C> {
    /// Create a new parser with the given [`ParserConfig`].
    pub fn new(config: ParserConfig<C>) -> Self {
        Parser { config }
    }

    /// Parse the given [`proc-macro2::TokenStream`] or
    /// [`proc-macro::TokenStream`] into a [`Node`] tree.
    ///
    /// [`proc-macro2::TokenStream`]: https://docs.rs/proc-macro2/latest/proc_macro2/struct.TokenStream.html
    /// [`proc-macro::TokenStream`]: https://doc.rust-lang.org/proc_macro/struct.TokenStream.html
    /// [`Node`]: struct.Node.html
    pub fn parse_simple(&self, v: impl Into<TokenStream>) -> Result<Vec<Node<C>>> {
        self.parse_recoverable(v).into_result()
    }

    /// Advance version of `parse_simple` that returns array of errors in case
    /// of partial parsing.
    pub fn parse_recoverable(&self, v: impl Into<TokenStream>) -> ParsingResult<Vec<Node<C>>> {
        use syn::parse::Parser as _;

        let parser = move |input: ParseStream| Ok(self.parse_syn_stream(input));
        let source = parser.parse2(v.into()).expect("No errors from parser");

        #[cfg(not(feature = "rawtext-stable-hack"))]
        {
            return source;
        }
        // re-parse using proc_macro2::fallback, only if output without error
        #[cfg(feature = "rawtext-stable-hack")]
        Self::reparse_raw_text(&self, parser, source)
    }
    #[cfg(feature = "rawtext-stable-hack")]
    fn reparse_raw_text<Parser>(
        &self,
        parser: Parser,
        mut source: ParsingResult<Vec<Node<C>>>,
    ) -> ParsingResult<Vec<Node<C>>>
    where
        Parser: FnOnce(ParseStream) -> syn::Result<ParsingResult<Vec<Node<C>>>>,
    {
        use syn::parse::Parser as _;
        // in case we already have valid raw_text, we can skip re-parsing
        if rawtext_stable_hack::is_join_span_available() {
            return source;
        }
        // Source is err, so we need to use fallback.
        if !source.is_ok() {
            let (mut source, errors) = source.split_vec();
            rawtext_stable_hack::inject_raw_text_default(&mut source);
            return ParsingResult::from_parts_vec(source, errors);
        }
        // return error, if macro source_text is not available.
        if !rawtext_stable_hack::is_macro_args_recoverable() {
            source.push_diagnostic(Diagnostic::new(
                    proc_macro2_diagnostics::Level::Warning,
                    "Failed to retrive source text of macro call, maybe macro was called from other macro?",
                ));
            return source;
        }
        // Feature is additive, this mean that top-level crate can activate
        // "rawtext-stable-hack", but other crates will not use macro_pattern
        if self.config.macro_pattern.is_empty() {
            return source;
        }
        let text = Span::call_site()
            .source_text()
            .expect("Source text should be available");

        proc_macro2::fallback::force();
        let stream = TokenStream::from_str(&text).unwrap();
        let stream = self
            .config
            .macro_pattern
            .match_content(stream)
            .expect("Cannot find macro pattern inside Span::call_site");
        let hacked = parser.parse2(stream).expect("No errors from parser");

        let mut source = source.into_result().expect("was checked");
        let hacked = hacked.into_result().expect("was checked");
        proc_macro2::fallback::unforce();
        rawtext_stable_hack::inject_raw_text(&mut source, &hacked);

        return ParsingResult::Ok(source);
    }

    /// Parse a given [`ParseStream`].
    pub fn parse_syn_stream(&self, input: ParseStream) -> ParsingResult<Vec<Node<C>>> {
        let mut nodes = vec![];
        let mut top_level_nodes = 0;

        let mut parser = RecoverableContext::new(self.config.clone().into());
        while !input.is_empty() {
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

        let nodes = if nodes.is_empty() {
            Some(vec![])
        } else {
            Some(nodes)
        };
        ParsingResult::from_parts(nodes, errors)
    }
}
