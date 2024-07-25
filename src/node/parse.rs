//!
//! Implementation of ToTokens and Spanned for node related structs

use proc_macro2::{extra::DelimSpan, Delimiter, TokenStream};
use proc_macro2_diagnostics::{Diagnostic, Level};
use quote::ToTokens;
use syn::{
    braced,
    parse::{discouraged::Speculative, Parse, ParseStream, Parser},
    spanned::Spanned,
    token::Brace,
    Block, Ident, LitStr, Token,
};

use super::{
    atoms::{
        tokens::{self, DocStart},
        CloseTag, FragmentClose, FragmentOpen, OpenTag,
    },
    raw_text::RawText,
    CustomNode, Node, NodeBlock, NodeDoctype, NodeFragment,
};
use crate::{
    atoms::CloseTagStart,
    config::TransformBlockFn,
    node::{NodeAttribute, NodeElement},
    parser::recoverable::{ParseRecoverable, RecoverableContext},
};

impl ParseRecoverable for NodeBlock {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        let fork = input.fork();

        let block = match parse_valid_block_expr(parser, &fork) {
            Ok(value) => {
                input.advance_to(&fork);
                NodeBlock::ValidBlock(value)
            }
            Err(e) if parser.config().recover_block => {
                parser.push_diagnostic(e);
                NodeBlock::Invalid(parser.parse_simple(input)?)
            }
            Err(e) => {
                parser.push_diagnostic(e);
                return None;
            }
        };
        Some(block)
    }
}

impl<C: CustomNode> ParseRecoverable for NodeFragment<C> {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        let tag_open: FragmentOpen = parser.parse_simple(input)?;

        let is_raw = |name| parser.config().raw_text_elements.contains(name);

        let (children, tag_close) = if is_raw("") {
            let (child, closed_tag) =
                parser.parse_with_ending(input, |_, t| RawText::from(t), FragmentClose::parse);

            (vec![Node::<C>::RawText(child)], closed_tag)
        } else {
            let (child, close_tag_start) =
                parser.parse_tokens_until_call::<Node<C>, _, _>(input, CloseTagStart::parse);
            (
                child,
                FragmentClose::parse_with_start_tag(parser, input, close_tag_start),
            )
        };
        let open_tag_end = tag_open.token_gt.span();
        let close_tag_start = tag_close.as_ref().map(|v| v.start_tag.token_lt.span());

        let children = RawText::vec_set_context(open_tag_end, close_tag_start, children);

        Some(NodeFragment {
            tag_open,
            children,
            tag_close,
        })
    }
}

impl ParseRecoverable for NodeDoctype {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        let token_start = parser.parse_simple::<DocStart>(input)?;
        let doctype_keyword = parser.parse_simple::<Ident>(input)?;
        if doctype_keyword.to_string().to_lowercase() != "doctype" {
            parser.push_diagnostic(Diagnostic::spanned(
                doctype_keyword.span(),
                Level::Error,
                "expected DOCTYPE keyword",
            ));
            return None;
        }
        let (value, token_end) =
            parser.parse_with_ending(input, |_, t| RawText::from(t), <Token![>]>::parse);

        let token_end = token_end?;
        Some(Self {
            token_start,
            token_doctype: doctype_keyword,
            value,
            token_end,
        })
    }
}

impl OpenTag {
    /// Parses the opening `<` of an open tag, providing handling of the
    /// unexpected `</` of a close tag.
    ///
    /// **Note:** This is an internal function exported to make parsing of
    /// custom nodes easier. It is not considered stable.
    pub fn parse_start_tag(
        parser: &mut RecoverableContext,
        input: ParseStream,
    ) -> Option<Token![<]> {
        let token_lt = parser.parse_simple::<Token![<]>(input)?;
        // Found closing tag when open tag was expected
        // keep parsing it as open tag.
        if input.peek(Token![/]) {
            let span = if let Ok(solidus) = input.parse::<Token![/]>() {
                solidus.span()
            } else {
                token_lt.span()
            };
            parser.push_diagnostic(Diagnostic::spanned(
                span,
                Level::Error,
                "close tag was parsed while waiting for open tag",
            ));
        }
        Some(token_lt)
    }
}

impl ParseRecoverable for OpenTag {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        let token_lt = Self::parse_start_tag(parser, input)?;
        let name = parser.parse_simple(input)?;
        let generics = parser.parse_simple(input)?;

        let (attributes, end_tag) = parser
            .parse_tokens_with_conflicted_ending::<NodeAttribute, _, _>(
                input,
                tokens::OpenTagEnd::parse,
            );

        if end_tag.is_none() {
            parser.push_diagnostic(Diagnostic::new(Level::Error, "expected end of tag '>'"));
        }
        end_tag.map(|end_tag| OpenTag {
            token_lt,
            name,
            generics,
            attributes,
            end_tag,
        })
    }
}

impl<C: CustomNode> NodeElement<C> {
    /// Parses the children of a node, stopping at the first matching closing
    /// tag, following the behavior specified in the [`ParserConfig`].
    ///
    /// **Note:** This is an internal function exported to make parsing of
    /// custom nodes easier. It is not considered stable.
    ///
    /// [`ParserConfig`]: crate::ParserConfig
    pub fn parse_children(
        parser: &mut RecoverableContext,
        input: ParseStream,
        raw: bool,
        open_tag: &OpenTag,
    ) -> Option<(Vec<Node<C>>, Option<CloseTag>)> {
        let (children, close_tag) = if raw {
            let (child, closed_tag) =
                parser.parse_with_ending(input, |_, t| RawText::from(t), CloseTag::parse);
            // don't keep empty RawText
            let children = if !child.is_empty() {
                vec![Node::RawText(child)]
            } else {
                vec![]
            };
            (children, closed_tag)
        } else {
            // If node is not raw use any closing tag as separator, to early report about
            // invalid closing tags.
            // Also parse only </ part to recover parser as soon as user types </
            let (children, close_tag) =
                parser.parse_tokens_until_call::<Node<C>, _, _>(input, CloseTagStart::parse);

            let close_tag = CloseTag::parse_with_start_tag(parser, input, close_tag);

            (children, close_tag)
        };

        let open_tag_end = open_tag.end_tag.token_gt.span();
        let close_tag_start = close_tag.as_ref().map(|c| c.start_tag.token_lt.span());
        let children = RawText::vec_set_context(open_tag_end, close_tag_start, children);

        let Some(close_tag) = close_tag else {
            let mut diagnostic = Diagnostic::spanned(
                open_tag.span(),
                Level::Error,
                "open tag has no corresponding close tag",
            );
            if !children.is_empty() {
                let mut note_span = TokenStream::new();
                children.iter().for_each(|v| v.to_tokens(&mut note_span));
                diagnostic = diagnostic.span_note(
                    note_span.span(),
                    "treating all inputs after open tag as it content",
                );
            }

            parser.push_diagnostic(diagnostic);
            return Some((children, None));
        };

        if close_tag.name != open_tag.name {
            match parser.config().element_close_wildcard.as_deref() {
                Some(is_wildcard) if is_wildcard(open_tag, &close_tag) => {}
                _ => {
                    let diagnostic = Diagnostic::spanned(
                        close_tag.span(),
                        Level::Error,
                        "wrong close tag found",
                    )
                    .spanned_child(
                        open_tag.span(),
                        Level::Help,
                        "open tag that should be closed; it's started here",
                    );

                    parser.push_diagnostic(diagnostic)
                }
            }
        }
        if close_tag.generics != open_tag.generics {
            let diagnostic = Diagnostic::spanned(
                close_tag.span(),
                Level::Error,
                "close tag generics missmatch",
            )
            .spanned_child(
                open_tag.span(),
                Level::Help,
                "open tag generics should match close tag generics",
            );
            parser.push_diagnostic(diagnostic)
        }
        Some((children, Some(close_tag)))
    }
}

impl<C: CustomNode> ParseRecoverable for NodeElement<C> {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        let open_tag: OpenTag = parser.parse_recoverable(input)?;
        let is_known_self_closed =
            |name| parser.config().always_self_closed_elements.contains(name);
        let is_raw = |name| parser.config().raw_text_elements.contains(name);

        let tag_name_str = &*open_tag.name.to_string();
        if open_tag.is_self_closed() || is_known_self_closed(tag_name_str) {
            return Some(NodeElement {
                open_tag,
                children: vec![],
                close_tag: None,
            });
        }
        let (children, close_tag) =
            Self::parse_children(parser, input, is_raw(tag_name_str), &open_tag)?;
        let element = NodeElement {
            open_tag,
            children,
            close_tag,
        };
        Some(element)
    }
}

impl<C: CustomNode> ParseRecoverable for Node<C> {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        let node = if C::peek_element(&input.fork()) {
            Node::Custom(C::parse_recoverable(parser, input)?)
        } else if input.peek(Token![<]) {
            if input.peek2(Token![!]) {
                if input.peek3(Ident) {
                    Node::Doctype(parser.parse_recoverable(input)?)
                } else {
                    Node::Comment(parser.parse_simple(input)?)
                }
            } else if input.peek2(Token![>]) {
                Node::Fragment(parser.parse_recoverable(input)?)
            } else {
                Node::Element(parser.parse_recoverable(input)?)
            }
        } else if input.peek(Brace) {
            Node::Block(parser.parse_recoverable(input)?)
        } else if input.peek(LitStr) {
            Node::Text(parser.parse_simple(input)?)
        } else if !input.is_empty() {
            // Parse any input except of any other Node starting
            Node::RawText(parser.parse_recoverable(input)?)
        } else {
            return None;
        };
        Some(node)
    }
}

// This method couldn't be const generic until https://github.com/rust-lang/rust/issues/63569
/// Parse array of tokens with
pub(super) fn parse_array_of2_tokens<T: Parse>(input: ParseStream) -> syn::Result<[T; 2]> {
    Ok([input.parse()?, input.parse()?])
}

pub(super) fn to_tokens_array<I>(input: &mut TokenStream, iter: I)
where
    I: IntoIterator,
    I::Item: ToTokens,
{
    use quote::TokenStreamExt;
    input.append_all(iter)
}

/// Replace the next [`TokenTree::Group`] in the given parse stream with a
/// token stream returned by a user callback, or parse as original block if
/// no token stream is returned.
fn block_transform(input: ParseStream, transform_fn: &TransformBlockFn) -> syn::Result<Block> {
    input.step(|cursor| {
        let (block_group, block_span, next) = cursor
            .group(Delimiter::Brace)
            .ok_or_else(|| cursor.error("unexpected: no Group found"))?;
        let parser = move |block_content: ParseStream| {
            let forked_block_content = block_content.fork();

            match transform_fn(&forked_block_content) {
                Ok(transformed_tokens) => match transformed_tokens {
                    Some(tokens) => {
                        let parser = move |input: ParseStream| {
                            Ok(block_expr_with_extern_span(input, block_span))
                        };
                        let transformed_content = parser.parse2(tokens)?;
                        block_content.advance_to(&forked_block_content);
                        transformed_content
                    }
                    None => block_expr_with_extern_span(block_content, block_span),
                },
                Err(error) => Err(error),
            }
        };

        Ok((parser.parse2(block_group.token_stream())?, next))
    })
}

#[allow(clippy::needless_pass_by_ref_mut)]
pub(crate) fn parse_valid_block_expr(
    parser: &mut RecoverableContext,
    input: syn::parse::ParseStream,
) -> syn::Result<Block> {
    let transform_block = parser.config().transform_block.clone();
    let value = if let Some(transform_fn) = transform_block {
        block_transform(input, &*transform_fn)?
    } else {
        block_expr(input)?
    };
    Ok(value)
}
/// Parse the given stream and span as [`Expr::Block`].
fn block_expr_with_extern_span(input: ParseStream, span: DelimSpan) -> syn::Result<Block> {
    Ok(Block {
        brace_token: Brace { span },
        stmts: Block::parse_within(input)?,
    })
}

/// Parse the given stream as [`Expr::Block`].
pub(crate) fn block_expr(input: syn::parse::ParseStream) -> syn::Result<Block> {
    let content;
    let brace_token = braced!(content in input);
    Ok(Block {
        brace_token,
        stmts: Block::parse_within(&content)?,
    })
}
