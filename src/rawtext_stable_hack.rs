//! Recover space from token stream, using hack described in https://github.com/rs-tml/rstml/issues/5
//! This hack can be activated using feature = "rawtext-stable-hack", it has no
//! penalty in nightly, or rust-analyzer but on stable it parses input two
//! times.

use proc_macro2::{Span, TokenStream, TokenTree};

use crate::node::{CustomNode, Node};

/// Returns true if join span is available.
pub fn is_join_span_available() -> bool {
    let join_call_span = Span::call_site().join(Span::call_site()).is_some();

    // Hack: Rust analyzer will return first span on join, but source_text is
    // missing in this case.
    let source_text_available = Span::call_site().source_text().is_some();
    join_call_span && source_text_available
}

/// Returns true if current macro call is available inside file.
/// Returns false if it is from other macro call.
pub fn is_macro_args_recoverable() -> bool {
    Span::call_site().source_text().is_some()
}

// Inject default text to every raw node, just to avoid panics.
pub fn inject_raw_text_default<C: CustomNode>(source: &mut [Node<C>]) {
    for source in source.into_iter() {
        replace_node_default(source)
    }
}

// Inject raw text to every raw node, recovered using proc-macro2 second
// parsing;
pub fn inject_raw_text<C: CustomNode + std::fmt::Debug>(
    source: &mut [Node<C>],
    hacked: &[Node<C>],
) {
    assert_eq!(
        source.len(),
        hacked.len(),
        "Second parsing return different result in recover_space_hack"
    );
    for (source, hacked) in source.into_iter().zip(hacked) {
        replace_node(source, hacked)
    }
}

pub fn replace_node_default<C: CustomNode>(source: &mut Node<C>) {
    match source {
        Node::RawText(source )=> source.init_recover_space(String::from("")),
        Node::Fragment(source) => {
            inject_raw_text_default(&mut source.children)
        }
        Node::Element(source) => {
                        inject_raw_text_default(&mut source.children)
        }
        Node::Doctype(_) | // => source.value.recover_space(&hacked.value),
        Node::Block(_)
        | Node::Comment(_)
        | Node::Custom(_)
        | Node::Text(_) => {}
    }
}

pub fn replace_node<C: CustomNode + std::fmt::Debug>(source: &mut Node<C>, hacked: &Node<C>) {
    match (source, hacked) {
        (Node::RawText(source), Node::RawText(hacked)) => source.recover_space(&hacked),
        (Node::Fragment(source), Node::Fragment(hacked)) => {
            inject_raw_text(&mut source.children, &hacked.children)
        }
        (Node::Element(source), Node::Element(hacked)) => {
            inject_raw_text(&mut source.children, &hacked.children)
        }
        (Node::Doctype(_), Node::Doctype(_)) | // => source.value.recover_space(&hacked.value),
        (Node::Block(_), Node::Block(_))
        | (Node::Comment(_), Node::Comment(_))
        | (Node::Custom(_), Node::Custom(_))
        | (Node::Text(_), Node::Text(_)) => {}
        (source, hacked) => {
            panic!(
                "Mismatched node type in recover_space_hack {:?}, {:?}",
                source, hacked
            )
        }
    }
}

// TODO: Add possibility to check macro name.
#[derive(Debug, Clone)]
pub enum TokenStreamOperations {
    SkipToken(usize),
    SkipUntil(TokenStream),
    UnwrapGroup,
}

#[derive(Clone, Debug, Default)]
pub struct MacroPattern {
    macro_operations: Vec<TokenStreamOperations>,
}

impl MacroPattern {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn skip_tokens(mut self, num_tokens: usize) -> Self {
        assert!(num_tokens > 0, "num_tokens should be > 0");
        if let Some(TokenStreamOperations::SkipToken(ref mut already_skipped)) =
            self.macro_operations.last_mut()
        {
            *already_skipped += num_tokens;
        } else {
            self.macro_operations
                .push(TokenStreamOperations::SkipToken(num_tokens));
        }
        self
    }

    pub fn unwrap_group(mut self) -> Self {
        self.macro_operations
            .push(TokenStreamOperations::UnwrapGroup);
        self
    }

    pub fn skip_until(mut self, expected_stream: TokenStream) -> Self {
        self.macro_operations
            .push(TokenStreamOperations::SkipUntil(expected_stream));
        self
    }
    /// Try to create `RecoverSpacePattern` from token_stream example.
    /// Find first occurence of '%%' tokens, and use its position in tokens as
    /// marker. Example:
    /// original macro:
    /// ```no_compile
    /// html! {some_context, provided, [ can use guards, etc], {<div>}, [other context]};
    /// RecoverSpacePattern::from_token_stream(quote!(
    ///     html! {ident, ident, // can use real idents, or any other
    ///         [/* can ignore context of auxilary groups */],
    ///         {%%}, // important part
    ///         []
    ///     }
    /// ))
    /// .unwrap()
    /// ```
    pub fn from_token_stream(stream: TokenStream) -> Option<Self> {
        let mut pattern = MacroPattern::new();
        let mut stream = stream.into_iter();

        // skip any token before first '!'
        // to allow using macro with differnet paths.
        while let Some(t) = stream.next() {
            if let TokenTree::Punct(p) = t {
                if p.as_char() == '!' {
                    break;
                }
            }
        }
        pattern = pattern.skip_until(quote::quote! {!});

        if let Some(post_res) = Self::from_token_stream_inner(stream) {
            pattern
                .macro_operations
                .extend_from_slice(&post_res.macro_operations);
            return Some(pattern);
        }

        None
    }
    fn from_token_stream_inner(stream: impl IntoIterator<Item = TokenTree>) -> Option<Self> {
        let mut pattern = MacroPattern::new();

        let mut last_token_percent = false;

        for tt in stream {
            match tt {
                TokenTree::Group(group) => {
                    if let Some(post_res) = Self::from_token_stream_inner(group.stream()) {
                        pattern = pattern.unwrap_group();
                        pattern
                            .macro_operations
                            .extend_from_slice(&post_res.macro_operations);
                        return Some(pattern);
                    }
                }
                TokenTree::Punct(p) => match (p.as_char(), last_token_percent) {
                    ('%', true) => return Some(pattern),
                    ('%', false) => {
                        last_token_percent = true;
                        // ignore skip
                        continue;
                    }
                    (_, true) => {
                        last_token_percent = false;
                        // skip previous token
                        pattern = pattern.skip_tokens(1);
                    }
                    (_, false) => {}
                },
                _ => {}
            }
            pattern = pattern.skip_tokens(1)
        }
        None
    }

    /// Try to capture macro input from outer `TokenStream` using initialized
    /// `MacroPattern`. Returns None if needed TokenTree::Group wasn't
    /// found.
    ///
    /// "html!{/*content*/}" ==> "/*content*/"
    /// If pattern `is_empty` returns source stream.
    pub fn match_content(&self, source: TokenStream) -> Option<TokenStream> {
        let mut stream = source.into_iter();
        for op in &self.macro_operations {
            match op {
                TokenStreamOperations::SkipToken(num) => {
                    let _ = stream.nth(num - 1);
                }
                TokenStreamOperations::UnwrapGroup => match stream.next() {
                    Some(TokenTree::Group(g)) => stream = g.stream().into_iter(),
                    _ => return None,
                },
                // Skip tokens until expected [`TokenTree`] is found.
                // Compare `TokenTree` using `to_string()` function.
                TokenStreamOperations::SkipUntil(expected) => 'compare: loop {
                    let needed = expected.clone().into_iter();
                    for expected in needed {
                        let Some(t) = stream.next() else { return None };

                        if t.to_string() != expected.to_string() {
                            continue 'compare;
                        }
                    }
                    break;
                },
            }
        }
        Some(stream.collect())
    }

    pub fn is_empty(&self) -> bool {
        self.macro_operations.is_empty()
    }
}

#[cfg(test)]
mod test {
    use quote::quote;

    use super::MacroPattern;

    #[test]
    fn macro_content_matcher() {
        let pattern = MacroPattern::from_token_stream(quote! {html!{ctx, other_arg, %%}}).unwrap();
        let content = pattern
            .match_content(quote! {html!{ctx, div, <div> <Foo/> </div>}})
            .unwrap();

        assert_eq!(
            content.to_string(),
            quote! {<div> <Foo/> </div>}.to_string()
        );
    }

    #[test]
    fn macro_content_matcher_ignore_single_percent() {
        let pattern = MacroPattern::from_token_stream(quote! {html!{ctx, %other_arg, %%}}).unwrap();
        // also check that ignore percent inside html input
        let content = pattern
            .match_content(quote! {html!{ctx, %div, <div>%% <Foo/> </div>}})
            .unwrap();

        assert_eq!(
            content.to_string(),
            quote! {<div>%% <Foo/> </div>}.to_string()
        );
    }

    #[test]
    fn macro_content_matcher_group() {
        // group type does not count
        let pattern =
            MacroPattern::from_token_stream(quote! {html!{ctx, other_arg, {%%}}}).unwrap();
        let content = pattern
            .match_content(quote! {html!{ctx, div, [<div> <Foo/> </div>]}})
            .unwrap();

        assert_eq!(
            content.to_string(),
            quote! {<div> <Foo/> </div>}.to_string()
        );
    }

    #[test]
    fn macro_content_matcher_with_postfix_group() {
        // group type does not count
        let pattern = MacroPattern::from_token_stream(
            quote! {html!{ctx, other_arg, {%%}, any other context}},
        )
        .unwrap();
        let content = pattern
            .match_content(quote! {html!{ctx, div, [<div> <Foo/> </div>], foo}})
            .unwrap();

        assert_eq!(
            content.to_string(),
            quote! {<div> <Foo/> </div>}.to_string()
        );
    }

    #[test]
    fn extend_macro_matcher_using_until_token() {
        // group type does not count
        let pattern = MacroPattern::from_token_stream(
            quote! {html!{ctx, other_arg, {%%}, any other context}},
        )
        .unwrap()
        .skip_until(quote! {<Baz/>});

        let content = pattern
            .match_content(quote! {html!{ctx, div, [any content before Baz tag is ignored <div> </div> {foo:red} <Baz/> <div> <Foo/> </div>], foo}})
            .unwrap();

        assert_eq!(
            content.to_string(),
            quote! {<div> <Foo/> </div>}.to_string()
        );
    }

    #[test]
    fn check_macro_patch_differ() {
        let pattern = MacroPattern::from_token_stream(quote! {html!{ctx, other_arg, %%}}).unwrap();
        let content = pattern
            .match_content(
                quote! {some_other::path::to_macro::html!{ctx, div, <div> <Foo/> </div>}},
            )
            .unwrap();

        assert_eq!(
            content.to_string(),
            quote! {<div> <Foo/> </div>}.to_string()
        );
    }
}
