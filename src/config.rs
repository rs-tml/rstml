use std::{collections::HashSet, convert::Infallible, fmt::Debug, marker::PhantomData, rc::Rc};

use proc_macro2::TokenStream;
use syn::{parse::ParseStream, Result};

#[cfg(feature = "rawtext-stable-hack")]
use crate::rawtext_stable_hack::MacroPattern;
use crate::{
    atoms::{CloseTag, OpenTag},
    node::{CustomNode, NodeType},
};

pub type TransformBlockFn = dyn Fn(ParseStream) -> Result<Option<TokenStream>>;
pub type ElementWildcardFn = dyn Fn(&OpenTag, &CloseTag) -> bool;

/// Configures the `Parser` behavior
pub struct ParserConfig<C = Infallible> {
    pub(crate) flat_tree: bool,
    pub(crate) number_of_top_level_nodes: Option<usize>,
    pub(crate) type_of_top_level_nodes: Option<NodeType>,
    pub(crate) transform_block: Option<Rc<TransformBlockFn>>,
    pub(crate) recover_block: bool,
    pub(crate) always_self_closed_elements: HashSet<&'static str>,
    pub(crate) raw_text_elements: HashSet<&'static str>,
    pub(crate) element_close_wildcard: Option<Rc<ElementWildcardFn>>,
    #[cfg(feature = "rawtext-stable-hack")]
    pub(crate) macro_pattern: MacroPattern,
    custom_node: PhantomData<C>,
}

impl<C> Clone for ParserConfig<C> {
    fn clone(&self) -> Self {
        Self {
            flat_tree: self.flat_tree.clone(),
            number_of_top_level_nodes: self.number_of_top_level_nodes,
            type_of_top_level_nodes: self.type_of_top_level_nodes.clone(),
            transform_block: self.transform_block.clone(),
            recover_block: self.recover_block.clone(),
            always_self_closed_elements: self.always_self_closed_elements.clone(),
            raw_text_elements: self.raw_text_elements.clone(),
            element_close_wildcard: self.element_close_wildcard.clone(),
            #[cfg(feature = "rawtext-stable-hack")]
            macro_pattern: self.macro_pattern.clone(),
            custom_node: self.custom_node.clone(),
        }
    }
}

impl Default for ParserConfig {
    fn default() -> Self {
        Self {
            flat_tree: Default::default(),
            number_of_top_level_nodes: Default::default(),
            type_of_top_level_nodes: Default::default(),
            transform_block: Default::default(),
            recover_block: Default::default(),
            always_self_closed_elements: Default::default(),
            raw_text_elements: Default::default(),
            element_close_wildcard: Default::default(),
            #[cfg(feature = "rawtext-stable-hack")]
            macro_pattern: Default::default(),
            custom_node: Default::default(),
        }
    }
}

impl<C> Debug for ParserConfig<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct("ParserConfig");

        s.field("flat_tree", &self.flat_tree)
            .field("number_of_top_level_nodes", &self.number_of_top_level_nodes)
            .field("type_of_top_level_nodes", &self.type_of_top_level_nodes)
            .field("recover_block", &self.recover_block)
            .field(
                "always_self_closed_elements",
                &self.always_self_closed_elements,
            )
            .field("raw_text_elements", &self.raw_text_elements)
            .field(
                "element_close_wildcard",
                &self.element_close_wildcard.is_some(),
            );
        #[cfg(feature = "rawtext-stable-hack")]
        s.field("macro_pattern", &self.macro_pattern);
        s.finish()
    }
}

impl ParserConfig {
    /// Create new `ParserConfig` with default config
    pub fn new() -> ParserConfig {
        ParserConfig::default()
    }
}

impl<C> ParserConfig<C> {
    /// Return flat tree instead of nested tree
    pub fn flat_tree(mut self) -> Self {
        self.flat_tree = true;
        self
    }

    /// Exact number of required top level nodes
    pub fn number_of_top_level_nodes(mut self, number: usize) -> Self {
        self.number_of_top_level_nodes = Some(number);
        self
    }

    /// Enforce the `NodeType` of top level nodes
    pub fn type_of_top_level_nodes(mut self, node_type: NodeType) -> Self {
        self.type_of_top_level_nodes = Some(node_type);
        self
    }

    /// Try to parse invalid `syn::Block`.
    /// If set tot true, `NodeBlock` can return `Invalid` variant.
    ///
    /// If `NodeBlock` is failed to parse as `syn::Block`
    /// it still usefull to emit it as expression.
    /// It will enhance IDE compatibility, and provide completion in cases of
    /// invalid blocks, for example `{x.}` is invalid expression, because
    /// after dot token `}` is unexpected. But for ide it is a marker that
    /// quick completion should be provided.
    pub fn recover_block(mut self, recover_block: bool) -> Self {
        self.recover_block = recover_block;
        self
    }

    /// Set array of nodes that is known to be self closed,
    /// it also known as void element.
    /// Void elements has no child and must not have closing tag.
    /// Parser will not search for it closing tag,
    /// even if no slash at end of it open part was found.
    ///
    /// Because we work in proc-macro context, we expect it as 'static refs.
    ///
    /// Examples:
    /// <br> <link> <img>
    pub fn always_self_closed_elements(mut self, elements: HashSet<&'static str>) -> Self {
        self.always_self_closed_elements = elements;
        self
    }

    /// Set array of nodes that is known to be parsed in two-phases,
    /// Parser will skip parsing of children nodes.
    /// and provide one child with RawText instead.
    ///
    /// This is usefull when parsing `<script>` or `<style>` tags elements.
    ///
    /// If you need fragment to be used in this context, empty string("") should
    /// be inserted.
    ///
    /// Raw texts has few limitations, check out `RawText` documentation.
    pub fn raw_text_elements(mut self, elements: HashSet<&'static str>) -> Self {
        self.raw_text_elements = elements;
        self
    }

    /// Transforms the `value` of all `NodeType::Block`s with the given closure
    /// callback. The provided `ParseStream` is the content of the block.
    ///
    /// When `Some(TokenStream)` is returned, the `TokenStream` is parsed as
    /// Rust block content. The `ParseStream` must be completely consumed in
    /// this case, meaning no tokens can be left in the stream.
    ///
    /// If `None` is returned, parsing happens with the original `ParseStream`,
    /// since the tokens that are passend into the transform callback are a
    /// fork, which gets only advanced if `Some` is returned.
    ///
    /// An example usage might be a custom syntax inside blocks which isn't
    /// valid Rust. The given example simply translates the `%` character into
    /// the string `percent`
    ///
    /// ```rust
    /// use quote::quote;
    /// use syn::Token;
    /// use rstml::{parse2_with_config, ParserConfig};
    ///
    /// let tokens = quote! {
    ///     <div>{%}</div>
    /// };
    ///
    /// let config = ParserConfig::new().transform_block(|input| {
    ///     input.parse::<Token![%]>()?;
    ///     Ok(Some(quote! { "percent" }))
    /// });
    ///
    /// parse2_with_config(tokens, config).unwrap();
    /// ```
    pub fn transform_block<F>(mut self, callback: F) -> Self
    where
        F: Fn(ParseStream) -> Result<Option<TokenStream>> + 'static,
    {
        self.transform_block = Some(Rc::new(callback));
        self
    }

    /// Allows unmatched tag pairs where close tag matches a specified wildcard.
    ///
    /// For example:
    ///
    /// ```rust
    /// use quote::quote;
    /// use rstml::{
    ///     node::{Node, NodeElement},
    ///     Parser, ParserConfig,
    /// };
    /// use syn::{Expr, ExprRange, RangeLimits, Stmt};
    ///
    /// let config = ParserConfig::new()
    ///     .element_close_wildcard(|_open_tag, close_tag| close_tag.name.is_wildcard());
    ///
    /// let tokens = quote! {
    ///     <{"OpenTag"}>{"Content"}</_>
    /// };
    ///
    /// Parser::new(config).parse_simple(tokens).unwrap();
    /// ```
    pub fn element_close_wildcard<F>(mut self, predicate: F) -> Self
    where
        F: Fn(&OpenTag, &CloseTag) -> bool + 'static,
    {
        self.element_close_wildcard = Some(Rc::new(predicate));
        self
    }
    /// Allows unmatched tag pairs where close tag matches a specified wildcard.
    ///
    /// Uses default wildcard ident (`_`), and optionally check whenewer
    /// open_tag name is block.
    pub fn element_close_use_default_wildcard_ident(self, open_tag_should_be_block: bool) -> Self {
        self.element_close_wildcard(move |open_tag, close_tag| {
            close_tag.name.is_wildcard() && (!open_tag_should_be_block || open_tag.name.is_block())
        })
    }

    ///
    /// Provide pattern of macro call.
    ///
    /// It is used with feature = "rawtext-stable-hack" to retrive
    /// space information in `RawText`.
    /// Checkout https://github.com/rs-tml/rstml/issues/5 for details.
    ///
    ///
    /// This method receive macro pattern as `TokenStream`, uses tokens `%%`
    /// as marker for input `rstml::parse`.
    /// Support only one marker in pattern.
    /// Currently, don't check other tokens for equality with pattern.
    ///
    /// Example:
    ///
    /// Imagine one have macro, that used like this:
    /// ```
    /// html! {some_context, provided, [ can use groups, etc], {<div>}, [other context]};
    /// ```
    ///
    /// One can set `macro_call_pattern` like this:
    /// ```
    /// config.macro_call_pattern(quote!(html! // macro name currently is not checked
    ///     {ident, ident, // can use real idents, or any other
    ///         [/* can ignore context of auxilary groups */],
    ///         {%%}, // important part
    ///         []
    ///     }))
    /// ```
    ///
    /// Panics if no `%%` token was found.
    ///
    /// If macro_call_patern is set rstml will parse input two times in order to
    /// recover spaces in `RawText`. Rstml will panic if macro source text
    /// is not possible to recover.
    #[cfg(feature = "rawtext-stable-hack")]
    pub fn macro_call_pattern(mut self, pattern: TokenStream) -> Self {
        self.macro_pattern =
            MacroPattern::from_token_stream(pattern).expect("No %% token found in pattern.");
        self
    }

    /// Enables parsing for [`Node::Custom`] using a type implementing
    /// [`CustomNode`].
    pub fn custom_node<CN: CustomNode>(self) -> ParserConfig<CN> {
        ParserConfig {
            flat_tree: self.flat_tree,
            number_of_top_level_nodes: self.number_of_top_level_nodes,
            type_of_top_level_nodes: self.type_of_top_level_nodes,
            transform_block: self.transform_block,
            recover_block: self.recover_block,
            always_self_closed_elements: self.always_self_closed_elements,
            raw_text_elements: self.raw_text_elements,
            element_close_wildcard: self.element_close_wildcard,
            #[cfg(feature = "rawtext-stable-hack")]
            macro_pattern: self.macro_pattern,
            custom_node: Default::default(),
        }
    }
}
