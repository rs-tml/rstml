//! Recoverable parser helper module. Contains trait and types that are using
//! during implementation of parsing with recovery after semantic errors.
//!
//! Recoverable parser is a type of parsing technique when parser don't give up
//! after getting invalid token, and instead continue to parse code to provide
//! more info about [`TokenStream`] to IDE or user.
//!
//! Instead of failing after first unclosed tag, or invalid block, recoverable
//! parser will try to check if there any other syntax or semantic errors.
//!
//! Example:
//! ```rust
//!   # use quote::quote;
//!   # use rstml::{Parser, ParserConfig};
//!   # Parser::new(ParserConfig::default()).parse_recoverable(quote! {
//!   <div hello={world.} /> // dot after world is invalid syn::Expr
//!   <>
//!       <div>"1"</x> // incorrect closed tag
//!       <div>"2"</div>
//!       <div>"3"</div>
//!       <div {"some-attribute-from-rust-block"}/>
//!   </>
//!   <bar> // unclosed tag
//!   # });
//! ```
//! If this example was parsed by regular parser, it will fail with "invalid
//! expression error" and no output. User will see only one error, and IDE
//! cannot produce any completion in case of invalid expression.
//!
//! But recoverable parser differ (see [`Parser::parse_recoverable`]), it will
//! return array of errors and array of [`Node`]. Errors should be emitted, and
//! value should be handled as no errors was found. In result, user will see all
//! errors, and IDE can provide completion even if some part of token stream was
//! unexpected.
//!
//!
//! [`TokenStream`]: https://doc.rust-lang.org/proc_macro/struct.TokenStream.html
//! [`Parser::parse_recoverable`]: struct.Parser.html#method.parse_recoverable
//! [`Node`]: struct.Node.html

use std::{collections::HashSet, fmt::Debug, rc::Rc};

use proc_macro2_diagnostics::{Diagnostic, Level};
use syn::parse::{Parse, ParseStream};

use crate::{
    config::{ElementWildcardFn, TransformBlockFn},
    node::CustomNode,
    ParserConfig,
};

/// Config of parser.
/// Used to extend parsing functionality by user needs.
///
/// Can't be created directly, instead use [`From<ParserConfig>::from`].
#[derive(Default, Clone)]
pub struct RecoveryConfig {
    ///
    /// Try to parse invalid syn::Block as something.
    /// Usefull to make expressions more IDE-friendly.
    pub(crate) recover_block: bool,
    /// elements that has no child and is always self closed like <img> and <br>
    pub(crate) always_self_closed_elements: HashSet<&'static str>,
    /// Elements like `<script>` `<style>`, context of which is not a valid
    /// html, and should be provided as is.
    pub(crate) raw_text_elements: HashSet<&'static str>,
    pub(crate) transform_block: Option<Rc<TransformBlockFn>>,
    /// Allows wildcard closing tag matching for blocks
    pub(crate) element_close_wildcard: Option<Rc<ElementWildcardFn>>,
}
impl PartialEq for RecoveryConfig {
    fn eq(&self, other: &Self) -> bool {
        self.recover_block == other.recover_block
            && self.always_self_closed_elements == other.always_self_closed_elements
            && self.raw_text_elements == other.raw_text_elements
            && self.transform_block.is_some() == other.transform_block.is_some()
            && self.element_close_wildcard.is_some() == other.element_close_wildcard.is_some()
    }
}
impl Eq for RecoveryConfig {}

impl Debug for RecoveryConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RecoveryConfig")
            .field("recover_block", &self.recover_block)
            .field(
                "always_self_closed_elements",
                &self.always_self_closed_elements,
            )
            .field("raw_text_elements", &self.raw_text_elements)
            .field(
                "element_close_wildcard",
                &self.element_close_wildcard.is_some(),
            )
            .finish()
    }
}

/// Context that is provided in [`ParseRecoverable`] interface.
/// Used to save [`Diagnostic`] messages or [`syn::Result`].
///
/// Also can be extended with user needs through [`RecoveryConfig`].
#[derive(Debug, Default, Clone)]
pub struct RecoverableContext {
    pub(super) diagnostics: Vec<Diagnostic>,
    config: RecoveryConfig,
}

impl PartialEq for RecoverableContext {
    fn eq(&self, other: &Self) -> bool {
        if self.diagnostics.len() != other.diagnostics.len() || self.config != other.config {
            return false;
        }

        self.diagnostics
            .iter()
            .zip(other.diagnostics.iter())
            .all(|(a, b)| format!("{:?}", a) == format!("{:?}", b))
    }
}
impl Eq for RecoverableContext {}

impl RecoverableContext {
    pub fn new(config: RecoveryConfig) -> Self {
        Self {
            diagnostics: vec![],
            config,
        }
    }
    pub fn config(&self) -> &RecoveryConfig {
        &self.config
    }
    pub fn parse_result<T>(self, val: Option<T>) -> ParsingResult<T> {
        ParsingResult::from_parts(val, self.diagnostics)
    }

    /// Parse token using [`syn::parse::Parse`]
    pub fn parse_simple<T: Parse>(&mut self, input: ParseStream) -> Option<T> {
        match input.parse() {
            Ok(v) => Some(v),
            Err(e) => {
                self.push_diagnostic(e);
                None
            }
        }
    }
    /// Parse token using closure
    pub fn parse_mixed_fn<F, T>(&mut self, input: ParseStream, mut parser: F) -> Option<T>
    where
        F: FnMut(&mut Self, ParseStream) -> Result<T, syn::Error>,
    {
        match parser(self, input) {
            Ok(v) => Some(v),
            Err(e) => {
                self.push_diagnostic(e);
                None
            }
        }
    }

    /// Parse token using [`ParseRecoverable`]
    pub fn parse_recoverable<T: ParseRecoverable>(&mut self, input: ParseStream) -> Option<T> {
        T::parse_recoverable(self, input)
    }

    /// Save diagnostic message of [`syn::Result`]
    /// and convert result to `Option`, that can be used directly
    /// as output in [`ParseRecoverable::parse_recoverable`]
    pub fn save_diagnostics<T>(&mut self, val: syn::Result<T>) -> Option<T> {
        match val {
            Ok(v) => Some(v),
            Err(e) => {
                self.push_diagnostic(e);
                None
            }
        }
    }

    /// Push custom message of [`syn::Error`] or
    /// [`proc_macro2_diagnostics::Diagnostic`]
    pub fn push_diagnostic(&mut self, diagnostic: impl Into<Diagnostic>) {
        let diag = diagnostic.into();
        // println!(
        //     "Push diagnostic: {:?}, backtrace={}",
        //     diag,
        //     std::backtrace::Backtrace::capture()
        // );
        self.diagnostics.push(diag);
    }
}

///
/// Result of parsing.
#[derive(Debug)]
pub enum ParsingResult<T> {
    /// Fully valid ast that was parsed without errors.
    Ok(T),
    /// The ast contain invalid starting tokens, and cannot be parsed.
    Failed(Vec<Diagnostic>),
    /// The ast can be partially parsed,
    /// but some tokens was skipped during parsing, or their meaning was
    /// changed.
    Partial(T, Vec<Diagnostic>),
}

impl<T> ParsingResult<T> {
    /// Create new ParsingResult from optional value and accumulated errors.
    pub fn from_parts(value: Option<T>, errors: Vec<Diagnostic>) -> Self {
        match (value, errors) {
            (Some(v), err) if err.is_empty() => Self::Ok(v),
            (Some(v), err) => Self::Partial(v, err),
            (None, err) => Self::Failed(err),
        }
    }

    ///
    /// Convert into [`syn::Result`], with fail on first diagnostic message,
    /// Ignores any diagnostic non error message when result is available.
    /// Returns Error on [`ParsingResult::Failed`], and
    /// [`ParsingResult::Partial`].
    pub fn into_result(self) -> syn::Result<T> {
        match self {
            ParsingResult::Ok(r) => Ok(r),
            ParsingResult::Failed(errors) => Err(errors
                .into_iter()
                .next()
                .unwrap_or_else(|| {
                    Diagnostic::new(
                        Level::Error,
                        "Object parsing failed, but no additional info was provided",
                    )
                })
                .into()),
            ParsingResult::Partial(ok, errors) => {
                if let Some(err) = errors
                    .into_iter()
                    .filter(|p| p.level() == Level::Error)
                    .next()
                {
                    Err(err.into())
                } else {
                    Ok(ok)
                }
            }
        }
    }

    pub fn split(self) -> (Option<T>, Vec<Diagnostic>) {
        match self {
            Self::Ok(r) => (Some(r), vec![]),
            Self::Failed(errors) => (None, errors),
            Self::Partial(r, errors) => (Some(r), errors),
        }
    }

    pub fn push_diagnostic(&mut self, diagnostic: Diagnostic) {
        *self = match std::mem::replace(self, ParsingResult::Failed(vec![])) {
            Self::Ok(r) => Self::Partial(r, vec![diagnostic]),
            Self::Failed(errors) => {
                Self::Failed(errors.into_iter().chain(Some(diagnostic)).collect())
            }
            Self::Partial(r, errors) => {
                Self::Partial(r, errors.into_iter().chain(Some(diagnostic)).collect())
            }
        };
    }

    pub fn is_ok(&self) -> bool {
        matches!(self, Self::Ok(_))
    }
}

impl<T> ParsingResult<Vec<T>> {
    pub fn split_vec(self) -> (Vec<T>, Vec<Diagnostic>) {
        let (r, e) = self.split();
        (r.unwrap_or_default(), e)
    }
    pub fn from_parts_vec(value: Vec<T>, errors: Vec<Diagnostic>) -> Self {
        match (value, errors) {
            (v, err) if err.is_empty() => Self::Ok(v),
            (v, err) if !v.is_empty() => Self::Partial(v, err),
            (_, err) => Self::Failed(err),
        }
    }
}

impl<T> From<syn::Result<T>> for ParsingResult<T> {
    ///
    /// Convert into syn::Result,
    /// Returns Error on ParsingResult::Failed, and ParsingResult::Partial.
    fn from(result: syn::Result<T>) -> ParsingResult<T> {
        match result {
            Result::Ok(r) => ParsingResult::Ok(r),
            Result::Err(e) => ParsingResult::Failed(vec![e.into()]),
        }
    }
}

impl<C: CustomNode> From<ParserConfig<C>> for RecoveryConfig {
    fn from(config: ParserConfig<C>) -> Self {
        RecoveryConfig {
            recover_block: config.recover_block,
            raw_text_elements: config.raw_text_elements.clone(),
            always_self_closed_elements: config.always_self_closed_elements.clone(),
            transform_block: config.transform_block.clone(),
            element_close_wildcard: config.element_close_wildcard.clone(),
        }
    }
}

///
/// Adaptor to provide a [`syn::parse::Parse`] interface to [`ParseRecoverable`]
/// types. Returns error if any error was set in [`RecoverableContext`] during
/// parsing. Use Default implementation of [`RecoveryConfig`].
///
/// Panics:
/// If [`ParseRecoverable`] implementation doesn't save any diagnostic message,
/// and return [`None`].
pub struct Recoverable<T>(pub T);
impl<T> Recoverable<T> {
    pub fn inner(self) -> T {
        self.0
    }
}

impl<T: ParseRecoverable> Parse for Recoverable<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut empty_context = RecoverableContext::default();
        let parse = T::parse_recoverable(&mut empty_context, input);
        empty_context
            .parse_result(parse)
            .into_result()
            .map(Recoverable)
    }
}

///
/// Parsing interface for recoverable [`TokenStream`] parsing,
///     analog to [`syn::parse::Parse`] but with ability to skip unexpected
/// tokens, and more diagnostic messages.
///
/// - If input stream can be parsed to valid, or partially valid object
/// [`Option::Some`] should be returned.
///
/// - If object is parsed partially one can save
/// diagnostic message in [`RecoverableContext`].
///
/// - If object is failed to parse
/// [`Option::None`] should be returned, and any message should be left in
/// [`RecoverableContext`].
///
/// Instead of using [`RecoverableContext`] the interface can be changed to the
/// following:
/// ```rust
/// # use syn::parse::ParseStream;
/// # use rstml::ParsingResult;
/// pub trait ParseRecoverable: Sized {
///     fn parse_recoverable(input: ParseStream) -> ParsingResult<Self>;
/// }
/// ```
/// It would more type-safe, but because [`std::ops::Try`] is not stable,
/// writing implementation for this trait would end with a lot of boilerplate.
///
/// [`TokenStream`]: https://doc.rust-lang.org/proc_macro/struct.TokenStream.html
pub trait ParseRecoverable: Sized {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self>;
}
