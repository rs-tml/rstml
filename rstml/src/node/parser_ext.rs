use proc_macro2::{TokenStream, TokenTree};
use proc_macro2_diagnostics::{Diagnostic, Level};
use syn::{
    parse::{discouraged::Speculative, Parse, ParseStream, Parser},
    spanned::Spanned,
};

use crate::recoverable::{ParseRecoverable, RecoverableContext};

impl RecoverableContext {
    /// Like [`parse_simple`], but splits the tokenstream at `E` first only
    /// parsing the tokens before it as `T`.
    ///
    /// **Note:** This is an internal function exported to make parsing of
    /// custom nodes easier. It has some quirks, e.g.,
    /// `parse_simple_until<Expr, Token![>]>`, would not support any
    /// [`Expr`] containing a `>`.
    ///
    /// It is not considered stable.
    ///
    /// [`parse_simple`]: #method.parse_simple
    /// [`Expr`]: https://docs.rs/syn/latest/syn/enum.Expr.html
    pub fn parse_simple_until<T: Parse, E: Parse>(&mut self, input: ParseStream) -> Option<(T, E)> {
        let mut tokens = TokenStream::new();
        let res = loop {
            // Use fork, because we can't limit separator to be only Peekable for custom
            // tokens but we also need to parse complex expressions like
            // "foo=x/y" or "/>"
            let fork = input.fork();
            if let Ok(end) = fork.parse() {
                input.advance_to(&fork);
                break Some(end);
            }

            if input.is_empty() {
                break None;
            }

            let next: TokenTree = self
                .parse_simple(input)
                .expect("TokenTree should always be parsable");
            tokens.extend([next]);
        };
        res.and_then(|res| {
            self.save_diagnostics(syn::parse2(tokens))
                .map(|val| (val, res))
        })
    }

    /// Parse array of toknes using recoverable parser.
    /// Stops parsing when other branch could parse anything.
    ///
    /// **Note:** This is an internal function exported to make parsing of
    /// custom nodes easier.
    /// It is not considered stable.
    ///
    /// Example:
    /// ```ignore
    /// # use syn::{parse::{Parser, ParseStream}, Ident, Result, parse_macro_input, Token};
    /// # use rstml::{parse_tokens_until};
    /// # fn main() -> syn::Result<()>{
    /// let tokens:proc_macro2::TokenStream = quote::quote!(few idents seperated by spaces and then minus sign - that will stop parsing).into();
    /// let concat_idents_without_minus = |input: ParseStream| -> Result<String> {
    ///     let (idents, _minus) = parser.parse_tokens_until::<Ident, _, _>(input, |i|
    ///         i.parse::<Token![-]>()
    ///     )?;
    ///     let mut new_str = String::new();
    ///     for ident in idents {
    ///         new_str.push_str(&ident.to_string())
    ///     }
    ///     // .. skip rest idents in input
    /// #    while !input.is_empty() {
    /// #        input.parse::<Ident>()?;
    /// #    }
    ///     Ok(new_str)
    /// };
    /// let concated = concat_idents_without_minus.parse2(tokens)?;
    /// assert_eq!(concated, "fewidentsseperatedbyspacesandthenminussign");
    /// # Ok(())
    /// # }
    /// ```
    pub fn parse_tokens_until_call<T, F, U>(
        &mut self,
        input: ParseStream,
        stop_fn: F,
    ) -> (Vec<T>, Option<U>)
    where
        T: ParseRecoverable + Spanned,
        F: Fn(ParseStream) -> syn::Result<U>,
    {
        let mut collection = vec![];
        let res = loop {
            let old_cursor = input.cursor();
            let fork = input.fork();
            if let Ok(res) = stop_fn(&fork) {
                input.advance_to(&fork);
                break Some(res);
            }
            if let Some(o) = self.parse_recoverable(input) {
                collection.push(o)
            }

            if old_cursor == input.cursor() {
                break None;
            }
        };
        (collection, res)
    }
    /// Two-phase parsing, firstly find separator, and then parses array of
    /// tokens before separator.
    /// For simple input this method will work like
    /// `parse_tokens_until`.
    /// Internally it creates intermediate `TokenStream`` and
    /// copy of all tokens until separator token is found. It is usefull
    /// when separator (or it's part) can be treated as part of token T.
    ///
    ///
    /// **Note:** This is an internal function exported to make parsing of
    /// custom nodes easier.
    /// It is not considered stable.
    ///
    /// Example:
    /// ```ignore
    /// let tokens = quote!(some_expr_seperated + with - lt_gt * tokens <> other part);
    /// ```
    /// In this example "<" can can be parsed as part of expression, but we want
    /// to split tokens after "<>" was found. So instead of parsing all
    /// input as expression, firstly we need to seperate it into two chunks.
    pub fn parse_tokens_with_conflicted_ending<T, F, U>(
        &mut self,
        input: ParseStream,
        separator: F,
    ) -> (Vec<T>, Option<U>)
    where
        T: ParseRecoverable,
        F: Fn(ParseStream) -> syn::Result<U>,
    {
        let parser = |parser: &mut Self, tokens: TokenStream| {
            let parse = |input: ParseStream| {
                let mut collection = vec![];

                while !input.is_empty() {
                    let old_cursor = input.cursor();
                    if let Some(o) = parser.parse_recoverable(input) {
                        collection.push(o)
                    }
                    if old_cursor == input.cursor() {
                        break;
                    }
                }
                let eated_tokens = input.parse::<TokenStream>()?;
                Ok((collection, eated_tokens))
            };
            let (collection, eaten_tokens) = parse.parse2(tokens).expect("No errors allowed");
            if !eaten_tokens.is_empty() {
                parser.push_diagnostic(Diagnostic::spanned(
                    eaten_tokens.span(),
                    Level::Error,
                    "tokens was ignored during parsing",
                ))
            }
            collection
        };
        self.parse_with_ending(input, parser, separator)
    }

    pub(crate) fn parse_with_ending<F, CNV, V, U>(
        &mut self,
        input: ParseStream,
        parser: CNV,
        ending: F,
    ) -> (V, Option<U>)
    where
        F: Fn(ParseStream) -> syn::Result<U>,
        CNV: Fn(&mut Self, TokenStream) -> V,
    {
        let mut tokens = TokenStream::new();
        let res = loop {
            // Use fork, because we can't limit separator to be only Peekable for custom
            // tokens but we also need to parse complex expressions like
            // "foo=x/y" or "/>"
            let fork = input.fork();
            if let Ok(end) = ending(&fork) {
                input.advance_to(&fork);
                break Some(end);
            }

            if input.is_empty() {
                break None;
            }

            let next: TokenTree = self
                .parse_simple(input)
                .expect("TokenTree should always be parsable");
            tokens.extend([next]);
        };
        (parser(self, tokens), res)
    }
}
