use proc_macro2::{TokenStream, TokenTree};
use syn::parse::{discouraged::Speculative, Parse, ParseStream};

use crate::recoverable::RecoverableContext;

impl RecoverableContext {
    /// Like [`parse_simple`], but splits the tokenstream at `E` first only
    /// parsing the tokens before it as `T`.
    ///
    /// [`parse_simple`]: #method.parse_simple
    pub fn parse_simple_with_ending<T: Parse, E: Parse>(
        &mut self,
        input: ParseStream,
    ) -> Option<(T, E)> {
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
}
