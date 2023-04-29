use std::fmt::Display;

use proc_macro2::{TokenStream, TokenTree};
use syn::{parse::{Parse, ParseStream}, token::Brace, LitStr, Token};


/// Raw unquoted text
/// Internally it is still valid TokenStream.
/// So it cant contain any unclosed branches, braces or parens.
#[derive(Clone, Debug, syn_derive::ToTokens)]
pub struct RawText {
    token_stream: TokenStream
}

impl Parse for RawText {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut token_stream = TokenStream::new();
        let any_node = |input: ParseStream| input.peek(Token![<]) || input.peek(Brace) || input.peek(LitStr);
        // Parse any input until catching any node.
        // Fail only on eof.
        while !any_node(input) {
            token_stream.extend([input.parse::<TokenTree>()?])
        }
        Ok(Self {
            token_stream
        })
    }
}

// TODO: add spaces if needed.
impl Display for RawText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_stream)
    }
}