//!
//! Tokens that is used as parts of nodes, to simplify parsing.
//! Example:
//! `<!--` `<>` `</>` `<!` `/>`
//!
//! Also contain some entities that split parsing into several small units,
//! like: `<open_tag attr />`
//! `</close_tag>`

use proc_macro2::{Ident, TokenStream};
use proc_macro2_diagnostics2::{Diagnostic, Level};
use quote::ToTokens;
use syn::{
    ext::IdentExt,
    parse::{Parse, ParseStream},
    Token,
};

use crate::{
    node::{parse, NodeAttribute, NodeName},
    parser::recoverable::RecoverableContext,
};

pub(crate) mod tokens {
    //! Custom syn punctuations
    use proc_macro2::TokenStream;
    use quote::ToTokens;
    use syn::{
        custom_punctuation,
        parse::{Parse, ParseStream},
        Token,
    };

    use crate::node::parse;
    // Dash between node-name
    custom_punctuation!(Dash, -);

    // Later use custom punct, currently it is not compatible with quote;

    // // Start part of doctype tag
    //     // `<!`
    //     custom_punctuation!(DocStart, <!);

    //     // Start part of element's close tag.
    //     // Its commonly used as separator
    //     // `</`
    //     custom_punctuation!(CloseTagStart, </);

    //     custom_punctuation!(ComEnd, -->);

    //     //
    //     // Rest tokens is impossible to implement using custom_punctuation,
    //     // because they have Option fields, or more than 3 elems
    //     //
    /// Start part of doctype tag
    /// `<!`
    #[derive(Eq, PartialEq, Clone, Debug)]
    pub struct DocStart {
        pub token_lt: Token![<],
        pub token_not: Token![!],
    }

    impl Parse for DocStart {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            Ok(Self {
                token_lt: input.parse()?,
                token_not: input.parse()?,
            })
        }
    }

    impl ToTokens for DocStart {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.token_lt.to_tokens(tokens);
            self.token_not.to_tokens(tokens);
        }
    }

    /// Start part of comment tag
    /// `<!--`
    #[derive(Eq, PartialEq, Clone, Debug)]
    pub struct ComStart {
        pub token_lt: Token![<],
        pub token_not: Token![!],
        pub token_minus: [Token![-]; 2],
    }

    impl Parse for ComStart {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            Ok(Self {
                token_lt: input.parse()?,
                token_not: input.parse()?,
                token_minus: parse::parse_array_of2_tokens(input)?,
            })
        }
    }

    impl ToTokens for ComStart {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.token_lt.to_tokens(tokens);
            self.token_not.to_tokens(tokens);
            parse::to_tokens_array(tokens, self.token_minus);
        }
    }

    /// End part of comment tag
    /// `-->`
    #[derive(Eq, PartialEq, Clone, Debug)]
    pub struct ComEnd {
        pub token_minus: [Token![-]; 2],
        pub token_gt: Token![>],
    }

    impl Parse for ComEnd {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            Ok(Self {
                token_minus: parse::parse_array_of2_tokens(input)?,
                token_gt: input.parse()?,
            })
        }
    }

    impl ToTokens for ComEnd {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            parse::to_tokens_array(tokens, self.token_minus);
            self.token_gt.to_tokens(tokens);
        }
    }

    /// End part of element's open tag
    /// `/>` or `>`
    #[derive(Eq, PartialEq, Clone, Debug)]
    pub struct OpenTagEnd {
        pub token_solidus: Option<Token![/]>,
        pub token_gt: Token![>],
    }

    impl Parse for OpenTagEnd {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            Ok(Self {
                token_solidus: input.parse()?,
                token_gt: input.parse()?,
            })
        }
    }

    impl ToTokens for OpenTagEnd {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.token_solidus.to_tokens(tokens);
            self.token_gt.to_tokens(tokens);
        }
    }

    /// Start part of element's close tag.
    /// Its commonly used as separator
    /// `</`
    #[derive(Eq, PartialEq, Clone, Debug)]
    pub struct CloseTagStart {
        pub token_lt: Token![<],
        pub token_solidus: Token![/],
    }

    impl Parse for CloseTagStart {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            Ok(Self {
                token_lt: input.parse()?,
                token_solidus: input.parse()?,
            })
        }
    }

    impl ToTokens for CloseTagStart {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.token_lt.to_tokens(tokens);
            self.token_solidus.to_tokens(tokens);
        }
    }
}

pub use tokens::*;

/// Fragment open part
/// `<>`
#[derive(Eq, PartialEq, Clone, Debug)]
pub struct FragmentOpen {
    pub token_lt: Token![<],
    pub token_gt: Token![>],
}

impl Parse for FragmentOpen {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            token_lt: input.parse()?,
            token_gt: input.parse()?,
        })
    }
}

impl ToTokens for FragmentOpen {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.token_lt.to_tokens(tokens);
        self.token_gt.to_tokens(tokens);
    }
}

/// Fragment close part
/// `</>`
#[derive(Eq, PartialEq, Clone, Debug)]
pub struct FragmentClose {
    pub start_tag: tokens::CloseTagStart,
    pub token_gt: Token![>],
}

impl Parse for FragmentClose {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            start_tag: input.parse()?,
            token_gt: input.parse()?,
        })
    }
}

impl ToTokens for FragmentClose {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.start_tag.to_tokens(tokens);
        self.token_gt.to_tokens(tokens);
    }
}

impl FragmentClose {
    ///
    /// # Panics
    ///
    /// Panics if an identifier cannot be parsed after a successful peek (should
    /// be unreachable).
    pub fn parse_with_start_tag(
        parser: &mut RecoverableContext,
        input: syn::parse::ParseStream,
        start_tag: Option<tokens::CloseTagStart>,
    ) -> Option<Self> {
        let start_tag = start_tag?;
        if input.peek(Ident::peek_any) {
            let ident_from_invalid_closing = Ident::parse_any(input).expect("parse after peek");
            parser.push_diagnostic(Diagnostic::spanned(
                ident_from_invalid_closing.span(),
                Level::Error,
                "expected fragment closing, found element closing tag",
            ));
        }
        Some(Self {
            start_tag,
            token_gt: parser.parse_simple(input)?,
        })
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct TagGenerics {
    // None if no generics
    pub lt_token: Option<Token![<]>,
    pub args: syn::punctuated::Punctuated<syn::GenericArgument, Token![,]>,
    pub gt_token: Option<Token![>]>,
}

impl ToTokens for TagGenerics {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.lt_token.to_tokens(tokens);
        self.args.to_tokens(tokens);
        self.gt_token.to_tokens(tokens);
    }
}

impl TagGenerics {
    pub fn type_params(&self) -> impl Iterator<Item = &syn::Type> {
        self.args.iter().filter_map(|arg| {
            if let syn::GenericArgument::Type(ty) = arg {
                Some(ty)
            } else {
                None
            }
        })
    }
    pub fn lifetimes(&self) -> impl Iterator<Item = &syn::Lifetime> {
        self.args.iter().filter_map(|arg| {
            if let syn::GenericArgument::Lifetime(lt) = arg {
                Some(lt)
            } else {
                None
            }
        })
    }
    pub fn const_params(&self) -> impl Iterator<Item = &syn::Expr> {
        self.args.iter().filter_map(|arg| {
            if let syn::GenericArgument::Const(expr) = arg {
                Some(expr)
            } else {
                None
            }
        })
    }
    pub fn all_params(&self) -> impl Iterator<Item = &syn::GenericArgument> {
        self.args.iter()
    }
}

impl syn::parse::Parse for TagGenerics {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if !input.peek(Token![<]) {
            return Ok(TagGenerics::default());
        }

        let lt_token: Token![<] = input.parse()?;
        let args = syn::punctuated::Punctuated::parse_separated_nonempty(input)?;
        let gt_token: Token![>] = input.parse()?;

        Ok(Self {
            lt_token: Some(lt_token),
            args,
            gt_token: Some(gt_token),
        })
    }
}

/// Open tag for element, possibly self-closed.
/// `<name attr=x, attr_flag>`
#[derive(Clone, Debug)]
pub struct OpenTag {
    pub token_lt: Token![<],
    pub name: NodeName,
    pub generics: TagGenerics,
    pub attributes: Vec<NodeAttribute>,
    pub end_tag: tokens::OpenTagEnd,
}

impl ToTokens for OpenTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.token_lt.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.generics.to_tokens(tokens);
        parse::to_tokens_array(tokens, &self.attributes);
        self.end_tag.to_tokens(tokens);
    }
}

impl OpenTag {
    #[must_use]
    pub fn is_self_closed(&self) -> bool {
        self.end_tag.token_solidus.is_some()
    }
}

/// Open tag for element, `<name attr=x, attr_flag>`
#[derive(Clone, Debug)]
pub struct CloseTag {
    pub start_tag: tokens::CloseTagStart,
    pub name: NodeName,
    pub generics: TagGenerics,
    pub token_gt: Token![>],
}

impl Parse for CloseTag {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            start_tag: input.parse()?,
            name: input.parse()?,
            generics: input.parse()?,
            token_gt: input.parse()?,
        })
    }
}

impl ToTokens for CloseTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.start_tag.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.generics.to_tokens(tokens);
        self.token_gt.to_tokens(tokens);
    }
}

impl CloseTag {
    pub fn parse_with_start_tag(
        parser: &mut RecoverableContext,
        input: syn::parse::ParseStream,
        start_tag: Option<tokens::CloseTagStart>,
    ) -> Option<Self> {
        Some(Self {
            start_tag: start_tag?,
            name: parser.parse_simple(input)?,
            generics: parser.parse_simple(input)?,
            token_gt: parser.parse_simple(input)?,
        })
    }
}

#[cfg(test)]
mod test {
    use syn::custom_punctuation;

    use super::*;

    macro_rules! parse_quote {
            ($mod_name:ident, $name: ident=> $($tts:tt)*) => {
                mod $mod_name {
                    use super::*;
                    // use super::tokens::*;
                    #[test]
                    fn parse_quote() {

                        let tts = quote::quote!{
                            $($tts)*
                        };
                        syn::parse2::<$name>(tts).unwrap();

                    }
                }
            }
        }

    parse_quote! {docstart, DocStart => <!}

    parse_quote! {comstart, ComStart => <!--}

    parse_quote! {comend, ComEnd => -->}

    parse_quote! {open_tag_end1, OpenTagEnd => >}

    parse_quote! {open_tag_end2, OpenTagEnd => />}

    parse_quote! {close_tag_start, CloseTagStart => </}

    /// Custom punctuation wasnt compatible with quote,
    /// check if it now compatible to replace simple parser with it  
    #[test]
    fn parse_quote_doc_comp_custom_punct() {
        custom_punctuation!(CloseTagStart2, </);
        let tts = quote::quote! {
            </
        };
        syn::parse2::<CloseTagStart2>(tts).unwrap_err();
    }
}
