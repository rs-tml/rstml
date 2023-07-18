use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    parse::{discouraged::Speculative, Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    token::{Brace, Comma, Paren},
    Attribute, Expr, Lit, Pat, PatType, Token,
};

use crate::{
    node::{NodeBlock, NodeName},
    parser::recoverable::{ParseRecoverable, RecoverableContext},
};

#[derive(Clone, Debug, syn_derive::ToTokens)]
pub struct AttributeValueExpr {
    pub token_eq: Token![=],
    pub value: Expr,
}
impl AttributeValueExpr {
    ///
    /// Returns string representation of inner value,
    /// if value expression contain something that can be treated as displayable
    /// literal.
    ///
    /// Example of displayable literals:
    /// `"string"`      // string
    /// `'c'`           // char
    /// `0x12`, `1231`  // integer - converted to decimal form
    /// `0.12`          // float point value - converted to decimal form
    /// `true`, `false` // booleans
    ///
    /// Examples of literals that also will be non-displayable:
    /// `b'a'`     // byte
    /// `b"asdad"` // byte-string
    ///
    /// Examples of non-static non-displayable expressions:
    /// `{ x + 1}`     // block of code
    /// `y`            // usage of variable
    /// `|v| v + 1`    // closure is valid expression too
    /// `[1, 2, 3]`    // arrays,
    /// `for/while/if` // any controll flow
    /// .. and this list can be extended
    ///
    /// Adapted from leptos
    pub fn value_literal_string(&self) -> Option<String> {
        match &self.value {
            Expr::Lit(l) => match &l.lit {
                Lit::Str(s) => Some(s.value()),
                Lit::Char(c) => Some(c.value().to_string()),
                Lit::Int(i) => Some(i.base10_digits().to_string()),
                Lit::Float(f) => Some(f.base10_digits().to_string()),
                Lit::Bool(b) => Some(b.value.to_string()),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Clone, Debug, syn_derive::ToTokens)]
pub enum KeyedAttributeValue {
    Binding(FnBinding),
    Value(AttributeValueExpr),
    None,
}

impl KeyedAttributeValue {
    pub fn to_value(&self) -> Option<&AttributeValueExpr> {
        match self {
            KeyedAttributeValue::Value(v) => Some(v),
            KeyedAttributeValue::None => None,
            KeyedAttributeValue::Binding(_) => None,
        }
    }
}
///
/// Element attribute with fixed key.
///
/// Example:
/// key=value // attribute with ident as value
/// key // attribute without value
#[derive(Clone, Debug, syn_derive::ToTokens)]
pub struct KeyedAttribute {
    /// Key of the element attribute.
    pub key: NodeName,
    /// Value of the element attribute.
    pub possible_value: KeyedAttributeValue,
}
impl KeyedAttribute {
    ///
    /// Returns string representation of inner value,
    /// if value expression contain something that can be treated as displayable
    /// literal.
    ///
    /// Example of displayable literals:
    /// `"string"`      // string
    /// `'c'`           // char
    /// `0x12`, `1231`  // integer - converted to decimal form
    /// `0.12`          // float point value - converted to decimal form
    /// `true`, `false` // booleans
    ///
    /// Examples of literals that also will be non-displayable:
    /// `b'a'`     // byte
    /// `b"asdad"` // byte-string
    ///
    /// Examples of non-static non-displayable expressions:
    /// `{ x + 1}`     // block of code
    /// `y`            // usage of variable
    /// `|v| v + 1`    // closure is valid expression too
    /// `[1, 2, 3]`    // arrays,
    /// `for/while/if` // any controll flow
    /// .. and this list can be extended
    ///
    /// Adapted from leptos
    pub fn value_literal_string(&self) -> Option<String> {
        self.possible_value
            .to_value()
            .and_then(|v| v.value_literal_string())
    }

    pub fn value(&self) -> Option<&Expr> {
        self.possible_value.to_value().map(|v| &v.value)
    }

    // Checks if error is about eof.
    // This error is known to report Span::call_site.
    // Correct them to point to ParseStream
    pub(crate) fn correct_expr_error_span(error: syn::Error, input: ParseStream) -> syn::Error {
        let error_str = error.to_string();
        if error_str.starts_with("unexpected end of input") {
            let stream = input
                .parse::<TokenStream>()
                .expect("BUG: Token stream should always be parsable");
            return syn::Error::new(
                stream.span(),
                format!("failed to parse expression: {}", error),
            );
        }
        error
    }
}

/// Represent arguments of closure.
/// One can use it to represent variable binding from one scope to another.
#[derive(Clone, Debug)]
pub struct FnBinding {
    pub paren: Paren,
    pub inputs: Punctuated<Pat, Comma>,
}

// Copy - pasted from syn1 closure argument parsing
fn closure_arg(input: ParseStream) -> syn::Result<Pat> {
    let attrs = input.call(Attribute::parse_outer)?;
    let mut pat: Pat = Pat::parse_single(input)?;

    if input.peek(Token![:]) {
        Ok(Pat::Type(PatType {
            attrs,
            pat: Box::new(pat),
            colon_token: input.parse()?,
            ty: input.parse()?,
        }))
    } else {
        match &mut pat {
            Pat::Ident(pat) => pat.attrs = attrs,
            Pat::Lit(pat) => pat.attrs = attrs,
            Pat::Macro(pat) => pat.attrs = attrs,
            Pat::Or(pat) => pat.attrs = attrs,
            Pat::Path(pat) => pat.attrs = attrs,
            Pat::Range(pat) => pat.attrs = attrs,
            Pat::Reference(pat) => pat.attrs = attrs,
            Pat::Rest(pat) => pat.attrs = attrs,
            Pat::Slice(pat) => pat.attrs = attrs,
            Pat::Struct(pat) => pat.attrs = attrs,
            Pat::Tuple(pat) => pat.attrs = attrs,
            Pat::TupleStruct(pat) => pat.attrs = attrs,
            Pat::Type(_) => unreachable!("BUG: Type handled in if"),
            Pat::Verbatim(_) => {}
            Pat::Wild(pat) => pat.attrs = attrs,
            _ => unreachable!(),
        }
        Ok(pat)
    }
}

/// Sum type for Dyn and Keyed attributes.
///
/// Attributes is stored in opening tags.
#[derive(Clone, Debug, syn_derive::ToTokens)]
pub enum NodeAttribute {
    ///
    /// Element attribute that is computed from rust code block.
    ///
    /// Example:
    /// `<div {"some-fixed-key"}>` // attribute without value
    /// that is computed from string
    Block(NodeBlock),
    ///
    /// Element attribute with key, and possible value.
    /// Value is a valid Rust expression.
    ///
    /// Example:
    /// - `<div attr>`
    /// - `<div attr = value>`
    ///
    /// Value can be also in parens after key, but then it is parsed as closure
    /// arguments. Example:
    /// - `<div attr(x)>`
    /// - `<div attr(x: Type)>`
    Attribute(KeyedAttribute),
}

// Use custom parse to correct error.
impl Parse for KeyedAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let key = NodeName::parse(input)?;
        let possible_value = if input.peek(Paren) {
            KeyedAttributeValue::Binding(FnBinding::parse(input)?)
        } else if input.peek(Token![=]) {
            let eq = input.parse::<Token![=]>()?;
            if input.is_empty() {
                return Err(syn::Error::new(eq.span(), "missing attribute value"));
            }

            let fork = input.fork();
            let res = fork.parse::<Expr>().map_err(|e| {
                // if we stuck on end of input, span that is created will be call_site, so we
                // need to correct it, in order to make it more IDE friendly.
                if fork.is_empty() {
                    KeyedAttribute::correct_expr_error_span(e, input)
                } else {
                    e
                }
            })?;

            input.advance_to(&fork);
            KeyedAttributeValue::Value(AttributeValueExpr {
                token_eq: eq,
                value: res,
            })
        } else {
            KeyedAttributeValue::None
        };
        Ok(KeyedAttribute {
            key,
            possible_value,
        })
    }
}

impl ParseRecoverable for NodeAttribute {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        let node = if input.peek(Brace) {
            NodeAttribute::Block(parser.parse_recoverable(input)?)
        } else {
            NodeAttribute::Attribute(parser.parse_simple(input)?)
        };
        Some(node)
    }
}

impl Parse for FnBinding {
    fn parse(stream: ParseStream) -> syn::Result<Self> {
        let content;
        let paren = syn::parenthesized!(content in stream);
        let inputs = Punctuated::parse_terminated_with(&content, closure_arg)?;
        Ok(Self { paren, inputs })
    }
}

impl ToTokens for FnBinding {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.paren.surround(tokens, |tokens| {
            self.inputs.to_tokens(tokens);
        })
    }
}
