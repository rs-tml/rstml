use std::{
    convert::TryFrom,
    fmt::{self, Display},
};

use proc_macro2::Punct;
use syn::{
    ext::IdentExt,
    parse::{discouraged::Speculative, Parse, ParseStream, Peek},
    punctuated::{Pair, Punctuated},
    token::{Brace, Colon, Dot, PathSep},
    Block, ExprPath, Ident, LitInt, Path, PathSegment,
};

use super::{atoms::tokens::Dash, path_to_string};
use crate::{node::parse::block_expr, Error};

#[derive(Clone, Debug, syn_derive::Parse, syn_derive::ToTokens)]
pub enum NodeNameFragment {
    #[parse(peek = Ident::peek_any)]
    Ident(#[parse(Ident::parse_any)] Ident),
    #[parse(peek = LitInt)]
    Literal(LitInt),
    // In case when name contain more than one Punct in series
    Empty,
}
impl NodeNameFragment {
    fn peek_any(input: ParseStream) -> bool {
        input.peek(Ident::peek_any) || input.peek(LitInt)
    }
}

impl PartialEq<NodeNameFragment> for NodeNameFragment {
    fn eq(&self, other: &NodeNameFragment) -> bool {
        match (self, other) {
            (NodeNameFragment::Ident(s), NodeNameFragment::Ident(o)) => s == o,
            // compare literals by their string representation
            // So 0x00 and 0 is would be different literals.
            (NodeNameFragment::Literal(s), NodeNameFragment::Literal(o)) => {
                s.to_string() == o.to_string()
            }
            (NodeNameFragment::Empty, NodeNameFragment::Empty) => true,
            _ => false,
        }
    }
}
impl Eq for NodeNameFragment {}

impl Display for NodeNameFragment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeNameFragment::Ident(i) => i.fmt(f),
            NodeNameFragment::Literal(l) => l.fmt(f),
            NodeNameFragment::Empty => Ok(()),
        }
    }
}

/// Name of the node.
#[derive(Clone, Debug, syn_derive::ToTokens)]
pub enum NodeName {
    /// A plain identifier like `div` is a path of length 1, e.g. `<div />`. Can
    /// be separated by double colons, e.g. `<foo::bar />`.
    Path(ExprPath),

    ///
    /// Name separated by punctuation, e.g. `<div data-foo="bar" />` or `<div
    /// data:foo="bar" />`.
    ///
    /// It is fully compatible with SGML (ID/NAME) tokens format.
    /// Which is described as follow:
    /// ID and NAME tokens must begin with a letter ([A-Za-z]) and may be
    /// followed by any number of letters, digits ([0-9]), hyphens ("-"),
    /// underscores ("_"), colons (":"), and periods (".").
    ///
    /// Support more than one punctuation in series, in this case
    /// `NodeNameFragment::Empty` would be used.
    ///
    /// Note: that punct and `NodeNameFragment` has different `Spans` and IDE
    /// (rust-analyzer/idea) can controll them independently.
    /// So if one needs to add semantic highlight or go-to definition to entire
    /// `NodeName` it should emit helper statements for each `Punct` and
    /// `NodeNameFragment` (excludeing `Empty` fragment).
    Punctuated(Punctuated<NodeNameFragment, Punct>),

    /// Arbitrary rust code in braced `{}` blocks.
    Block(Block),
}

impl NodeName {
    /// Returns true if `NodeName` parsed as block of code.
    ///
    /// Example:
    /// {"Foo"}
    pub fn is_block(&self) -> bool {
        matches!(self, Self::Block(_))
    }

    /// Returns true if `NodeName` is dash seperated.
    ///
    /// Example:
    /// foo-bar
    pub fn is_dashed(&self) -> bool {
        match self {
            Self::Punctuated(p) => {
                let p = p.pairs().next().unwrap();
                p.punct().unwrap().as_char() == '-'
            }
            _ => false,
        }
    }

    /// Returns true if `NodeName` is wildcard ident.
    ///
    /// Example:
    /// _
    pub fn is_wildcard(&self) -> bool {
        match self {
            Self::Path(e) => {
                if e.path.segments.len() != 1 {
                    return false;
                }
                let Some(last_ident) = e.path.segments.last() else {
                    return false;
                };
                last_ident.ident == "_"
            }
            _ => false,
        }
    }

    /// Parse the stream as punctuated idents.
    ///
    /// We can't replace this with [`Punctuated::parse_separated_nonempty`]
    /// since that doesn't support reserved keywords. Might be worth to
    /// consider a PR upstream.
    ///
    /// [`Punctuated::parse_separated_nonempty`]: https://docs.rs/syn/1.0.58/syn/punctuated/struct.Punctuated.html#method.parse_separated_nonempty
    pub(crate) fn node_name_punctuated_ident<T: Parse, F: Peek, X: From<Ident>>(
        input: ParseStream,
        punct: F,
    ) -> syn::Result<Punctuated<X, T>> {
        let fork = &input.fork();
        let mut segments = Punctuated::<X, T>::new();

        while !fork.is_empty() && fork.peek(Ident::peek_any) {
            let ident = Ident::parse_any(fork)?;
            segments.push_value(ident.clone().into());

            if fork.peek(punct) {
                segments.push_punct(fork.parse()?);
            } else {
                break;
            }
        }

        if segments.len() > 1 {
            input.advance_to(fork);
            Ok(segments)
        } else {
            Err(fork.error("expected punctuated node name"))
        }
    }

    /// Parse the stream as punctuated idents, with two possible punctuations
    /// available
    pub(crate) fn node_name_punctuated_ident_with_two_alternate<
        T: Parse,
        F: Peek,
        G: Peek,
        H: Peek,
        X: From<NodeNameFragment>,
    >(
        input: ParseStream,
        punct: F,
        alternate_punct: G,
        alternate_punct2: H,
    ) -> syn::Result<Punctuated<X, T>> {
        let fork = &input.fork();
        let mut segments = Punctuated::<X, T>::new();

        while !fork.is_empty() && NodeNameFragment::peek_any(fork) {
            let ident = NodeNameFragment::parse(fork)?;
            segments.push_value(ident.clone().into());

            if fork.peek(punct) || fork.peek(alternate_punct) || fork.peek(alternate_punct2) {
                segments.push_punct(fork.parse()?);
            } else {
                break;
            }
        }

        if segments.len() > 1 {
            input.advance_to(fork);
            Ok(segments)
        } else {
            Err(fork.error("expected punctuated node name"))
        }
    }
}

impl TryFrom<&NodeName> for Block {
    type Error = Error;

    fn try_from(node: &NodeName) -> Result<Self, Self::Error> {
        match node {
            NodeName::Block(b) => Ok(b.to_owned()),
            _ => Err(Error::TryFrom(
                "NodeName does not match NodeName::Block(Expr::Block(_))".into(),
            )),
        }
    }
}

impl PartialEq for NodeName {
    fn eq(&self, other: &NodeName) -> bool {
        match self {
            Self::Path(this) => match other {
                Self::Path(other) => this == other,
                _ => false,
            },
            // can't be derived automatically because `Punct` doesn't impl `PartialEq`
            Self::Punctuated(this) => match other {
                Self::Punctuated(other) => {
                    this.pairs()
                        .zip(other.pairs())
                        .all(|(this, other)| match (this, other) {
                            (
                                Pair::Punctuated(this_ident, this_punct),
                                Pair::Punctuated(other_ident, other_punct),
                            ) => {
                                this_ident == other_ident
                                    && this_punct.as_char() == other_punct.as_char()
                            }
                            (Pair::End(this), Pair::End(other)) => this == other,
                            _ => false,
                        })
                }
                _ => false,
            },
            Self::Block(this) => match other {
                Self::Block(other) => this == other,
                _ => false,
            },
        }
    }
}

impl fmt::Display for NodeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                NodeName::Path(expr) => path_to_string(expr),
                NodeName::Punctuated(name) => {
                    name.pairs()
                        .flat_map(|pair| match pair {
                            Pair::Punctuated(ident, punct) => {
                                [ident.to_string(), punct.to_string()]
                            }
                            Pair::End(ident) => [ident.to_string(), "".to_string()],
                        })
                        .collect::<String>()
                }
                NodeName::Block(_) => String::from("{}"),
            }
        )
    }
}

impl Parse for NodeName {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(LitInt) {
            Err(syn::Error::new(
                input.span(),
                "Name must start with latin character",
            ))
        } else if input.peek2(PathSep) {
            NodeName::node_name_punctuated_ident::<PathSep, fn(_) -> PathSep, PathSegment>(
                input, PathSep,
            )
            .map(|segments| {
                NodeName::Path(ExprPath {
                    attrs: vec![],
                    qself: None,
                    path: Path {
                        leading_colon: None,
                        segments,
                    },
                })
            })
        } else if input.peek2(Colon) || input.peek2(Dash) || input.peek2(Dot) {
            NodeName::node_name_punctuated_ident_with_two_alternate::<
                Punct,
                fn(_) -> Colon,
                fn(_) -> Dash,
                fn(_) -> Dot,
                NodeNameFragment,
            >(input, Colon, Dash, Dot)
            .map(NodeName::Punctuated)
        } else if input.peek(Brace) {
            let fork = &input.fork();
            let value = block_expr(fork)?;
            input.advance_to(fork);
            Ok(NodeName::Block(value))
        } else if input.peek(Ident::peek_any) {
            let mut segments = Punctuated::new();
            let ident = Ident::parse_any(input)?;
            segments.push_value(PathSegment::from(ident));
            Ok(NodeName::Path(ExprPath {
                attrs: vec![],
                qself: None,
                path: Path {
                    leading_colon: None,
                    segments,
                },
            }))
        } else {
            Err(input.error("invalid tag name or attribute key"))
        }
    }
}
