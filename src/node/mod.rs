//! Tree of nodes.

use std::{convert, fmt};

use atoms::{tokens, FragmentClose, FragmentOpen};
use proc_macro2::{Ident, TokenStream};
use quote::ToTokens;
use syn::{parse::ParseStream, ExprPath, LitStr, Token};

pub mod atoms;
mod attribute;
mod node_name;
mod node_value;
pub mod parse;
mod parser_ext;
mod raw_text;

pub use attribute::{
    AttributeValueExpr, FnBinding, KeyedAttribute, KeyedAttributeValue, NodeAttribute,
};
pub use node_name::{NodeName, NodeNameFragment};
pub use node_value::{InvalidBlock, NodeBlock};

pub use self::raw_text::RawText;
use crate::recoverable::{RecoverableContext, ParseRecoverable};

/// Node types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeType {
    Element,
    Text,
    Comment,
    Doctype,
    Block,
    Fragment,
    RawText,
    Custom,
}

impl fmt::Display for NodeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Element => "NodeType::Element",
                Self::Text => "NodeType::Text",
                Self::RawText => "NodeType::RawText",
                Self::Comment => "NodeType::Comment",
                Self::Doctype => "NodeType::Doctype",
                Self::Block => "NodeType::Block",
                Self::Fragment => "NodeType::Fragment",
                Self::Custom => "NodeType::Custom",
            }
        )
    }
}

/// Node in the tree.
#[derive(Clone, Debug)]
pub enum Node<C = Infallible> {
    Comment(NodeComment),
    Doctype(NodeDoctype),
    Fragment(NodeFragment<C>),
    Element(NodeElement<C>),
    Block(NodeBlock),
    Text(NodeText),
    RawText(RawText<C>),
    Custom(C),
}
// Manual implementation, because derive macro doesn't support generics.
impl<C: CustomNode> ToTokens for Node<C> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Comment(comment) => comment.to_tokens(tokens),
            Self::Doctype(doctype) => doctype.to_tokens(tokens),
            Self::Fragment(fragment) => fragment.to_tokens(tokens),
            Self::Element(element) => element.to_tokens(tokens),
            Self::Block(block) => block.to_tokens(tokens),
            Self::Text(text) => text.to_tokens(tokens),
            Self::RawText(raw_text) => raw_text.to_tokens(tokens),
            Self::Custom(custom) => custom.to_tokens(tokens),
        }
    }
}

impl<C: CustomNode> Node<C> {
    pub fn flatten(mut self) -> Vec<Self> {
        let children = self
            .children_mut()
            .map(|children| children.drain(..))
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();

        std::iter::once(self)
            .chain(children.into_iter().flat_map(Self::flatten))
            .collect()
    }
    /// Get the type of the node.
    pub fn r#type(&self) -> NodeType {
        match &self {
            Self::Element(_) => NodeType::Element,
            Self::Text(_) => NodeType::Text,
            Self::Comment(_) => NodeType::Comment,
            Self::Doctype(_) => NodeType::Element,
            Self::Block(_) => NodeType::Block,
            Self::Fragment(_) => NodeType::Fragment,
            Self::RawText(_) => NodeType::RawText,
            Self::Custom(_) => NodeType::Custom,
        }
    }

    /// Get node children.
    pub fn children(&self) -> Option<&Vec<Self>> {
        match self {
            Self::Fragment(NodeFragment { children, .. })
            | Self::Element(NodeElement { children, .. }) => Some(children),
            _ => None,
        }
    }

    /// Get mutable node children.
    pub fn children_mut(&mut self) -> Option<&mut Vec<Self>> {
        match self {
            Self::Fragment(NodeFragment { children, .. })
            | Self::Element(NodeElement { children, .. }) => Some(children),
            _ => None,
        }
    }
}

/// Element node.
///
/// A HTMLElement tag, with optional children and attributes.
/// Potentially selfclosing. Any tag name is valid.
#[derive(Clone, Debug)]
pub struct NodeElement<C> {
    pub open_tag: atoms::OpenTag,
    pub children: Vec<Node<C>>,
    pub close_tag: Option<atoms::CloseTag>,
}
// Manual implementation, because derive macro doesn't support generics.
impl<C: CustomNode> ToTokens for NodeElement<C> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.open_tag.to_tokens(tokens);
        for child in &self.children {
            child.to_tokens(tokens);
        }
        if let Some(close_tag) = &self.close_tag {
            close_tag.to_tokens(tokens);
        }
    }
}

impl<C: CustomNode> NodeElement<C> {
    pub fn name(&self) -> &NodeName {
        &self.open_tag.name
    }
    pub fn attributes(&self) -> &[NodeAttribute] {
        &self.open_tag.attributes
    }
    pub fn attributes_mut(&mut self) -> &mut Vec<NodeAttribute> {
        &mut self.open_tag.attributes
    }
    pub fn chidlren(&self) -> &[Node<C>] {
        &self.children
    }
    pub fn children_mut(&mut self) -> &mut Vec<Node<C>> {
        &mut self.children
    }
}

/// Text node.
///
/// Quoted text. Unquoted can be found in `RawText`.
#[derive(Clone, Debug, syn_derive::Parse, syn_derive::ToTokens)]
pub struct NodeText {
    /// The text value.
    pub value: LitStr,
}

impl NodeText {
    /// Returns value of inner LitStr
    pub fn value_string(&self) -> String {
        self.value.value()
    }
}

/// Comment node.
///
/// Comment: `<!-- "comment" -->`, currently has the same restrictions as
/// `Text` (comment needs to be quoted).
#[derive(Clone, Debug, syn_derive::Parse, syn_derive::ToTokens)]
pub struct NodeComment {
    pub token_start: tokens::ComStart,
    /// The comment value.
    pub value: LitStr,
    pub token_end: tokens::ComEnd,
}
/// Doctype node.
///
/// Doctype declaration: `<!DOCTYPE html>` (case insensitive), `html` is the
/// node value in this case.
/// Usually doctype only contaim html, but also can contain arbitrary DOCTYPE
/// legacy string, or "obsolete permitted DOCTYPE string", therewhy value is
/// RawText.
#[derive(Clone, Debug, syn_derive::ToTokens)]
pub struct NodeDoctype {
    pub token_start: tokens::DocStart,
    /// "doctype"
    pub token_doctype: Ident,
    /// The doctype value.
    pub value: RawText,
    pub token_end: Token![>],
}

/// Fragement node.
///
/// Fragment: `<></>`
#[derive(Clone, Debug)]
pub struct NodeFragment<C> {
    /// Open fragment token
    pub tag_open: FragmentOpen,
    /// Children of the fragment node.
    pub children: Vec<Node<C>>,
    /// Close fragment token
    pub tag_close: Option<FragmentClose>,
}
// Manual implementation, because derive macro doesn't support generics.
impl<C: CustomNode> ToTokens for NodeFragment<C> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.tag_open.to_tokens(tokens);
        for child in &self.children {
            child.to_tokens(tokens);
        }
        if let Some(close_tag) = &self.tag_close {
            close_tag.to_tokens(tokens);
        }
    }
}

impl<C> NodeFragment<C> {
    pub fn children(&self) -> &[Node<C>] {
        &self.children
    }
    pub fn children_mut(&mut self) -> &mut Vec<Node<C>> {
        &mut self.children
    }
}

fn path_to_string(expr: &ExprPath) -> String {
    expr.path
        .segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .collect::<Vec<String>>()
        .join("::")
}

pub trait CustomNode: ParseRecoverable + ToTokens {
    /// Peeks the token stream to decide whether this node should be parsed.
    ///
    /// Recieves a [`ParseStream::fork`].
    ///
    /// [`ParseStream::fork`]: syn::parse::ParseBuffer::fork
    /// 
    fn peek_element(input: ParseStream) -> bool;
}

/// Newtype for `std::convert::Infallible` used to implement
/// `ToTokens`` for `Infallible``.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Infallible (convert::Infallible);

impl From<convert::Infallible> for Infallible {
    fn from(s: convert::Infallible) -> Self {
        match s {}
    }
}
impl ToTokens for Infallible {
    fn to_tokens(&self, _tokens: &mut TokenStream) {
        match self.0 {}
    }
}
impl ParseRecoverable for Infallible {
    fn parse_recoverable(_: &mut RecoverableContext, _: ParseStream) -> Option<Self> {
        unreachable!("Infallible::peek_element returns false")
    }
}
impl CustomNode for Infallible {
    fn peek_element( _: ParseStream) -> bool {
        false
    }
}