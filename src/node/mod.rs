//! Tree of nodes.

use std::{convert::Infallible, fmt};

use atoms::{tokens, FragmentClose, FragmentOpen};
use proc_macro2::{Ident, TokenStream};
use syn::{parse::ParseStream, ExprPath, LitStr, Token};

pub mod atoms;
mod attribute;
mod node_name;
mod node_value;
pub mod parse;
mod raw_text;

pub use attribute::{
    AttributeValueExpr, FnBinding, KeyedAttribute, KeyedAttributeValue, NodeAttribute,
};
pub use node_name::{NodeName, NodeNameFragment};
pub use node_value::NodeBlock;

pub use self::raw_text::RawText;
use crate::recoverable::RecoverableContext;

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
#[derive(Clone, Debug, syn_derive::ToTokens)]
pub enum Node<C: CustomNode = Infallible> {
    Comment(NodeComment),
    Doctype(NodeDoctype),
    Fragment(NodeFragment<C>),
    Element(NodeElement<C>),
    Block(NodeBlock),
    Text(NodeText),
    RawText(RawText),
    Custom(C),
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
#[derive(Clone, Debug, syn_derive::ToTokens)]
pub struct NodeElement<C: CustomNode = Infallible> {
    pub open_tag: atoms::OpenTag,
    #[to_tokens(parse::to_tokens_array)]
    pub children: Vec<Node<C>>,
    pub close_tag: Option<atoms::CloseTag>,
}

impl<C: CustomNode> NodeElement<C> {
    pub fn name(&self) -> &NodeName {
        &self.open_tag.name
    }
    pub fn attributes(&self) -> &[NodeAttribute] {
        &self.open_tag.attributes
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
#[derive(Clone, Debug, syn_derive::ToTokens)]
pub struct NodeFragment<C: CustomNode = Infallible> {
    /// Open fragment token
    pub tag_open: FragmentOpen,
    /// Children of the fragment node.
    #[to_tokens(parse::to_tokens_array)]
    pub children: Vec<Node<C>>,
    /// Close fragment token
    pub tag_close: Option<FragmentClose>,
}

fn path_to_string(expr: &ExprPath) -> String {
    expr.path
        .segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .collect::<Vec<String>>()
        .join("::")
}

pub trait CustomNode: Sized {
    fn to_tokens(&self, tokens: &mut TokenStream);
    fn peek_element(input: ParseStream) -> bool;
    fn parse_element(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self>;
}

impl CustomNode for Infallible {
    fn to_tokens(&self, _tokens: &mut TokenStream) {
        match *self {}
    }

    fn peek_element(_input: ParseStream) -> bool {
        false
    }

    fn parse_element(_parser: &mut RecoverableContext, _input: ParseStream) -> Option<Self> {
        unreachable!("Infallible::peek_element returns false")
    }
}
