use std::fmt::Debug;

use derive_where::derive_where;
use proc_macro2_diagnostics::Diagnostic;
use quote::{ToTokens, TokenStreamExt};
use rstml::{
    atoms::{self, CloseTagStart, OpenTag, OpenTagEnd},
    node::{CustomNode, Node as RNode, NodeElement},
    recoverable::{ParseRecoverable, RecoverableContext},
};
use syn::{
    parse::{Parse, ParseStream},
    parse_quote,
    spanned::Spanned,
    Expr, Pat, Token,
};

#[cfg(not(feature = "extendable"))]
type Node = RNode<Conditions>;

#[cfg(feature = "extendable")]
type Node = RNode<crate::ExtendableCustomNode>;

use super::Either;
#[cfg(feature = "extendable")]
use crate::ExtendableCustomNode;
use crate::TryIntoOrCloneRef;
/// End part of control flow tag ending
/// `/>` (self closed) or `!>` (regular, token `!` can be changed)
///
/// Rust expressions can contain `>` (compare operator), which makes harder
/// for parser to distinguish whenewer it part of expression or tag end.To
/// solve this issue, we enforce for controll flow tags to have `!>` at the end.
/// We also allow `/>` ending, to have self closed controll flow tags.
#[derive_where(Clone; EndToken: Clone)]
#[derive_where(Debug; EndToken: std::fmt::Debug)]
pub struct ControlFlowTagEnd<EndToken = Token![!]> {
    pub self_close_marker: Either<EndToken, Token![/]>,
    pub token_gt: Token![>],
}

impl<EndToken> Parse for ControlFlowTagEnd<EndToken>
where
    EndToken: Parse,
{
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let self_close_marker = if input.peek(Token![/]) {
            Either::B(input.parse()?)
        } else {
            Either::A(input.parse()?)
        };
        let token_gt = input.parse()?;
        Ok(Self {
            self_close_marker,
            token_gt,
        })
    }
}

impl<EndToken> ToTokens for ControlFlowTagEnd<EndToken>
where
    EndToken: ToTokens,
{
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.self_close_marker.to_tokens(tokens);
        self.token_gt.to_tokens(tokens);
    }
}

impl<EndToken> ControlFlowTagEnd<EndToken> {
    pub fn is_start(&self) -> bool {
        matches!(self.self_close_marker, Either::A(_))
    }
}

/// If construction:
/// instead of attributes receive rust expression that can evaluate to bool.
/// As any controll flow tag - should contain marker `!` at the end of tag, or
/// be self closed If expression returns true - process child nodes.
/// example:
/// `<if x > 2 !>`
///
/// As in rust can contain arbitrary amount of `<else if ... !>` constructs and
/// one `<else !>` at the end close tag is expected.
#[derive(syn_derive::ToTokens)]
// #[derive_where(Clone, Debug; EndToken: Clone + std::fmt::Debug, C: Clone + std::fmt::Debug)]
#[derive(Clone, Debug)]
pub struct IfNode {
    pub token_lt: Token![<],
    pub token_if: Token![if],
    pub condition: Expr,
    pub open_tag_end: ControlFlowTagEnd,
    #[to_tokens(TokenStreamExt::append_all)]
    pub body: Vec<Node>,
    #[to_tokens(TokenStreamExt::append_all)]
    pub else_ifs: Vec<ElseIfNode>,
    pub else_child: Option<ElseNode>,
    pub close_tag: Option<atoms::CloseTag>,
}

#[derive(syn_derive::ToTokens, Clone, Debug)]
pub struct ElseIfNode {
    pub token_lt: Token![<],
    pub token_else_if: ElseIfToken,
    pub condition: Expr,
    pub open_tag_end: ControlFlowTagEnd,
    #[to_tokens(TokenStreamExt::append_all)]
    pub body: Vec<Node>,
    pub close_tag: Option<ElseIfCloseTag>,
}

/// Close tag for element, `<name attr=x, attr_flag>`
#[derive(Clone, Debug, syn_derive::Parse, syn_derive::ToTokens)]
pub struct ElseIfCloseTag {
    pub start_tag: CloseTagStart,
    pub token_else_if: ElseIfToken,
    pub token_gt: Token![>],
}

impl ElseIfCloseTag {
    pub fn parse_with_start_tag(
        parser: &mut RecoverableContext,
        input: syn::parse::ParseStream,
        start_tag: Option<CloseTagStart>,
    ) -> Option<Self> {
        Some(Self {
            start_tag: start_tag?,
            token_else_if: parser.parse_simple(input)?,
            token_gt: parser.parse_simple(input)?,
        })
    }
}

#[derive(syn_derive::ToTokens, Clone, Debug)]
pub struct ElseNode {
    pub token_lt: Token![<],
    pub token_else: Token![else],
    // Use same type as in if
    pub open_tag_end: OpenTagEnd,
    #[to_tokens(TokenStreamExt::append_all)]
    pub body: Vec<Node>,
    pub close_tag: Option<atoms::CloseTag>,
}

#[derive(Clone, Debug, syn_derive::Parse, syn_derive::ToTokens)]
pub struct ElseIfToken {
    pub token_else: Token![else],
    pub token_if: Token![if],
}

#[derive(syn_derive::ToTokens, Clone, Debug)]
pub struct ForNode {
    pub token_lt: Token![<],
    pub token_for: Token![for],
    pub pat: Pat,
    pub token_in: Token![in],
    pub expr: Expr,
    pub open_tag_end: ControlFlowTagEnd,
    #[to_tokens(TokenStreamExt::append_all)]
    pub body: Vec<Node>,
    pub close_tag: Option<atoms::CloseTag>,
}

impl ParseRecoverable for ForNode {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        let token_lt = OpenTag::parse_start_tag(parser, input)?;
        let token_for = parser.parse_simple(input)?;
        let pat = parser.parse_mixed_fn(input, |_parse, input| {
            Pat::parse_multi_with_leading_vert(input)
        })?;
        let token_in = parser.parse_simple(input)?;
        let (expr, open_tag_end): (_, ControlFlowTagEnd) = parser.parse_simple_until(input)?;

        let (body, close_tag) = if open_tag_end.is_start() {
            // If node is not raw use any closing tag as separator, to early report about
            // invalid closing tags.
            // Also parse only </ part to recover parser as soon as user types </
            let (children, close_tag) =
                parser.parse_tokens_until_call::<Node, _, _>(input, CloseTagStart::parse);

            let close_tag = atoms::CloseTag::parse_with_start_tag(parser, input, close_tag);
            (children, close_tag)
        } else {
            (Vec::new(), None)
        };
        Some(Self {
            token_lt,
            token_for,
            pat,
            token_in,
            expr,
            open_tag_end,
            body,
            close_tag,
        })
    }
}

impl ParseRecoverable for ElseIfNode {
    fn parse_recoverable(
        parser: &mut rstml::recoverable::RecoverableContext,
        input: ParseStream,
    ) -> Option<Self> {
        let token_lt = OpenTag::parse_start_tag(parser, input)?;
        let token_else_if = parser.parse_simple(input)?;
        let (condition, open_tag_end): (_, ControlFlowTagEnd) = parser.parse_simple_until(input)?;

        let (body, close_tag) = if open_tag_end.is_start() {
            // If node is not raw use any closing tag as separator, to early report about
            // invalid closing tags.
            // Also parse only </ part to recover parser as soon as user types </
            let (children, close_tag) =
                parser.parse_tokens_until_call::<Node, _, _>(input, CloseTagStart::parse);

            let close_tag = ElseIfCloseTag::parse_with_start_tag(parser, input, close_tag);
            (children, close_tag)
        } else {
            (Vec::new(), None)
        };
        Some(Self {
            token_lt,
            token_else_if,
            condition,
            open_tag_end,
            body,
            close_tag,
        })
    }
}

impl ParseRecoverable for ElseNode {
    fn parse_recoverable(
        parser: &mut rstml::recoverable::RecoverableContext,
        input: ParseStream,
    ) -> Option<Self> {
        let token_lt = OpenTag::parse_start_tag(parser, input)?;
        let token_else = parser.parse_simple(input)?;
        let open_tag_end: OpenTagEnd = parser.parse_simple(input)?;

        let (body, close_tag) = if open_tag_end.token_solidus.is_none() {
            let open_tag_end = OpenTagEnd {
                token_gt: open_tag_end.token_gt,
                token_solidus: None,
            };
            // Passed to allow parsing of close_tag
            NodeElement::parse_children(
                parser,
                input,
                false,
                &OpenTag {
                    token_lt,
                    name: parse_quote!(#token_else),
                    generics: Default::default(),
                    attributes: Default::default(),
                    end_tag: open_tag_end,
                },
            )?
        } else {
            (Vec::new(), None)
        };
        Some(Self {
            token_lt,
            token_else,
            open_tag_end,
            body,
            close_tag,
        })
    }
}

#[cfg(feature = "extendable")]
impl TryIntoOrCloneRef<Conditions> for ExtendableCustomNode {
    fn try_into_or_clone_ref(self) -> Either<Conditions, Self> {
        let Some(ref_val) = self.try_downcast_ref::<Conditions>() else {
            return Either::B(self);
        };
        Either::A(ref_val.clone())
    }
    fn new_from_value(value: Conditions) -> Self {
        ExtendableCustomNode::from_value(value)
    }
}

// A lot of bounds o§n type definition require a lot of bounds in type
// declaration.
impl ParseRecoverable for IfNode {
    // Parse as regular element,
    // then ensure that else if and else are in correct order
    // in the way move all else ifs and elses to separate fields.
    fn parse_recoverable(
        parser: &mut rstml::recoverable::RecoverableContext,
        input: ParseStream,
    ) -> Option<Self> {
        let token_lt = OpenTag::parse_start_tag(parser, input)?;
        let token_if = parser.parse_simple(input)?;
        let (condition, open_tag_end): (_, ControlFlowTagEnd) = parser.parse_simple_until(input)?;

        let open_tag_end_ = OpenTagEnd {
            token_gt: open_tag_end.token_gt,
            token_solidus: None,
        };
        let (body, close_tag) = NodeElement::parse_children(
            parser,
            input,
            false,
            &OpenTag {
                token_lt,
                name: parse_quote!(#token_if),
                generics: Default::default(),
                attributes: Default::default(),
                end_tag: open_tag_end_,
            },
        )?;
        let mut else_ifs = Vec::new();
        let mut else_child = None;
        let mut modified_body = Vec::new();
        for el in body {
            let rest = match el {
                Node::Custom(c) => match c.try_into_or_clone_ref() {
                    Either::A(Conditions::ElseIf(else_if)) => {
                        if else_child.is_some() {
                            parser.push_diagnostic(Diagnostic::spanned(
                                else_if.span(),
                                proc_macro2_diagnostics::Level::Error,
                                "else if after else",
                            ));
                            return None;
                        }
                        else_ifs.push(else_if);
                        continue;
                    }
                    Either::A(Conditions::Else(else_)) => {
                        else_child = Some(else_);
                        continue;
                    }
                    Either::A(condition) => {
                        Node::Custom(TryIntoOrCloneRef::new_from_value(condition))
                    }
                    Either::B(rest) => Node::Custom(rest),
                },
                _ => el,
            };
            modified_body.push(rest);
        }

        Some(Self {
            token_lt,
            token_if,
            condition,
            open_tag_end,
            body: modified_body,
            else_ifs,
            else_child,
            close_tag,
        })
    }
}

/// Conditions can be either if, else if, else, match or for
/// Make sure to check is_highlevel before using, to avoid toplevel else/else if
/// nodes.
#[derive(Clone, Debug)]
pub enum Conditions {
    ElseIf(ElseIfNode),
    Else(ElseNode),
    If(IfNode),
    For(ForNode),
    // Match(Match<Conditions>),
}

impl Conditions {
    pub fn is_highlevel(&self) -> bool {
        match self {
            Self::If(_) | Self::For(_) => true,
            _ => false,
        }
    }
}

impl ToTokens for Conditions {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::ElseIf(else_if) => else_if.to_tokens(tokens),
            Self::Else(else_) => else_.to_tokens(tokens),
            Self::If(if_) => if_.to_tokens(tokens),
            Self::For(for_) => for_.to_tokens(tokens),
        }
    }
}

impl ParseRecoverable for Conditions {
    fn parse_recoverable(
        parser: &mut rstml::recoverable::RecoverableContext,
        input: ParseStream,
    ) -> Option<Self> {
        let variants = if input.peek2(Token![if]) {
            let if_ = IfNode::parse_recoverable(parser, input)?;
            Self::If(if_)
        } else if input.peek2(Token![else]) {
            if input.peek3(Token![if]) {
                let else_if = ElseIfNode::parse_recoverable(parser, input)?;
                Self::ElseIf(else_if)
            } else {
                let else_ = ElseNode::parse_recoverable(parser, input)?;
                Self::Else(else_)
            }
        } else if input.peek2(Token![for]) {
            let for_ = ForNode::parse_recoverable(parser, input)?;
            Self::For(for_)
        } else {
            return None;
        };
        Some(variants)
    }
}

impl CustomNode for Conditions {
    fn peek_element(input: syn::parse::ParseStream) -> bool {
        input.peek(Token![<])
            && (input.peek2(Token![else]) || input.peek2(Token![if]) || input.peek2(Token![for]))
    }
}

#[cfg(not(feature = "extendable"))]
#[cfg(test)]
mod test {
    use quote::quote;
    use rstml::{node::Node, recoverable::Recoverable, Parser, ParserConfig};
    use syn::parse_quote;

    use super::ControlFlowTagEnd;
    use crate::tags::Conditions;

    #[test]
    fn control_flow_tag_end_parsable() {
        let _tokens: ControlFlowTagEnd = parse_quote! {
            !>
        };

        let _tokens: ControlFlowTagEnd = parse_quote! {
            />
        };
    }
    #[test]
    fn custom_node_origin() {
        let actual: Recoverable<Node<Conditions>> = parse_quote! {
            <if just && an || expression !>
                <a regular="html" component/>
                <div>
                </div>
            </if>
        };
        let Node::Custom(Conditions::If(actual)) = actual.inner() else {
            panic!()
        };

        assert_eq!(actual.condition, parse_quote!(just && an || expression));
    }

    #[test]
    fn custom_node_origin_cmp() {
        let actual: Recoverable<Node<Conditions>> = parse_quote! {
            <if foo > bar !>
                <a regular="html" component/>
                <div>
                </div>
            </if>
        };

        let Node::Custom(Conditions::If(actual)) = actual.inner() else {
            panic!()
        };

        assert_eq!(actual.condition, parse_quote!(foo > bar));
    }

    #[test]
    fn custom_node_using_config() {
        let actual = Parser::new(
            ParserConfig::new()
                .element_close_use_default_wildcard_ident(false)
                .custom_node::<Conditions>(),
        )
        .parse_simple(quote! {
            <if foo > bar !>
                <a regular="html" component/>
                <div>
                </div>
            </if>
        })
        .unwrap();
        let Node::Custom(Conditions::If(actual)) = &actual[0] else {
            panic!()
        };

        assert_eq!(actual.condition, parse_quote!(foo > bar));
    }

    #[test]
    fn custom_node_if_else() {
        let actual: Recoverable<Node<Conditions>> = parse_quote! {
            <if foo > bar !>
                <a regular="html" component/>
                <div>
                </div>
                <else>
                    <a regular="html" component/>
                    <div>
                    </div>
                </else>
            </if>
        };
        let Node::Custom(Conditions::If(actual)) = actual.inner() else {
            panic!()
        };

        assert_eq!(actual.condition, parse_quote!(foo > bar));
    }
    #[test]
    fn custom_node_else_ifs() {
        let actual: Recoverable<Node<Conditions>> = parse_quote! {
                <else if foo < bar !>
                   "foo < bar"
                </else if>
        };
        let Node::Custom(Conditions::ElseIf(actual)) = actual.inner() else {
            panic!()
        };

        assert_eq!(actual.condition, parse_quote!(foo < bar));
    }

    #[test]
    fn custom_node_if_else_ifs() {
        let actual: Recoverable<Node<Conditions>> = parse_quote! {
            <if foo > bar !>
                "foo > bar"
                <else if foo < bar !>
                   "foo < bar"
                </else if>
                <else>
                    "foo == bar"
                </else>
            </if>
        };
        let Node::Custom(Conditions::If(actual)) = actual.inner() else {
            panic!()
        };

        assert_eq!(actual.condition, parse_quote!(foo > bar));
    }

    #[test]
    fn constom_node_for() {
        let actual: Recoverable<Node<Conditions>> = parse_quote! {
            <for x in 0..3 !>
                <a regular="html" component/>
                "value is:" {x}
            </for>
        };

        let Node::Custom(Conditions::For(actual)) = actual.inner() else {
            panic!()
        };

        assert_eq!(actual.pat, parse_quote!(x));
    }
}
