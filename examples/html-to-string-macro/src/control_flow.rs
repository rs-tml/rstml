use quote::{quote, ToTokens, TokenStreamExt};
use rstml::{
    atoms::{self, OpenTag, OpenTagEnd},
    node::{CustomNode, Node, NodeElement},
    recoverable::Recoverable,
    Parser, ParserConfig,
};
use syn::{
    parse::{Parse, ParseStream, Peek},
    parse_quote, Expr, Token,
};

// Either variant, with Parse/ToTokens implementation
#[derive(Copy, Clone, Debug, syn_derive::Parse, syn_derive::ToTokens)]
pub enum Either<A: Parse + ToTokens, B: Parse + ToTokens> {
    #[parse(peek_func = Either::<A,B>::peek_a)]
    A(A),
    B(B),
}

impl<A: Parse + ToTokens, B: Parse + ToTokens> Either<A, B> {
    fn peek_a(stream: ParseStream) -> bool {
        stream.parse::<A>().is_ok()
    }
    fn to_b(self) -> Option<B> {
        match self {
            Self::A(_) => None,
            Self::B(b) => Some(b),
        }
    }
    fn to_a(self) -> Option<A> {
        match self {
            Self::A(a) => Some(a),
            Self::B(_) => None,
        }
    }
    fn is_b(self) -> bool {
        match self {
            Self::A(_) => false,
            Self::B(_) => true,
        }
    }
    fn is_a(self) -> bool {
        match self {
            Self::A(_) => true,
            Self::B(_) => false,
        }
    }
}

/// End part of control flow tag ending
/// `/>` or `!>`
///
/// Rust expressions can contain `>` (compare operator),which makes harder
/// for parser to distinguish whenewer it part of expression or tag end.To
/// solve this issue, we enforce for controll flow tags to have `!>` at the end.
/// We also allow `/>` ending, to have self closed controll flow tags.
#[derive(Clone, Debug, syn_derive::Parse, syn_derive::ToTokens)]
pub struct ControlFlowTagEnd {
    pub self_close_marker: Either<Token![!], Token![/]>,
    pub token_gt: Token![>],
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
#[derive(Debug, syn_derive::ToTokens)]
struct If {
    token_lt: Token![<],
    token_if: Token![if],
    condition: Expr,
    open_tag_end: ControlFlowTagEnd,
    #[to_tokens(TokenStreamExt::append_all)]
    body: Vec<Node>,
    close_tag: Option<atoms::CloseTag>,
}

mod from_syn {
    use rstml::recoverable::ParseRecoverable;
    use syn::{parse::ParseBuffer, token::Brace, Expr};

    use super::*;
    #[derive(Clone, Debug, syn_derive::ToTokens, syn_derive::Parse)]
    pub struct ElseIf {
        pub else_token: Token![else],
        pub if_token: Token![if],
        pub cond: Expr,
        pub then_branch: NodesInBraces,
    }
    #[derive(Clone, Debug, syn_derive::ToTokens, syn_derive::Parse)]
    pub struct Else {
        pub else_token: Token![else],
        pub then_branch: NodesInBraces,
    }
    #[derive(Clone, Debug, syn_derive::ToTokens, syn_derive::Parse)]
    pub struct ExprIf {
        pub if_token: Token![if],
        pub cond: Expr,
        pub then_branch: NodesInBraces,
        pub else_ifs: Vec<ElseIf>,
        pub else_branch: Option<Else>,
    }

    #[derive(Clone, Debug, syn_derive::ToTokens, syn_derive::Parse)]
    pub struct NodesInBraces {
        #[syn(braced)]
        pub brace_token: Brace,

        #[syn(in = brace_token)]
        #[to_tokens(|tokens, val| tokens.append_all(val))]
        #[parse(parse_array)]
        pub nodes: Vec<Node>,
    }

    fn parse_array<T: ParseRecoverable>(buffer: ParseStream) -> Result<Vec<T>, syn::Error> {
        while !buffer.is_empty() {
            buffer.parse()
        }
    }
}

// struct Escape<Operation> {
//     token_at: Token![@],
// }

impl CustomNode for If {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        ToTokens::to_tokens(&self, tokens)
    }

    fn peek_element(input: syn::parse::ParseStream) -> bool {
        input.peek(Token![<]) && input.peek2(Token![if])
    }

    fn parse_element(
        parser: &mut rstml::recoverable::RecoverableContext,
        input: syn::parse::ParseStream,
    ) -> Option<Self> {
        let token_lt = OpenTag::parse_start_tag(parser, input)?;
        let token_if = parser.parse_simple(input)?;
        let (condition, open_tag_end): (_, ControlFlowTagEnd) =
            parser.parse_simple_with_ending(input)?;
        let (body, close_tag) = if open_tag_end.self_close_marker.is_a() {
            let pseudo_end_tag = OpenTagEnd {
                token_solidus: open_tag_end.self_close_marker.to_b(),
                token_gt: open_tag_end.token_gt,
            };
            let pseudo_open_tag = OpenTag {
                token_lt,
                name: parse_quote!(#token_if),
                generics: Default::default(),
                attributes: Default::default(),
                end_tag: pseudo_end_tag,
            };

            // Passed to allow parsing of close_tag
            NodeElement::parse_children(parser, input, false, &pseudo_open_tag)?
        } else {
            (Vec::new(), None)
        };
        Some(Self {
            token_lt,
            token_if,
            condition,
            open_tag_end,
            body,
            close_tag,
        })
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn custom_node_origin() {
        let actual: Recoverable<Node<If>> = parse_quote! {
            <if just && an || expression !>
                <a regular="html" component/>
                <div>
                </div>
            </if>
        };
        let Node::Custom(actual) = actual.inner() else {
            panic!()
        };

        assert_eq!(actual.condition, parse_quote!(just && an || expression));
    }

    #[test]
    fn custom_node_origin_cmp() {
        let actual: Recoverable<Node<If>> = parse_quote! {
            <if condition = {foo > bar} >
                <a regular="html" component/>
                <div>
                </div>
            </if>
        };

        let Node::Custom(actual) = actual.inner() else {
            panic!()
        };

        assert_eq!(actual.condition, parse_quote!(just && an || expression));
    }

    #[test]
    fn custom_node_using_config() {
        let actual = Parser::new(
            ParserConfig::new()
                .element_close_use_default_wildcard_ident(false)
                .custom_node::<If>(),
        )
        .parse_simple(quote! {
            if just && an || expression {
                <a regular="html" component/>
                <div>
                </div>
            }
        })
        .unwrap();
        let Node::Custom(actual) = &actual[0] else {
            panic!()
        };

        assert_eq!(actual.condition, parse_quote!(just && an || expression));
    }
}
