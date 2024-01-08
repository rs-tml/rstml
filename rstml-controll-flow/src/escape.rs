//!
//! This module contain implementation of flow-controll based on rstml custom
//! nodes.
//!
//! This is usefull when one need to define optional or repetetive html(*ml)
//! elements. It contain implementation of If, Match and For constructions that
//! is very simmilar to rust. The main difference in implementation is that body
//! of construction (part inside curly brackets) is expected to be an html(*ml),
//! but condition is expected to be in rust syntax.

use quote::{ToTokens, TokenStreamExt};
use rstml::{
    node::{CustomNode, Node as RNode},
    recoverable::{ParseRecoverable, RecoverableContext},
    visitor::Visitor,
};
use syn::{
    braced,
    parse::{Parse, ParseStream},
    token::Brace,
    visit_mut::VisitMut,
    Expr, Pat, Token,
};

use crate::{Either, TryIntoOrCloneRef};

#[cfg(not(feature = "extendable"))]
type CustomNodeType = EscapeCode;

#[cfg(feature = "extendable")]
type CustomNodeType = crate::ExtendableCustomNode;

type Node = RNode<CustomNodeType>;

#[derive(Clone, Debug, syn_derive::ToTokens)]
pub struct Block {
    #[syn(braced)]
    pub brace_token: Brace,
    #[syn(in = brace_token)]
    #[to_tokens(|tokens, val| tokens.append_all(val))]
    pub body: Vec<Node>,
}

impl ParseRecoverable for Block {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        // we use this closure, because `braced!`
        // has private api and force it's usage inside methods that return Result
        let inner_parser = |parser: &mut RecoverableContext, input: ParseStream| {
            let content;
            let brace_token = braced!(content in input);
            let mut body = vec![];
            while !content.is_empty() {
                let Some(val) = parser.parse_recoverable(&content) else {
                    return Ok(None);
                };
                body.push(val);
            }
            Ok(Some(Block { brace_token, body }))
        };
        parser.parse_mixed_fn(input, inner_parser)?
    }
}

#[derive(Clone, Debug, syn_derive::ToTokens)]
pub struct ElseIf {
    pub else_token: Token![else],
    pub if_token: Token![if],
    pub condition: Expr,
    pub then_branch: Block,
}

impl ParseRecoverable for ElseIf {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        Some(ElseIf {
            else_token: parser.parse_simple(input)?,
            if_token: parser.parse_simple(input)?,
            condition: parser.parse_mixed_fn(input, |_, input| {
                input.call(Expr::parse_without_eager_brace)
            })?,
            then_branch: parser.parse_recoverable(input)?,
        })
    }
}

#[derive(Clone, Debug, syn_derive::ToTokens)]
pub struct Else {
    pub else_token: Token![else],
    pub then_branch: Block,
}
impl ParseRecoverable for Else {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        Some(Else {
            else_token: parser.parse_simple(input)?,
            then_branch: parser.parse_recoverable(input)?,
        })
    }
}

/// If construction:
/// Condition is any valid rust syntax, while body is expected yo be *ml
/// element.
///
/// Example:
/// `@if x > 2 { <div></div> }`
/// `@if let Some(x) = foo { <div></div> }`
///
/// As in rust can contain arbitrary amount of `else if .. {..}` constructs and
/// one `else {..}`.
#[derive(Clone, Debug, syn_derive::ToTokens)]
pub struct IfExpr {
    pub keyword: Token![if],
    pub condition: Expr,
    pub then_branch: Block,
    #[to_tokens(TokenStreamExt::append_all)]
    pub else_ifs: Vec<ElseIf>,
    pub else_branch: Option<Else>,
}

impl ParseRecoverable for IfExpr {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        let keyword = parser.parse_simple(input)?;

        let condition = parser.parse_mixed_fn(input, |_, input| {
            input.call(Expr::parse_without_eager_brace)
        })?;

        let then_branch = parser.parse_recoverable(input)?;
        let mut else_ifs = vec![];

        while input.peek(Token![else]) && input.peek2(Token![if]) {
            else_ifs.push(parser.parse_recoverable(input)?)
        }
        let mut else_branch = None;
        if input.peek(Token![else]) {
            else_branch = Some(parser.parse_recoverable(input)?)
        }
        Some(IfExpr {
            keyword,
            condition,
            then_branch,
            else_ifs,
            else_branch,
        })
    }
}

#[derive(Clone, Debug, syn_derive::ToTokens)]
pub struct ForExpr {
    pub keyword: Token![for],
    pub pat: Pat,
    pub token_in: Token![in],
    pub expr: Expr,
    pub block: Block,
}

impl ParseRecoverable for ForExpr {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        let keyword = parser.parse_simple(input)?;
        let pat = parser.parse_mixed_fn(input, |_parse, input| {
            Pat::parse_multi_with_leading_vert(input)
        })?;
        let token_in = parser.parse_simple(input)?;
        let expr = parser.parse_mixed_fn(input, |_, input| {
            input.call(Expr::parse_without_eager_brace)
        })?;
        let block = parser.parse_recoverable(input)?;
        Some(ForExpr {
            keyword,
            pat,
            token_in,
            expr,
            block,
        })
    }
}

#[derive(Clone, Debug, syn_derive::ToTokens)]
pub struct Arm {
    pub pat: Pat,
    // pub guard: Option<(If, Box<Expr>)>,
    pub fat_arrow_token: Token![=>],
    pub body: Block,
    pub comma: Option<Token![,]>,
}

impl ParseRecoverable for Arm {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        Some(Arm {
            pat: parser
                .parse_mixed_fn(input, |_, input| Pat::parse_multi_with_leading_vert(input))?,
            fat_arrow_token: parser.parse_simple(input)?,
            body: parser.parse_recoverable(input)?,
            comma: parser.parse_simple(input)?,
        })
    }
}

// match foo {
//     1|3 => {},
//     x if x > 10 => {},
//     | x => {}
// }

#[derive(Clone, Debug, syn_derive::ToTokens)]
pub struct MatchExpr {
    pub keyword: Token![match],
    pub expr: Expr,
    #[syn(braced)]
    pub brace_token: Brace,
    #[syn(in = brace_token)]
    #[to_tokens(TokenStreamExt::append_all)]
    pub arms: Vec<Arm>,
}

impl ParseRecoverable for MatchExpr {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        // we use this closure, because `braced!`
        // has private api and force it's usage inside methods that return Result
        let inner_parser = |parser: &mut RecoverableContext, input: ParseStream| {
            let Some(keyword) = parser.parse_simple(input) else {
                return Ok(None);
            };
            let content;
            let expr = Expr::parse_without_eager_brace(input)?;
            let brace_token = braced!(content in input);
            let mut arms = Vec::new();
            while !content.is_empty() {
                let Some(val) = parser.parse_recoverable(&content) else {
                    return Ok(None);
                };
                arms.push(val);
            }
            Ok(Some(MatchExpr {
                keyword,
                expr,
                brace_token,
                arms,
            }))
        };
        parser.parse_mixed_fn(input, inner_parser)?
    }
}

// Minimal version of syn::Expr, that uses custom `Block` with `Node` array
// instead of `syn::Block` that contain valid rust code.

#[derive(Clone, Debug, syn_derive::ToTokens)]
pub enum EscapedExpr {
    If(IfExpr),
    For(ForExpr),
    Match(MatchExpr),
}

impl ParseRecoverable for EscapedExpr {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        let res = if input.peek(Token![if]) {
            EscapedExpr::If(parser.parse_recoverable(input)?)
        } else if input.peek(Token![for]) {
            EscapedExpr::For(parser.parse_recoverable(input)?)
        } else if input.peek(Token![match]) {
            EscapedExpr::Match(parser.parse_recoverable(input)?)
        } else {
            return None;
        };

        Some(res)
    }
}
#[cfg(feature = "extendable")]
impl TryIntoOrCloneRef<EscapeCode> for crate::ExtendableCustomNode {
    fn try_into_or_clone_ref(self) -> Either<EscapeCode, Self> {
        if let Some(val) = self.try_downcast_ref::<EscapeCode>() {
            Either::A(val.clone())
        } else {
            Either::B(self.clone())
        }
    }
    fn new_from_value(value: EscapeCode) -> Self {
        Self::from_value(value)
    }
}

#[derive(Clone, Debug, syn_derive::ToTokens)]
pub struct EscapeCode<T: ToTokens = Token![@]> {
    pub escape_token: T,
    pub expression: EscapedExpr,
}

impl<T: ToTokens + Parse> ParseRecoverable for EscapeCode<T> {
    fn parse_recoverable(parser: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        let escape_token = parser.parse_simple(input)?;
        let expression = parser.parse_recoverable(input)?;

        Some(Self {
            escape_token,
            expression,
        })
    }
}

impl<T> CustomNode for EscapeCode<T>
where
    T: Parse + ToTokens,
{
    fn peek_element(input: syn::parse::ParseStream) -> bool {
        if input.parse::<T>().is_err() {
            return false;
        }
        input.peek(Token![if]) || input.peek(Token![for]) || input.peek(Token![match])
    }
}

pub mod visitor_impl {
    use super::*;

    impl Block {
        pub fn visit_custom_children<V: Visitor<Custom = CustomNodeType> + VisitMut>(
            visitor: &mut V,
            block: &mut Self,
        ) -> bool {
            block.body.iter_mut().all(|val| visitor.visit_node(val))
        }
    }

    impl ElseIf {
        pub fn visit_custom_children<V: Visitor<Custom = CustomNodeType> + VisitMut>(
            visitor: &mut V,
            expr: &mut Self,
        ) -> bool {
            visitor.visit_expr_mut(&mut expr.condition);
            Block::visit_custom_children(visitor, &mut expr.then_branch)
        }
    }

    impl Else {
        pub fn visit_custom_children<V: Visitor<Custom = CustomNodeType> + VisitMut>(
            visitor: &mut V,
            expr: &mut Self,
        ) -> bool {
            Block::visit_custom_children(visitor, &mut expr.then_branch)
        }
    }

    impl IfExpr {
        pub fn visit_custom_children<V: Visitor<Custom = CustomNodeType> + VisitMut>(
            visitor: &mut V,
            expr: &mut Self,
        ) -> bool {
            visitor.visit_expr_mut(&mut expr.condition);
            Block::visit_custom_children(visitor, &mut expr.then_branch)
                || expr
                    .else_ifs
                    .iter_mut()
                    .all(|val| ElseIf::visit_custom_children(visitor, val))
                || expr
                    .else_branch
                    .as_mut()
                    .map(|val| Else::visit_custom_children(visitor, val))
                    .unwrap_or(true)
        }
    }
    impl ForExpr {
        pub fn visit_custom_children<V: Visitor<Custom = CustomNodeType> + VisitMut>(
            visitor: &mut V,
            expr: &mut Self,
        ) -> bool {
            visitor.visit_pat_mut(&mut expr.pat);
            visitor.visit_expr_mut(&mut expr.expr);
            Block::visit_custom_children(visitor, &mut expr.block)
        }
    }
    impl Arm {
        pub fn visit_custom_children<V: Visitor<Custom = CustomNodeType> + VisitMut>(
            visitor: &mut V,
            expr: &mut Self,
        ) -> bool {
            visitor.visit_pat_mut(&mut expr.pat);
            Block::visit_custom_children(visitor, &mut expr.body)
        }
    }
    impl MatchExpr {
        pub fn visit_custom_children<V: Visitor<Custom = CustomNodeType> + VisitMut>(
            visitor: &mut V,
            expr: &mut Self,
        ) -> bool {
            visitor.visit_expr_mut(&mut expr.expr);

            expr.arms
                .iter_mut()
                .all(|val| Arm::visit_custom_children(visitor, val))
        }
    }
    impl EscapedExpr {
        pub fn visit_custom_children<V: Visitor<Custom = CustomNodeType> + VisitMut>(
            visitor: &mut V,
            expr: &mut Self,
        ) -> bool {
            match expr {
                EscapedExpr::If(expr) => IfExpr::visit_custom_children(visitor, expr),
                EscapedExpr::For(expr) => ForExpr::visit_custom_children(visitor, expr),
                EscapedExpr::Match(expr) => MatchExpr::visit_custom_children(visitor, expr),
            }
        }
    }
    impl EscapeCode {
        pub fn visit_custom_children<V: Visitor<Custom = CustomNodeType> + VisitMut>(
            visitor: &mut V,
            node: &mut CustomNodeType,
        ) -> bool {
            let Either::A(mut this): Either<EscapeCode, _> = node.clone().try_into_or_clone_ref()
            else {
                return true;
            };
            let result = EscapedExpr::visit_custom_children(visitor, &mut this.expression);
            *node = TryIntoOrCloneRef::new_from_value(this);
            result
        }
    }
}

#[cfg(test)]
#[cfg(not(feature = "extendable"))]
mod test {
    use quote::quote;
    use rstml::{node::Node, recoverable::Recoverable, Parser, ParserConfig};
    use syn::{parse_quote, Token};

    use super::{EscapeCode, EscapedExpr};

    type MyCustomNode = EscapeCode<Token![@]>;
    type MyNode = Node<MyCustomNode>;
    #[test]
    fn if_complex_expression() {
        let actual: Recoverable<MyNode> = parse_quote! {
            @if just && an || expression {
                <div/>
            }
        };
        let Node::Custom(actual) = actual.inner() else {
            panic!()
        };

        let EscapedExpr::If(expr) = actual.expression else {
            panic!()
        };

        assert_eq!(expr.condition, parse_quote!(just && an || expression));
    }

    #[test]
    fn if_let_expr() {
        let actual: Recoverable<MyNode> = parse_quote! {
            @if let (foo) = Bar {
                <div/>
            }
        };
        let Node::Custom(actual) = actual.inner() else {
            panic!()
        };

        let EscapedExpr::If(expr) = actual.expression else {
            panic!()
        };

        assert_eq!(expr.condition, parse_quote!(let (foo) = Bar));
    }

    #[test]
    fn if_simple() {
        let actual: Recoverable<MyNode> = parse_quote! {
            @if foo > bar {
                <a regular="html" component/>
                <div>
                </div>
            }
        };

        let Node::Custom(actual) = actual.inner() else {
            panic!()
        };

        let EscapedExpr::If(expr) = &actual.expression else {
            panic!()
        };

        assert_eq!(expr.condition, parse_quote!(foo > bar));
    }

    #[test]
    fn if_else_if() {
        let actual: Recoverable<MyNode> = parse_quote! {
            @if foo > bar {
                <first/>
            } else if foo == 2 {
                <second/>
            } else if foo == 3 {
                <third/>
            } else {
                <default/>
            }
        };

        let Node::Custom(actual) = actual.inner() else {
            panic!()
        };

        let EscapedExpr::If(expr) = &actual.expression else {
            panic!()
        };

        assert_eq!(expr.condition, parse_quote!(foo > bar));
        assert_eq!(expr.else_ifs[0].condition, parse_quote!(foo == 2));
        assert_eq!(expr.else_ifs[1].condition, parse_quote!(foo == 3));
        assert!(expr.else_branch.is_some());
    }

    #[test]
    fn for_simple() {
        let actual: Recoverable<MyNode> = parse_quote! {
            @for x in foo {
                <div>
                {x}
                </div>
            }
        };

        let Node::Custom(actual) = actual.inner() else {
            panic!()
        };

        let EscapedExpr::For(expr) = &actual.expression else {
            panic!()
        };

        assert_eq!(expr.pat, parse_quote!(x));
        assert_eq!(expr.expr, parse_quote!(foo));
    }

    #[test]
    fn for_binding() {
        let actual: Recoverable<MyNode> = parse_quote! {
            @for (ref x, f) in foo {
                <div>
                {x}
                </div>
            }
        };

        let Node::Custom(actual) = actual.inner() else {
            panic!()
        };

        let EscapedExpr::For(expr) = &actual.expression else {
            panic!()
        };

        assert_eq!(expr.pat, parse_quote!((ref x, f)));
        assert_eq!(expr.expr, parse_quote!(foo));
    }

    #[test]
    fn match_simple() {
        let actual: Recoverable<MyNode> = parse_quote! {
            @match foo {
                x => {<x/>}
                _ => {<default/>}
            }
        };

        let Node::Custom(actual) = actual.inner() else {
            panic!()
        };

        let EscapedExpr::Match(expr) = &actual.expression else {
            panic!()
        };

        assert_eq!(expr.expr, parse_quote!(foo));
        assert_eq!(expr.arms[0].pat, parse_quote!(x));
        assert_eq!(expr.arms[1].pat, parse_quote!(_));
    }

    #[test]
    fn match_complex_exprs() {
        let actual: Recoverable<MyNode> = parse_quote! {
            @match foo > 2 * 2 {
                Some(x) => {<x/>}
                y|z => {<default/>}
            }
        };

        let Node::Custom(actual) = actual.inner() else {
            panic!()
        };

        let EscapedExpr::Match(expr) = &actual.expression else {
            panic!()
        };

        assert_eq!(expr.expr, parse_quote!(foo > 2 * 2));
        assert_eq!(expr.arms[0].pat, parse_quote!(Some(x)));
        assert_eq!(expr.arms[1].pat, parse_quote!(y | z));
    }

    #[test]
    fn custom_node_using_config() {
        let actual = Parser::new(
            ParserConfig::new()
                .element_close_use_default_wildcard_ident(false)
                .custom_node::<MyCustomNode>(),
        )
        .parse_simple(quote! {
            @if just && an || expression {
                <a regular="html" component/>
                <div>
                </div>
            }
        })
        .unwrap();
        let Node::Custom(actual) = &actual[0] else {
            panic!()
        };

        let EscapedExpr::If(expr) = &actual.expression else {
            panic!()
        };

        assert_eq!(expr.condition, parse_quote!(just && an || expression));
    }

    #[test]
    fn check_if_inside_if() {
        let actual: Recoverable<MyNode> = parse_quote! {
            @if just && an || expression {
                @if foo > bar {
                    <div/>
                }
            }
        };
        let Node::Custom(actual) = actual.inner() else {
            panic!()
        };

        let EscapedExpr::If(expr) = actual.expression else {
            panic!()
        };

        assert_eq!(expr.condition, parse_quote!(just && an || expression));
        let node = expr.then_branch.body.iter().next().unwrap();
        let Node::Custom(actual) = node else { panic!() };
        let EscapedExpr::If(expr) = &actual.expression else {
            panic!()
        };
        assert_eq!(expr.condition, parse_quote!(foo > bar));
    }

    #[test]
    fn for_inside_if() {
        let actual: Recoverable<MyNode> = parse_quote! {
            @if just && an || expression {
                @for x in foo {
                    <div/>
                }
            }
        };
        let Node::Custom(actual) = actual.inner() else {
            panic!()
        };

        let EscapedExpr::If(expr) = actual.expression else {
            panic!()
        };

        assert_eq!(expr.condition, parse_quote!(just && an || expression));
        let node = expr.then_branch.body.iter().next().unwrap();
        let Node::Custom(actual) = node else { panic!() };
        let EscapedExpr::For(expr) = &actual.expression else {
            panic!()
        };
        assert_eq!(expr.pat, parse_quote!(x));
        assert_eq!(expr.expr, parse_quote!(foo));
    }
}
