//!
//! This module contain implementation of flow-controll based on rstml custom
//! nodes.
//!
//! This is usefull when one need to define optional or repetetive html(*ml)
//! elements. It contain implementation of If, Match and For constructions that
//! is very simmilar to rust. The main difference in implementation is that body
//! of construction (part inside curly brackets) is expected to be an html(*ml),
//! but condition is expected to be in rust syntax.

use quote::{quote, ToTokens, TokenStreamExt};
use rstml::{
    atoms::{self, OpenTag, OpenTagEnd},
    node::{CustomNode, Node, NodeElement},
    recoverable::{ParseRecoverable, Recoverable, RecoverableContext},
    Parser, ParserConfig,
};
use syn::{
    braced,
    parse::{Parse, ParseStream, Peek},
    parse_quote,
    token::Brace,
    Expr, Pat, Token,
};

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
struct IfExpr {
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

#[derive(Debug, syn_derive::ToTokens)]
struct ForExpr {
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

#[derive(Debug, syn_derive::ToTokens)]
struct Arm {
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

#[derive(Debug, syn_derive::ToTokens)]
struct MatchExpr {
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

#[derive(Debug, syn_derive::ToTokens)]
enum EscapedExpr {
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

impl EscapedExpr {
    // Convert `Expr` to `syn::Expr`.
    // Uses wrap closure to convert each `Block` to `syn::Block` .
    //
    // Usefull if one need to wrap inner `Node` array with macro call.
    // Example:
    // input: `macro!{<Foo>@if x > 1 {<Bar/>} </Foo>}`
    // output:
    // ```rust
    //  let mut foo = Foo::new();
    //  if x > 1 {
    //      foo = foo.child(macro!{<Bar/>})
    //  }
    //  foo
    // ```
    pub fn into_wrapped_expr<F>(self, wrap: F) -> syn::Expr
    where
        F: Fn(Block) -> syn::Block,
    {
        todo!()
    }
}

#[derive(Debug, syn_derive::ToTokens)]
struct EscapeCode<T: ToTokens> {
    escape_token: T,
    expression: EscapedExpr,
}

impl<T> CustomNode for EscapeCode<T>
where
    T: Parse + ToTokens,
    EscapeCode<T>: ToTokens,
{
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        ToTokens::to_tokens(&self, tokens)
    }

    fn peek_element(input: syn::parse::ParseStream) -> bool {
        if input.parse::<T>().is_err() {
            return false;
        }
        input.peek(Token![if]) ||
        input.peek(Token![for]) ||
        // input.peek(Token![while]) ||
        input.peek(Token![match])
    }

    fn parse_element(
        parser: &mut rstml::recoverable::RecoverableContext,
        input: syn::parse::ParseStream,
    ) -> Option<Self> {
        let escape_token = parser.parse_simple(input)?;
        let expression = parser.parse_recoverable(input)?;

        Some(Self {
            escape_token,
            expression,
        })
    }
}

#[cfg(test)]
mod test {
    use quote::quote;
    use rstml::{node::Node, recoverable::Recoverable, Parser, ParserConfig};
    use syn::{parse_quote, Token};

    use super::EscapeCode;
    use crate::escape::EscapedExpr;

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
}
