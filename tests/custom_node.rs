use quote::{quote, TokenStreamExt};
use rstml::{
    atoms::{self, OpenTag, OpenTagEnd},
    node::{CustomNode, Node, NodeElement},
    recoverable::{ParseRecoverable, Recoverable},
    Parser, ParserConfig,
};
use syn::{parse_quote, Expr, Token};

#[derive(Debug, syn_derive::ToTokens)]
struct If {
    token_lt: Token![<],
    token_if: Token![if],
    condition: Expr,
    open_tag_end: OpenTagEnd,
    #[to_tokens(TokenStreamExt::append_all)]
    body: Vec<Node>,
    close_tag: Option<atoms::CloseTag>,
}
impl ParseRecoverable for If {
    fn parse_recoverable(
        parser: &mut rstml::recoverable::RecoverableContext,
        input: syn::parse::ParseStream,
    ) -> Option<Self> {
        let token_lt = OpenTag::parse_start_tag(parser, input)?;
        let token_if = parser.parse_simple(input)?;
        let (condition, open_tag_end): (_, OpenTagEnd) = parser.parse_simple_until(input)?;
        let (body, close_tag) = if open_tag_end.token_solidus.is_none() {
            // Passed to allow parsing of close_tag
            NodeElement::parse_children(
                parser,
                input,
                false,
                &OpenTag {
                    token_lt,
                    name: parse_quote!(#token_if),
                    generics: Default::default(),
                    attributes: Default::default(),
                    end_tag: open_tag_end.clone(),
                },
            )?
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

impl CustomNode for If {
    fn peek_element(input: syn::parse::ParseStream) -> bool {
        input.peek(Token![<]) && input.peek2(Token![if])
    }
}

#[test]
fn custom_node() {
    let actual: Recoverable<Node<If>> = parse_quote! {
        <if just && an || expression>
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
        <if just && an || expression>
            <a regular="html" component/>
            <div>
            </div>
        </_>
    })
    .unwrap();
    let Node::Custom(actual) = &actual[0] else {
        panic!()
    };

    assert_eq!(actual.condition, parse_quote!(just && an || expression));
}
