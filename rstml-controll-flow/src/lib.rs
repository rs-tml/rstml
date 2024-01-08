//! Example of controll flow implementations:
//! 1. One variant is based on tags `<for />` `<if />` `<else />` `<else-if />`
//! 2. another variand is based on escape character inside unquoted texts `@if
//!    {}` `@for foo in array {}`
use std::marker::PhantomData;

use quote::ToTokens;
use syn::parse::{Parse, ParseStream};

pub mod escape;
#[cfg(feature = "extendable")]
pub mod extendable;
pub mod tags;

#[cfg(feature = "extendable")]
pub use extendable::ExtendableCustomNode;

// Either variant, with Parse/ToTokens implementation
#[derive(Copy, Clone, Debug)]
pub enum Either<A, B> {
    A(A),
    B(B),
}
impl<A: Parse, B: Parse> Parse for Either<A, B> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if Self::peek_a(input) {
            input.parse().map(Self::A)
        } else {
            input.parse().map(Self::B)
        }
    }
}
impl<A: ToTokens, B: ToTokens> ToTokens for Either<A, B> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::A(a) => a.to_tokens(tokens),
            Self::B(b) => b.to_tokens(tokens),
        }
    }
}

#[allow(dead_code)]
impl<A, B> Either<A, B> {
    pub fn peek_a(stream: ParseStream) -> bool
    where
        A: Parse,
        B: Parse,
    {
        stream.fork().parse::<A>().is_ok()
    }
    pub fn to_b(self) -> Option<B> {
        match self {
            Self::A(_) => None,
            Self::B(b) => Some(b),
        }
    }
    pub fn to_a(self) -> Option<A> {
        match self {
            Self::A(a) => Some(a),
            Self::B(_) => None,
        }
    }
    pub fn is_b(self) -> bool {
        match self {
            Self::A(_) => false,
            Self::B(_) => true,
        }
    }
    pub fn is_a(self) -> bool {
        match self {
            Self::A(_) => true,
            Self::B(_) => false,
        }
    }
}

pub struct EitherA<A, B>(pub A, pub PhantomData<B>);
pub struct EitherB<A, B>(pub PhantomData<A>, pub B);

impl<A, B> TryFrom<Either<A, B>> for EitherA<A, B> {
    type Error = Either<A, B>;
    fn try_from(value: Either<A, B>) -> Result<Self, Self::Error> {
        match value {
            Either::A(a) => Ok(EitherA(a, PhantomData)),
            rest => Err(rest),
        }
    }
}

impl<A, B> TryFrom<Either<A, B>> for EitherB<A, B> {
    type Error = Either<A, B>;
    fn try_from(value: Either<A, B>) -> Result<Self, Self::Error> {
        match value {
            Either::B(b) => Ok(EitherB(PhantomData, b)),
            rest => Err(rest),
        }
    }
}

impl<A, B> From<EitherA<A, B>> for Either<A, B> {
    fn from(value: EitherA<A, B>) -> Self {
        Self::A(value.0)
    }
}

impl<A, B> From<EitherB<A, B>> for Either<A, B> {
    fn from(value: EitherB<A, B>) -> Self {
        Self::B(value.1)
    }
}

pub trait TryIntoOrCloneRef<T>: Sized {
    fn try_into_or_clone_ref(self) -> Either<T, Self>;
    fn new_from_value(value: T) -> Self;
}

impl<T> TryIntoOrCloneRef<T> for T {
    fn try_into_or_clone_ref(self) -> Either<T, Self> {
        Either::A(self)
    }
    fn new_from_value(value: T) -> Self {
        value
    }
}

#[cfg(test)]
mod tests {

    #[test]
    #[cfg(feature = "extendable")]
    fn test_mixed_tags_and_escape() {
        use quote::ToTokens;
        use rstml::node::Node;

        use crate::{escape, tags, ExtendableCustomNode};

        let tokens = quote::quote! {
                @if true {
                    <p>True</p>
                    <for foo in array !>
                        <p>Foo</p>
                    </for>
                }
                else {
                    <p>False</p>
                }
                @for foo in array {
                    <if foo == 1 !>
                        <p>Foo</p>
                    </if>
                    <p>Foo</p>
                }
        };

        let result = ExtendableCustomNode::parse2_with_config::<(
            tags::Conditions,
            escape::EscapeCode,
        )>(Default::default(), tokens);
        let ok = result.into_result().unwrap();
        assert_eq!(ok.len(), 2);

        let Node::Custom(c) = &ok[0] else {
            unreachable!()
        };
        let escape_if = c.try_downcast_ref::<escape::EscapeCode>().unwrap();
        let escape::EscapedExpr::If(if_) = &escape_if.expression else {
            unreachable!()
        };
        assert_eq!(if_.condition.to_token_stream().to_string(), "true");
        let for_tag = &if_.then_branch.body[1];
        let Node::Custom(c) = &for_tag else {
            unreachable!()
        };
        let for_tag = c.try_downcast_ref::<tags::Conditions>().unwrap();
        let tags::Conditions::For(for_) = for_tag else {
            unreachable!()
        };

        assert_eq!(for_.pat.to_token_stream().to_string(), "foo");

        let Node::Custom(c) = &ok[1] else {
            unreachable!()
        };

        let escape_for = c.try_downcast_ref::<escape::EscapeCode>().unwrap();
        let escape::EscapedExpr::For(for_) = &escape_for.expression else {
            unreachable!()
        };
        assert_eq!(for_.pat.to_token_stream().to_string(), "foo");
    }
}
