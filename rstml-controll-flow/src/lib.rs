//! Example of controll flow implementations:
//! 1. One variant is based on tags `<for />` `<if />` `<else />` `<else-if />`
//! 2. another variand is based on escape character inside unquoted texts `@if
//!    {}` `@for foo in array {}`
use std::marker::PhantomData;

use quote::ToTokens;
use syn::parse::{Parse, ParseStream};

pub mod escape;
pub mod tags;

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
