//!
//! Extendable `CustomNode`.
//! Provide a way to mix different `CustomNode` implementation and parse them in
//! mixed context. Uses `thread_local` to save parsing routines, therefore it
//! should be initialized before usage.
//!
//! If implementors of custom node, want to support mixed context, and allow
//! using their code in mixed with other custom nodes, they should follow next
//! steps:
//!
//! 1. type Node
//! Type alias that change it's meaning depending on feature flag `extendable`.
//! Example:
//! ```no_compile
//! use rstml::node::Node as RNode;
//!
//! #[cfg(not(feature = "extendable"))]
//! type Node = RNode<EscapeCode>;
//! #[cfg(feature = "extendable")]
//! type Node = RNode<crate::ExtendableCustomNode>;
//! ```
//! This allows custom node implementation to be used in both contexts.
//!
//! 2. (optional) Trait that allows working with both contexts (can be used one
//!    from)
//! ```no_compile
//! pub trait TryIntoOrCloneRef<T>: Sized {
//!     fn try_into_or_clone_ref(self) -> Either<T, Self>;
//!     fn new_from_value(value: T) -> Self;
//! }
//! ```
//! And implementation of TryIntoOrCloneRef for both:
//! 2.1. `impl<T> TryIntoOrCloneRef<T> for T` in order to work with custom nodes
//! without extending.
//! 2.2. `impl TryIntoOrCloneRef<MyCustomNode> for
//! crate::ExtendableCustomNode` in order to work with custom nodes with
//! extending.
//!
//! 3. (optional) Implement trait `CustomNode` for `MyCustomNode` that will use
//!    trait defined in 2.

use std::{any::Any, cell::RefCell, rc::Rc};

use proc_macro2_diagnostics::Diagnostic;
use quote::ToTokens;
use rstml::{
    node::{CustomNode, Node},
    recoverable::{ParseRecoverable, RecoverableContext},
    Parser, ParserConfig, ParsingResult,
};
use syn::parse::ParseStream;

type ToTokensHandler = Box<dyn Fn(&ExtendableCustomNode, &mut proc_macro2::TokenStream)>;
type ParseRecoverableHandler =
    Box<dyn Fn(&mut RecoverableContext, ParseStream) -> Option<ExtendableCustomNode>>;
type PeekHandler = Box<dyn Fn(ParseStream) -> bool>;

thread_local! {
    static TO_TOKENS: RefCell<Option<ToTokensHandler> > = RefCell::new(None);
    static PARSE_RECOVERABLE: RefCell<Option<ParseRecoverableHandler> > = RefCell::new(None);
    static PEEK: RefCell<Option<PeekHandler> > = RefCell::new(None);
}

#[derive(Clone, Debug)]
pub struct ExtendableCustomNode {
    value: Rc<dyn Any>,
}

impl ToTokens for ExtendableCustomNode {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        TO_TOKENS.with_borrow(|f| f.as_ref().unwrap()(self, tokens))
    }
}

impl ParseRecoverable for ExtendableCustomNode {
    fn parse_recoverable(ctx: &mut RecoverableContext, input: ParseStream) -> Option<Self> {
        PARSE_RECOVERABLE.with_borrow(|f| f.as_ref().unwrap()(ctx, input))
    }
}

impl CustomNode for ExtendableCustomNode {
    fn peek_element(input: ParseStream) -> bool {
        PEEK.with_borrow(|f| f.as_ref().unwrap()(input))
    }
}
trait Sealed {}

#[allow(private_bounds)]
pub trait Tuple: Sealed {
    fn to_tokens(this: &ExtendableCustomNode, tokens: &mut proc_macro2::TokenStream);
    fn parse_recoverable(
        ctx: &mut RecoverableContext,
        input: ParseStream,
    ) -> Option<ExtendableCustomNode>;
    fn peek(input: ParseStream) -> bool;
}

macro_rules! impl_tuple {
    ($($name:ident),*) => {
        impl<$($name: CustomNode + 'static),*> Sealed for ($($name,)*) {}
        impl<$($name: CustomNode + 'static+ std::fmt::Debug),*> Tuple for ($($name,)*) {
            fn to_tokens(this: &ExtendableCustomNode, tokens: &mut proc_macro2::TokenStream) {
                $(if let Some(v) = this.try_downcast_ref::<$name>() {
                    v.to_tokens(tokens);
                })*
            }
            fn parse_recoverable(ctx: &mut RecoverableContext, input: ParseStream) -> Option<ExtendableCustomNode> {

                $(if $name::peek_element(&input.fork()) {
                    $name::parse_recoverable(ctx, input).map(ExtendableCustomNode::from_value)
                })else*
                else {
                    ctx.push_diagnostic(Diagnostic::new(proc_macro2_diagnostics::Level::Error, "Parsing invalid custom node"));
                    None
                }
            }
            fn peek(input: ParseStream) -> bool {
                dbg!(&input);
                $(dbg!($name::peek_element(&input.fork())))||*
            }
        }
    };
}

impl_tuple!(A);
impl_tuple!(A, B);
impl_tuple!(A, B, C);
impl_tuple!(A, B, C, D);
impl_tuple!(A, B, C, D, E);
impl_tuple!(A, B, C, D, E, F);
impl_tuple!(A, B, C, D, E, F, G);
impl_tuple!(A, B, C, D, E, F, G, H);

fn init_extendable_node<E: Tuple + 'static>() {
    TO_TOKENS.with_borrow_mut(|f| *f = Some(Box::new(E::to_tokens)));
    PARSE_RECOVERABLE.with_borrow_mut(|f| *f = Some(Box::new(E::parse_recoverable)));
    PEEK.with_borrow_mut(|f| *f = Some(Box::new(E::peek)));
}

fn clear_context() {
    TO_TOKENS.with_borrow_mut(|f| *f = None);
    PARSE_RECOVERABLE.with_borrow_mut(|f| *f = None);
    PEEK.with_borrow_mut(|f| *f = None);
}

impl ExtendableCustomNode {
    pub fn from_value<T: CustomNode + 'static>(value: T) -> Self {
        Self {
            value: Rc::new(value),
        }
    }
    pub fn try_downcast_ref<T: CustomNode + 'static>(&self) -> Option<&T> {
        self.value.downcast_ref::<T>()
    }
    pub fn parse2_with_config<E: Tuple + 'static>(
        config: ParserConfig,
        tokens: proc_macro2::TokenStream,
    ) -> ParsingResult<Vec<Node<ExtendableCustomNode>>> {
        init_extendable_node::<E>();
        let result =
            Parser::new(config.custom_node::<ExtendableCustomNode>()).parse_recoverable(tokens);
        clear_context();
        result
    }
}
