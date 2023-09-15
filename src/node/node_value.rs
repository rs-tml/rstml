//!
//!  Node value type
use std::convert::TryFrom;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{parse::Parse, token::Brace, Block};

use crate::recoverable::RecoverableContext;

/// Block node.
///
/// Arbitrary rust code in braced `{}` blocks.
#[derive(Clone, Debug)]
pub enum NodeBlock {
    /// The block value..
    ValidBlock(Block),

    Invalid {
        brace: Brace,
        body: TokenStream,
    },
}

impl NodeBlock {
    ///
    /// Returns syntactically valid `syn::Block` of Rust code.
    ///
    /// Usually to make macro expansion IDE friendly, its better to use
    /// `ToTokens` instead. Because it also emit blocks that is invalid for
    /// syn, but valid for rust and rust analyzer. But if you need early
    /// checks that this block is valid  - use this method.
    ///
    /// Example of blocks that will or will not parse:
    /// ```no_compile
    /// {x.} // Rust will parse this syntax, but for syn this is invalid Block, because after dot ident is expected.
    ///      // Emiting code like this for rust analyzer allows it to find completion.
    ///      // This block is parsed as NodeBlock::Invalid
    /// {]}   // this is invalid syntax for rust compiler and rust analyzer so it will not be parsed at all.
    /// {x + y} // Valid syn Block, parsed as NodeBlock::Valid
    /// ```
    pub fn try_block(&self) -> Option<&Block> {
        match self {
            Self::ValidBlock(b) => Some(b),
            Self::Invalid { .. } => None,
        }
    }
}

impl TryFrom<NodeBlock> for Block {
    type Error = syn::Error;
    fn try_from(v: NodeBlock) -> Result<Block, Self::Error> {
        match v {
            NodeBlock::ValidBlock(v) => Ok(v),
            NodeBlock::Invalid { .. } => Err(syn::Error::new_spanned(
                v,
                "Cant parse expression as block.",
            )),
        }
    }
}

impl ToTokens for NodeBlock {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Invalid { brace, body } => {
                brace.surround(tokens, |tokens| body.to_tokens(tokens))
            }
            Self::ValidBlock(b) => b.to_tokens(tokens),
        }
    }
}

impl Parse for NodeBlock {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut context = RecoverableContext::default();
        let block = context.parse_recoverable(input);
        context.parse_result(block).into_result()
    }
}
