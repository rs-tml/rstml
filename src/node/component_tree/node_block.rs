use quote::ToTokens;
use termtree::Tree;

use crate::node::{component_tree::Components, NodeBlock};

impl Into<Tree<Components>> for NodeBlock {
    fn into(self) -> Tree<Components> {
        match self {
            NodeBlock::ValidBlock(block) => {
                let mut root = Tree::new(Components::SelfTitle(
                    "NodeBlock: ValidBlock, syn::stmt::Block".to_string(),
                ));
                root.push(Tree::new(Components::SelfTitle(format!(
                    "{}",
                    block.to_token_stream()
                ))));
                root
            }
            NodeBlock::Invalid { body, .. } => {
                let mut root = Tree::new(Components::SelfTitle(
                    "NodeBlock: InvalidBlock, proc_macro2::TokenStream".to_string(),
                ));
                root.push(Tree::new(Components::SelfTitle("{...}".to_string())));
                root.push(Tree::new(Components::SelfTitle(format!("{}", body))));
                root
            }
        }
    }
}
