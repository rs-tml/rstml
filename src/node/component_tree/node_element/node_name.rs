use quote::ToTokens;
use termtree::Tree;

use crate::node::{component_tree::Components, NodeName};

impl Into<Tree<Components>> for NodeName {
    fn into(self) -> Tree<Components> {
        match self {
            NodeName::Path(path) => {
                let mut root = Tree::new(Components::SelfTitle(
                    "NodeName: Path, syn::expr::ExprPath".to_string(),
                ));
                root.push(Tree::new(Components::SelfTitle(format!(
                    "{}",
                    path.to_token_stream()
                ))));
                root
            }
            NodeName::Punctuated(punctuated) => {
                let mut root = Tree::new(Components::SelfTitle(
                    "NodeName: Punctuated, syn::punctuated::Punctuated<Ident, Punct>".to_string(),
                ));
                root.push(Tree::new(Components::SelfTitle(format!(
                    "{}",
                    punctuated.to_token_stream()
                ))));
                root
            }
            NodeName::Block(block) => {
                let mut root = Tree::new(Components::SelfTitle(
                    "NodeName: Block, syn::stmt::Block".to_string(),
                ));
                root.push(Tree::new(Components::SelfTitle(format!(
                    "{}",
                    block.to_token_stream()
                ))));
                root
            }
        }
    }
}
