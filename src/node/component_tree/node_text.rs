use quote::ToTokens;
use termtree::Tree;

use crate::node::{component_tree::Components, NodeText};

impl Into<Tree<Components>> for NodeText {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle(
            "NodeText, syn::lit::LitStr".to_string(),
        ));

        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.value.to_token_stream()
        ))));

        root
    }
}
