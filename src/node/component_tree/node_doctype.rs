use quote::ToTokens;
use termtree::Tree;

use crate::{
    atoms::tokens::DocStart,
    node::{component_tree::Components, NodeDoctype},
};

impl Into<Tree<Components>> for DocStart {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle("DocStart".to_string()));
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_lt.to_token_stream()
        ))));
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_not.to_token_stream()
        ))));

        root
    }
}
impl Into<Tree<Components>> for NodeDoctype {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle("NodeDoctype".to_string()));

        root.push(self.token_start);
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_doctype.to_token_stream()
        ))));
        root.push(self.value);
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_end.to_token_stream()
        ))));

        root
    }
}
