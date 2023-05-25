use quote::ToTokens;
use termtree::Tree;

use super::Components;
use crate::{
    atoms::tokens::{ComEnd, ComStart},
    node::NodeComment,
};
impl Into<Tree<Components>> for ComStart {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle("ComStart".to_string()));
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_lt.to_token_stream()
        ))));
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_not.to_token_stream()
        ))));
        root.push(Tree::new(Components::SelfTitle(format!(
            "{:?}",
            self.token_minus
                .iter()
                .map(|el| format!("{}", el.to_token_stream()))
                .collect::<Vec<_>>()
        ))));

        root
    }
}

impl Into<Tree<Components>> for ComEnd {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle("ComEnd".to_string()));
        root.push(Tree::new(Components::SelfTitle(format!(
            "{:?}",
            self.token_minus
                .iter()
                .map(|el| format!("{}", el.to_token_stream()))
                .collect::<Vec<_>>()
        ))));
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_gt.to_token_stream()
        ))));

        root
    }
}
impl Into<Tree<Components>> for NodeComment {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle("NodeComment".to_string()));
        root.push(self.token_start);
        root.push(Tree::new(Components::SelfTitle(format!(
            "{:?}",
            self.value
        ))));
        root.push(self.token_end);

        root
    }
}
