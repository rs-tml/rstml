use quote::ToTokens;
use termtree::Tree;

use super::Components;
use crate::{
    atoms::{tokens::CloseTagStart, FragmentClose, FragmentOpen},
    node::NodeFragment,
};

impl Into<Tree<Components>> for FragmentOpen {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle("FragmentOpen".to_string()));
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_lt.to_token_stream()
        ))));
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_gt.to_token_stream()
        ))));

        root
    }
}

impl Into<Tree<Components>> for CloseTagStart {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle("CloseTagStart".to_string()));
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_lt.to_token_stream()
        ))));
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_solidus.to_token_stream()
        ))));

        root
    }
}

impl Into<Tree<Components>> for FragmentClose {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle("FragmentClose: Some".to_string()));
        root.push(self.start_tag);
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_gt.to_token_stream()
        ))));
        root
    }
}

impl Into<Tree<Components>> for NodeFragment {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle("NodeFragment".to_string()));
        root.push(self.tag_open);

        let mut children = Tree::new(Components::Children("children".to_string()));
        let children_len = self.children.len();
        for child in self.children {
            children.push(child);
        }
        if children_len > 0 {
            root.push(children);
        }

        if let Some(tag_close) = self.tag_close {
            root.push(tag_close);
        } else {
            root.push(Tree::new(Components::SelfTitle(
                "FragmentClose: None".to_string(),
            )));
        }

        root
    }
}
