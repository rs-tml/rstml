use quote::ToTokens;
use termtree::Tree;

pub mod node_attribute;
pub mod node_name;

use crate::{
    atoms::{tokens::OpenTagEnd, CloseTag, OpenTag},
    node::{component_tree::Components, NodeElement},
};

impl Into<Tree<Components>> for CloseTag {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle("CloseTag: Some".to_string()));
        root.push(self.start_tag);
        root.push(self.name);
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_gt.to_token_stream()
        ))));

        root
    }
}

impl Into<Tree<Components>> for OpenTagEnd {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle("OpenTagEnd".to_string()));
        if let Some(solidus) = self.token_solidus {
            root.push(Tree::new(Components::SelfTitle(format!(
                "{}",
                solidus.to_token_stream()
            ))));
        } else {
            root.push(Tree::new(Components::SelfTitle(
                "token_solidus: None".to_string(),
            )));
        }
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_gt.to_token_stream()
        ))));

        root
    }
}
impl Into<Tree<Components>> for OpenTag {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle("OpenTag".to_string()));
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_lt.to_token_stream()
        ))));
        root.push(self.name);

        let mut attributes = Tree::new(Components::Children("attributes".to_string()));
        let attributes_len = self.attributes.len();
        for child in self.attributes {
            attributes.push(child);
        }
        if attributes_len > 0 {
            root.push(attributes);
        }
        root.push(self.end_tag);

        root
    }
}
impl Into<Tree<Components>> for NodeElement {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle("NodeElement".to_string()));
        root.push(self.open_tag);

        let mut children = Tree::new(Components::Children("children".to_string()));
        let children_len = self.children.len();
        for child in self.children {
            children.push(child);
        }
        if children_len > 0 {
            root.push(children);
        }
        if let Some(close_tag) = self.close_tag {
            root.push(close_tag);
        } else {
            root.push(Tree::new(Components::SelfTitle(
                "CloseTag: None".to_string(),
            )));
        }

        root
    }
}
