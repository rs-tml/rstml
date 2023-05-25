use quote::ToTokens;
use termtree::Tree;

use crate::node::{
    attribute::KeyedAttributeValue, component_tree::Components, KeyedAttribute, NodeAttribute,
};

impl Into<Tree<Components>> for KeyedAttributeValue {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle(
            "KeyedAttributeValue: Some, syn::expr::Expr".to_string(),
        ));
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.token_eq.to_token_stream()
        ))));
        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.value.to_token_stream()
        ))));

        root
    }
}

impl Into<Tree<Components>> for KeyedAttribute {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle("KeyedAttribute".to_string()));
        root.push(self.key);

        if let Some(keyed_value) = self.possible_value {
            root.push(keyed_value);
        } else {
            root.push(Tree::new(Components::SelfTitle(
                "KeyedAttributeValue: None".to_string(),
            )));
        }

        root
    }
}

impl Into<Tree<Components>> for NodeAttribute {
    fn into(self) -> Tree<Components> {
        match self {
            NodeAttribute::Block(block) => {
                let mut root = Tree::new(Components::SelfTitle("NodeAttribute: Block".to_string()));
                root.push(block);
                root
            }
            NodeAttribute::Attribute(keyed) => {
                let mut root = Tree::new(Components::SelfTitle(
                    "NodeAttribute: Attribute".to_string(),
                ));
                root.push(keyed);
                root
            }
        }
    }
}
