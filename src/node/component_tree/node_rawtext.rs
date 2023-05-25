use termtree::Tree;

use crate::node::{component_tree::Components, RawText};

impl Into<Tree<Components>> for RawText {
    fn into(self) -> Tree<Components> {
        let mut root = Tree::new(Components::SelfTitle(
            "RawText, proc_macro2::TokenStream".to_string(),
        ));

        root.push(Tree::new(Components::SelfTitle(format!(
            "{}",
            self.to_string_best()
        ))));
        root
    }
}
