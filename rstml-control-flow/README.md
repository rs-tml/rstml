# Control flow implementation for `rstml`

The collection of `rstml` `CustomNode`s for the control flow.

## Motivation

This crate aims to provide an example of how to extend `rstml` with custom nodes.
Using custom nodes instead of using inner macro calls decreases the complexity of written templates, and allows `rstml` to parse the whole template at once.


## Custom nodes
Custom nodes in `rstml` are allowing external code to extend the `Node` enum. This is useful for supporting
custom syntax that is not common for HTML/XML documents. It is only allowed to be used in the context of `Node`, not in element attributes or node names.

# Control flow implementation

The common use case for custom nodes is implementing if/else operators and loops. This crate provides two different ways of implementing if/else control flow.

## Control flow using tags

The first way is to use custom tags. This is the most native way of implementing control flow in HTML templates since control flow looks like a regular HTML element. The downside of this approach is that it is not possible to properly parse `Rust` expressions inside HTML element attributes.
For example, for `<if foo > bar> </if>` it is hard to determine where the tag with attributes ends and where the content starts.

In this crate, we force the user to use a special delimiter at the end of the tag.
so instead of `<if foo > bar> </if>` we have to write `<if foo > bar !> </if>`, where `!>` is a delimiter. This special syntax is used inside `<else if>` and `<for>` tags as well.

Example:
```rust
use rstml::{parse2_with_config, node::*};
use rstml_controll_flow::tags::*;
use quote::quote;


let template = quote!{
<if foo !>
        <p> foo is true </p>
    <else if bar !>
        <p> bar is true </p>
    <else if/>
    <else>
        <p> foo and bar are false </p>
    <else/>
</if>
}

let nodes = parse2_with_config(template, Default::default().with_custom_nodes::<Conditions>())
.unwrap();
```
Note: that `else if` and `else` tags are optional and their content is moved to the fields of the `IfNode`. Other nodes inside the `if` tag are all collected into the `IfNode::body` field, even if they were between `<else if/>` and `<else>` tags in the example above.


## Controll flow using escape symbol in unquoted text.

The second way is to use the escape symbol in unquoted text.
This approach is more native for `Rust` since it is declared in the same way as in `Rust` code.
The only difference is that the block inside `{}` is not `Rust` code, but `rstml` template.

Example:
```rust
use rstml::{parse2_with_config, node::*};
use rstml_controll_flow::escape::*;
use quote::quote;


let template = quote!{
    <p>
        @if foo {
            <p> foo is true </p>
        } else if bar {
            <p> bar is true </p>
        } else {
            <p> foo and bar are false </p>
        }
    </p>
};

let nodes = parse2_with_config(template, Default::default().with_custom_nodes::<EscapedCode>())
```

`EscapedCode` escape character is configurable, and by default uses the "@" symbol.


## Using multiple `CustomNode`s at once
It is also possible to use more than one `CustomNode` at once.
For example, if you want to use both `Conditions` and `EscapedCode` custom nodes.
`rstml-control-flow` crate provides an `ExtendableCustomNode` struct that can be used to combine multiple `CustomNode`s into one. Check out `extendable.rs` docs and tests in `lib.rs` for more details.


```rust