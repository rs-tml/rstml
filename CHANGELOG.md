# Changelog

All notable changes to this project will be documented in this file.

## [0.12.1] - 2025-01-21

### Chore

- Update deps versions.

### Other

- Update dependency versions.


## [0.12.0] - 2024-07-28

### Chore

- Fix ci
- Fix clippy
- Add example of custom node to html-to-string-macro.
- Add eq and partial eq implementation.
- Move custom node implementation to separate crate.
- Refactor CustomNode trait
- Add readme to rstml-controll-flow
- Refine visitor api (remove clone/debug bounds)
- Add custom node visitor implementation
- Apply fmt/tests
- Refactor custom node visitor.
- Refactor code coverage script.

### Features

- Support for custom node
- Parsing utilities for custom nodes
- Add macro matcher
- Add visitor api.
- Implement extendable custom node

### Fix

- Fix imports in example
- Empty input parsing
- Make Recoverable field public.
- Fix spelling mistake "witespace" to "whitespace"
- Add unquoted text custom node integration
- Use config and diagnostc from invalid_block in attribute place.

### Other

- Use example from inline docs in readme
- Address review

- Allow multiple puncts in a row in NodeName
- Added test for consecutive punctuation in NodeName

Signed-off-by: max <gmx.sht@gmail.com>

- Update README.md
- Default impl for Parser ([#51](https://github.com/rs-tml/rstml/issues/51))

- Initial implementation of block attributes

- Implement braced key value attribute. See also: [#54](https://github.com/rs-tml/rstml/issues/54#issuecomment-2247924159)

- Remove scratch

- Rename KVAttributeValue::Braced -> KVAttributeValue::InvalidBraced


### Refactor

- Refactor workspace move rstml to separate folder.

Use workspace version of dependencies whenever is possible.


## [0.11.1] - 2023-08-14

### Chore

- Update html-to-macro example
- Move ide feature of html-to-string example to separate macro.
- Fix ci
- Add powered by section in readme.

### Features

- Allow disabling color diagnostics

### Fix

- Export NodeNameFragment

### Other

- Coresponding -> corresponding


## [0.11.0] - 2023-07-18

### Chore

- Clippy ci
- Apply fmt fixes
- Add tests for fn binding syntax.

### Documentation

- Doc test formatting


### Features

- Add generics support to node element.
- FnBinding pattern
- Feat: add number support in NodeName attribute
Make NodeName compatible with SGML basic types specification (by adding support of more than one punctuation in series).


### Fix

- Allow wildcard in any element.

### Other

- Update README.md
- Allow wildcard close tag for block elements

- Make block element close wildcard more general


## [0.10.5] - 2023-05-17

### Chore

- Fix docs build.

## [0.10.4] - 2023-05-17

### Chore

- Fix fmt add git-hook

## [0.10.3] - 2023-05-14

### Chore

- Remove dbg macro.

## [0.10.2] - 2023-05-14

### Documentation

- Fixup cargo docs

## [0.10.0] - 2023-05-13

### Chore

- Refactor node module, split types into submodules
- Update to syn 2.0
- Implement config passing, and allow to emit more than one error from macro expansion.
- Make Node types clonable.
- Fix recoverable parser. Now try to recover if any sequence is incorrect.
- Bump syn_derive to 0.1.6 to avoid nightly
- Start a new version history

### Documentation

- Add comparsion with syn-rsx

### Features

- Remove attributes from node list, and make type guaranties that attribute will be stored only in Element
- Improve tag close handling.
- Allow parsing of invalid rust code blocks
- Add support of unqoted text.

### Fix

- Refactor ToTokens implementation and node structure to contain all source tokens.
- Re-implement tree flattening.
- Reimplement transform_block and fix tests
- Attribute value parsing span
- Updated example, bench, made fragment parsing recoverable.
- Make public keyed attribute fields.
- RawText to_source implementation on stable

### Refactor

- Move node.rs to separate module.
- Remove Display implementation for most of node types.
- Simlify Parse and ToToken impl, by using syn_derive.
- Refactor: remove NodeValueExpr type
NodeValueExpr was replaced by syn::Expr in all places where expr is needed.
For places where {code} is expected NodeBlock was used.

- Cleanup of parsing code.
- Refactor recoverable parser.
- Refactor: Remove tls context at all.
Use RecoverableContext instead.


### Testing

- TRYBUILD=overwrite in ui tests


## [unreleased]

### Documentation

- Mixed name punctuation and node span ([#44](https://github.com/stoically/syn-rsx/issues/44))
- Missing module documentation ([#45](https://github.com/stoically/syn-rsx/issues/45))
- Fix formatting ([#46](https://github.com/stoically/syn-rsx/issues/46))
- Fix code block ([#47](https://github.com/stoically/syn-rsx/issues/47))

### Features

- Track the source span for all nodes ([#42](https://github.com/stoically/syn-rsx/issues/42))

## [0.9.0] - 2022-11-10

### Documentation

- Remove outdated node link ([#36](https://github.com/stoically/syn-rsx/issues/36))
- Hint regarding braced blocks ([#38](https://github.com/stoically/syn-rsx/issues/38))
- Fix typo ([#39](https://github.com/stoically/syn-rsx/issues/39))

## [0.9.0-beta.2] - 2022-11-06

### Features

- [**breaking**] Replace Colon and Dash with a merged variant ([#34](https://github.com/stoically/syn-rsx/issues/34))

## [0.9.0-beta.1] - 2022-11-03

### Chore

- Add rustfmt.toml

### Documentation

- Update README
- Update example
- Update html macro docs
- Add link to example
- Fix node links
- Improve parser docs
- Remove TODO
- Fix attribute value example ([#28](https://github.com/stoically/syn-rsx/issues/28))
- Fix blocks example ([#29](https://github.com/stoically/syn-rsx/issues/29))
- Fix typo ([#30](https://github.com/stoically/syn-rsx/issues/30))

### Features

- [**breaking**] Make path_to_string private

### Other

- Move flat tree converter back into node parser
- Switch fmt to nightly toolchain

### Refactor

- [**breaking**] Drop `NodeName::span` method
- Pass block_transform a forked stream
- Move flat tree converter to node method
- Replace extrude with let-else ([#31](https://github.com/stoically/syn-rsx/issues/31))

## [0.9.0-alpha.1] - 2022-10-21

### Chore

- Add Cargo.lock to .gitignore
- Add git-cliff configuration
- Add CHANGELOG
- Use the actual html-to-string-macro crate as example
- Update README badges

### Refactor

- Move config into dedicated module
- [**breaking**] Switch `Node` to enum-style ([#23](https://github.com/stoically/syn-rsx/issues/23))

## [0.8.1] - 2022-06-26

### Chore

- Clippy
- Clippy
- Remove Cargo.lock
- Bump dependencies

### Documentation

- Update README

## [0.8.0] - 2021-02-17

### Documentation

- Fix and sync
- Typo

### Features

- Doctypes, comments and fragments
- Value_as_string support for `ExprPath`

### Other

- Should be value not name

### Refactor

- Remove unnecessary `Result`

### Testing

- More reserved keywords tests

## [0.8.0-beta.2] - 2021-01-04

### Documentation

- Sync lib with readme

### Features

- [**breaking**] Block in node name position ([#11](https://github.com/stoically/syn-rsx/issues/11))

### Other

- Tarpaulin and codecov

## [0.8.0-beta.1] - 2021-01-03

### Documentation

- Node

### Features

- Properly handle empty elements
- [**breaking**] Transform_block callback ([#9](https://github.com/stoically/syn-rsx/issues/9))
- [**breaking**] Doctype ([#6](https://github.com/stoically/syn-rsx/issues/6))
- [**breaking**] Html comments ([#7](https://github.com/stoically/syn-rsx/issues/7))
- [**breaking**] Fragments ([#8](https://github.com/stoically/syn-rsx/issues/8))

### Other

- Formatting
- Bump criterion

### Refactor

- Cleanup

## [0.7.3] - 2020-10-30

### Documentation

- Rephrase misleading unquoted text hint
- Update node description
- Update NodeName description

### Features

- Value_as_block method for nodes
- Implement ToString for NodeName
- Support blocks in html-to-string-macro
- Implement ToTokens for NodeName

### Other

- Only count top level nodes in case of flat_tree
- Parse2 with criterion
- More test tokens
- Update

### Performance

- More peeking and better block parsing ([#5](https://github.com/stoically/syn-rsx/issues/5))
- Use `node_name_punctuated_ident` to parse name path

### Refactor

- Better error reporting
- Rename test file
- Switch impl ToString on Node to impl Display
- Merge text and block handling

## [0.7.2] - 2020-09-10

### Documentation

- Error reporting

### Features

- Expose span fn on NodeName as well

### Refactor

- Better error messages

## [0.7.1] - 2020-09-09

### Other

- Check after parsing is done

## [0.7.0] - 2020-09-09

### Documentation

- Update readme
- Update readme
- Test feature examples

### Features

- Helper function to get node name span
- Support blocks as attributes
- Configure maximum number of allowed top level nodes
- Configure type of top level nodes

### Other

- Bump
- Add html_to_string macro

### Refactor

- Peek to determine node type
- Better error messages
- Move integration tests into tests folder
- Move parse configuration from arg to dedicated fns
- Check value first
- Get rid of helper struct
- Exactly required number of top level nodes

## [0.6.1] - 2020-06-06

### Chore

- Update cargo lock

### Documentation

- Typo

## [0.6.0] - 2020-06-06

### Chore

- Update cargo.lock

### Documentation

- Exposed Dash and minor improvements

### Features

- Node names with colons

### Refactor

- Cleanup
- Rename Dashed to Dash
- Tests cleanup

## [0.5.0] - 2020-06-04

### Features

- Dashed node names

## [0.4.1] - 2020-06-04

### Documentation

- Update readme

### Refactor

- Cleanup

## [0.4.0] - 2020-06-03

### Documentation

- Update example

### Refactor

- Rename childs to children ([#1](https://github.com/stoically/syn-rsx/issues/1))

## [0.3.4] - 2020-06-03

### Documentation

- Spelling
- Update example
- Update readme

### Refactor

- Change node name to `syn::ExprPath`
- Use advance_to after fork
- Restructure code

## [0.3.1] - 2020-06-03

### Refactor

- Cleanup

## [0.3.0] - 2020-06-03

### Documentation

- Update readme

### Features

- Parse tag name and attribute value as `syn::Path`

### Refactor

- Clippy lints
- Cleanup
- Use iter::once
- Cleanup
- Block expression parsing

## [0.2.0] - 2020-05-30

### Chore

- Update cargo.lock
- Bump syn dep

### Documentation

- Project keywords

### Features

- Parse full block

## [0.1.2] - 2020-05-29

### Chore

- Update cargo.lock

### Documentation

- Readme badges
- Readme key for crates.io
- Update
- Update readme

### Other

- Initial commit

- Build workflow

### Refactor

- Pub not needed
- Parse blocks as NodeType::Block

<!-- generated by git-cliff -->
