use rstml::node::RawText;
use rstml_to_string_macro::html_ide;

// Using this parser, one can write docs and link html tags to them.
// if this macro would be independent, it would be nicer to have docs in
// separate crate.
pub mod docs {
    /// Element has open and close tags, content and attributes.
    pub fn element() {}
}
#[test]
fn test() {
    let nightly_unqoted = " Hello  world with spaces ";
    let stable_unqoted = "Hello world with spaces";
    assert_eq!(cfg!(rstml_signal_nightly), RawText::is_source_text_available());
    let unquoted_text = if cfg!(rstml_signal_nightly) {
        nightly_unqoted
    } else {
        stable_unqoted
    };
    let world = "planet";
    assert_eq!(
        html_ide! {
            <!DOCTYPE html>
            <html>
                <head>
                    <title>"Example"</title>
                </head>
                <body>
                    <!-- "comment" -->
                    <div hello={world} />
                    <>
                        <div>"1"</div>
                        <div> Hello  world with spaces </div>
                        <div>"3"</div>
                        <div {"some-attribute-from-rust-block"}/>
                    </>
                </body>
            </html>
        },
        format!(
            r#"
            <!DOCTYPE html>
            <html>
                <head>
                    <title>Example</title>
                </head>
                <body>
                    <!-- comment -->
                    <div hello="planet"></div>
                    <div>1</div>
                    <div>{}</div>
                    <div>3</div>
                    <div some-attribute-from-rust-block></div>
                </body>
            </html>
        "#,
            unquoted_text
        )
        .split('\n')
        .map(|line| line.trim())
        .collect::<Vec<&str>>()
        .join("")
    );
}
