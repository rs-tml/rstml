use html_to_string_macro::html;

// Using this parser, one can write docs and link html tags to them.
// if this macro would be independent, it would be nicer to have docs in
// separate crate.
pub mod docs {
    /// Element has open and close tags, content and attributes.
    pub fn element() {}
}

#[test]
fn test_basic() {
    let nightly_unqoted = " Hello  world with spaces ";
    let stable_unqoted = "Hello world with spaces";
    let unquoted_text = if cfg!(feature = "nightly") {
        nightly_unqoted
    } else {
        stable_unqoted
    };
    let world = "planet";
    assert_eq!(
        html! {
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
                    <div hello="planet"/>
                    <div>1</div>
                    <div>{}</div>
                    <div>3</div>
                    <div some-attribute-from-rust-block/>
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

#[test]
fn test_raw_text_involved() {
    let world = "planet";
    assert_eq!(
        html! {
                <head>
                    <title>raw text, first child node " Text : second child node "<third-element-child/> raw text, fourth child node {world}</title>
                    <script>one single " Monolithic "<included-in-raw-text/> {world} raw child</script>
                </head>
        },
            r#"
                <head>
                    <title>raw text, first child node Text : second child node <third-element-child/>raw text, fourth child nodeplanet</title>
                    <script>one single " Monolithic " < included - in - raw - text / > { world } raw child</script>
                </head>
        "#
        .split('\n')
        .map(|line| line.trim())
        .collect::<Vec<&str>>()
        .join("")
    );
}
