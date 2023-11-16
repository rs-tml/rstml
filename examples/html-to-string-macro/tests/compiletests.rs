#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    if cfg!(rstml_signal_nightly) {
        t.compile_fail("tests/ui/*.rs")
    };
}
