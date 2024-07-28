use std::{env, process::Command, str::from_utf8};

fn main() {
    if is_rustc_nightly() {
        println!("cargo:rustc-cfg=rstml_signal_nightly");
    }
}

fn is_rustc_nightly() -> bool {
    || -> Option<bool> {
        let rustc = env::var_os("RUSTC")?;
        let output = Command::new(rustc).arg("--version").output().ok()?;
        let version = from_utf8(&output.stdout).ok()?;
        Some(version.contains("nightly"))
    }()
    .unwrap_or_default()
}
