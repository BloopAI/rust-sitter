use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=src");

    // Generate json for debugging
    let grammars = rust_sitter_tool::generate_grammars(&PathBuf::from("src/main.rs"));
    let out_dir = std::env::var("OUT_DIR").unwrap();
    for (i, (grammar, _)) in grammars.iter().enumerate() {
        std::fs::write(
            PathBuf::from(&out_dir).join(format!("grammar_{i}.json")),
            format!("{:#}", grammar),
        )
        .unwrap();
    }

    rust_sitter_tool::build_parsers(&PathBuf::from("src/main.rs"));
}
