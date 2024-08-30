#[rust_sitter::grammar("externals", "src/externals/scanner.c")]
#[allow(dead_code)]
mod grammar {
    #[rust_sitter::language]
    #[derive(Debug)]
    pub struct Language {
        #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
        v: Option<i32>,
        #[rust_sitter::leaf(text = "_")]
        _s: (),
        #[rust_sitter::leaf(text = ".")]
        _d: Option<()>,
    }

    #[rust_sitter::external]
    #[rust_sitter::extra]
    struct WhiteSpaces;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn externals_grammar() {
        insta::assert_debug_snapshot!(grammar::parse("_"));
        insta::assert_debug_snapshot!(grammar::parse("_."));
        insta::assert_debug_snapshot!(grammar::parse("1_"));
        insta::assert_debug_snapshot!(grammar::parse("23_."));
        insta::assert_debug_snapshot!(grammar::parse(" _  "));
        insta::assert_debug_snapshot!(grammar::parse("_ , ."));
        insta::assert_debug_snapshot!(grammar::parse("1_;"));
        insta::assert_debug_snapshot!(grammar::parse("23;; , ;;_."));
    }
}
