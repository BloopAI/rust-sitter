use std::collections::HashSet;

use rust_sitter_common::*;
use serde_json::{json, Map, Value};
use syn::{parse::Parse, punctuated::Punctuated, *};

use crate::Metadata;

fn gen_field(
    path: String,
    leaf_type: Type,
    leaf_attrs: Vec<Attribute>,
    word_rule: &mut Option<String>,
    out: &mut Map<String, Value>,
) -> (Value, bool) {
    let leaf_attr = leaf_attrs
        .iter()
        .find(|attr| attr.path == syn::parse_quote!(rust_sitter::leaf));

    if leaf_attrs
        .iter()
        .any(|attr| attr.path == syn::parse_quote!(rust_sitter::word))
    {
        if word_rule.is_some() {
            panic!("Multiple `word` rules specified");
        }

        *word_rule = Some(path.clone());
    }

    let leaf_params = leaf_attr.and_then(|a| {
        a.parse_args_with(Punctuated::<NameValueExpr, Token![,]>::parse_terminated)
            .ok()
    });

    let pattern_param = leaf_params.as_ref().and_then(|p| {
        p.iter()
            .find(|param| param.path == "pattern")
            .map(|p| p.expr.clone())
    });

    let text_param = leaf_params.as_ref().and_then(|p| {
        p.iter()
            .find(|param| param.path == "text")
            .map(|p| p.expr.clone())
    });

    let mut skip_over = HashSet::new();
    skip_over.insert("Spanned");
    skip_over.insert("Box");

    let (inner_type_vec, is_vec) = try_extract_inner_type(&leaf_type, "Vec", &skip_over);
    let (inner_type_option, is_option) = try_extract_inner_type(&leaf_type, "Option", &skip_over);

    if !is_vec && !is_option {
        if let Some(Expr::Lit(lit)) = pattern_param {
            if let Lit::Str(s) = &lit.lit {
                out.insert(
                    path.clone(),
                    json!({
                        "type": "PATTERN",
                        "value": s.value(),
                    }),
                );

                (
                    json!({
                        "type": "SYMBOL",
                        "name": path
                    }),
                    is_option,
                )
            } else {
                panic!("Expected string literal for pattern");
            }
        } else if let Some(Expr::Lit(lit)) = text_param {
            if let Lit::Str(s) = &lit.lit {
                out.insert(
                    path.clone(),
                    json!({
                        "type": "STRING",
                        "value": s.value(),
                    }),
                );

                (
                    json!({
                        "type": "SYMBOL",
                        "name": path
                    }),
                    is_option,
                )
            } else {
                panic!("Expected string literal for text");
            }
        } else {
            let symbol_name = if let Type::Path(p) = filter_inner_type(&leaf_type, &skip_over) {
                if p.path.segments.len() == 1 {
                    p.path.segments[0].ident.to_string()
                } else {
                    panic!("Expected a single segment path");
                }
            } else {
                panic!("Expected a path");
            };

            (
                json!({
                    "type": "SYMBOL",
                    "name": symbol_name,
                }),
                false,
            )
        }
    } else if is_vec {
        let (field_json, field_optional) = gen_field(
            path.clone(),
            inner_type_vec,
            leaf_attr.iter().cloned().cloned().collect(),
            word_rule,
            out,
        );

        let delimited_attr = leaf_attrs
            .iter()
            .find(|attr| attr.path == syn::parse_quote!(rust_sitter::delimited));

        let delimited_params =
            delimited_attr.and_then(|a| a.parse_args_with(FieldThenParams::parse).ok());

        let delimiter_json = delimited_params.map(|p| {
            gen_field(
                format!("{path}_vec_delimiter"),
                p.field.ty,
                p.field.attrs,
                word_rule,
                out,
            )
        });

        let repeat_attr = leaf_attrs
            .iter()
            .find(|attr| attr.path == syn::parse_quote!(rust_sitter::repeat));

        let repeat_params = repeat_attr.and_then(|a| {
            a.parse_args_with(Punctuated::<NameValueExpr, Token![,]>::parse_terminated)
                .ok()
        });

        let repeat_non_empty = repeat_params
            .and_then(|p| {
                p.iter()
                    .find(|param| param.path == "non_empty")
                    .map(|p| p.expr.clone())
            })
            .map(|e| e == syn::parse_quote!(true))
            .unwrap_or(false);

        let field_rule_non_optional = json!({
            "type": "FIELD",
            "name": format!("{path}_vec_element"),
            "content": field_json
        });

        let field_rule = if field_optional {
            json!({
                "type": "CHOICE",
                "members": [
                    {
                        "type": "BLANK"
                    },
                    field_rule_non_optional
                ]
            })
        } else {
            field_rule_non_optional
        };

        let vec_contents = if let Some((delimiter_json, delimiter_optional)) = delimiter_json {
            let delim_made_optional = if delimiter_optional {
                json!({
                    "type": "CHOICE",
                    "members": [
                        {
                            "type": "BLANK"
                        },
                        delimiter_json
                    ]
                })
            } else {
                delimiter_json
            };

            json!({
                "type": "SEQ",
                "members": [
                    field_rule,
                    {
                        "type": if field_optional {
                            "REPEAT1"
                        } else {
                            "REPEAT"
                        },
                        "content": {
                            "type": "SEQ",
                            "members": [
                                delim_made_optional,
                                field_rule,
                            ]
                        }
                    }
                ]
            })
        } else {
            json!({
                "type": "REPEAT1",
                "content": field_rule
            })
        };

        let contents_ident = format!("{path}_vec_contents");
        out.insert(contents_ident.clone(), vec_contents);

        (
            json!({
                "type": "SYMBOL",
                "name": contents_ident,
            }),
            !repeat_non_empty,
        )
    } else {
        // is_option
        let (field_json, field_optional) =
            gen_field(path, inner_type_option, leaf_attrs, word_rule, out);

        if field_optional {
            panic!("Option<Option<_>> is not supported");
        }

        (field_json, true)
    }
}

fn gen_struct_or_variant(
    path: String,
    attrs: Vec<Attribute>,
    fields: Fields,
    out: &mut Map<String, Value>,
    word_rule: &mut Option<String>,
) {
    fn gen_field_optional(
        path: &str,
        field: &Field,
        word_rule: &mut Option<String>,
        out: &mut Map<String, Value>,
        ident_str: String,
    ) -> Value {
        let (field_contents, is_option) = gen_field(
            format!("{path}_{ident_str}"),
            field.ty.clone(),
            field.attrs.clone(),
            word_rule,
            out,
        );

        let core = json!({
            "type": "FIELD",
            "name": ident_str,
            "content": field_contents
        });

        if is_option {
            json!({
                "type": "CHOICE",
                "members": [
                    {
                        "type": "BLANK"
                    },
                    core
                ]
            })
        } else {
            core
        }
    }

    let children = fields
        .iter()
        .enumerate()
        .filter_map(|(i, field)| {
            if field
                .attrs
                .iter()
                .any(|attr| attr.path == syn::parse_quote!(rust_sitter::skip))
            {
                None
            } else {
                let ident_str = field
                    .ident
                    .as_ref()
                    .map(|v| v.to_string())
                    .unwrap_or(format!("{i}"));

                Some(gen_field_optional(&path, field, word_rule, out, ident_str))
            }
        })
        .collect::<Vec<Value>>();

    let prec_attr = attrs
        .iter()
        .find(|attr| attr.path == syn::parse_quote!(rust_sitter::prec));

    let prec_param = prec_attr.and_then(|a| a.parse_args_with(Expr::parse).ok());

    let prec_left_attr = attrs
        .iter()
        .find(|attr| attr.path == syn::parse_quote!(rust_sitter::prec_left));

    let prec_left_param = prec_left_attr.and_then(|a| a.parse_args_with(Expr::parse).ok());

    let prec_right_attr = attrs
        .iter()
        .find(|attr| attr.path == syn::parse_quote!(rust_sitter::prec_right));

    let prec_right_param = prec_right_attr.and_then(|a| a.parse_args_with(Expr::parse).ok());

    let base_rule = match fields {
        Fields::Unit => {
            let dummy_field = Field {
                attrs: attrs.clone(),
                vis: Visibility::Inherited,
                ident: None,
                colon_token: None,
                ty: Type::Tuple(TypeTuple {
                    paren_token: Default::default(),
                    elems: Punctuated::new(),
                }),
            };
            gen_field_optional(&path, &dummy_field, word_rule, out, "unit".to_owned())
        }
        _ => json!({
            "type": "SEQ",
            "members": children
        }),
    };

    let rule = if let Some(Expr::Lit(lit)) = prec_param {
        if prec_left_attr.is_some() || prec_right_attr.is_some() {
            panic!("only one of prec, prec_left, and prec_right can be specified");
        }

        if let Lit::Int(i) = &lit.lit {
            json!({
                "type": "PREC",
                "value": i.base10_parse::<u32>().unwrap(),
                "content": base_rule
            })
        } else {
            panic!("Expected integer literal for precedence");
        }
    } else if let Some(Expr::Lit(lit)) = prec_left_param {
        if prec_right_attr.is_some() {
            panic!("only one of prec, prec_left, and prec_right can be specified");
        }

        if let Lit::Int(i) = &lit.lit {
            json!({
                "type": "PREC_LEFT",
                "value": i.base10_parse::<u32>().unwrap(),
                "content": base_rule
            })
        } else {
            panic!("Expected integer literal for precedence");
        }
    } else if let Some(Expr::Lit(lit)) = prec_right_param {
        if let Lit::Int(i) = &lit.lit {
            json!({
                "type": "PREC_RIGHT",
                "value": i.base10_parse::<u32>().unwrap(),
                "content": base_rule
            })
        } else {
            panic!("Expected integer literal for precedence");
        }
    } else {
        base_rule
    };

    out.insert(path, rule);
}

pub fn generate_grammar(module: &ItemMod) -> (Value, Metadata) {
    let mut metadata = Metadata::default();

    let mut rules_map = Map::new();
    // for some reason, source_file must be the first key for things to work
    rules_map.insert("source_file".to_string(), json!({}));

    let mut extras_list = vec![];
    let mut externals_list = vec![];
    let mut external_token_types = vec![];

    let grammar_attr = module
        .attrs
        .iter()
        .find(|a| a.path == syn::parse_quote!(rust_sitter::grammar))
        .expect("Each grammar must have a name");

    let grammar_arg_exprs = grammar_attr
        .parse_args_with(Punctuated::<LitStr, Token![,]>::parse_terminated)
        .unwrap_or_else(|_| panic!("Expected string literal arguments"));
    let mut grammar_arg_exprs = grammar_arg_exprs.into_iter();

    let grammar_name = grammar_arg_exprs
        .next()
        .unwrap_or_else(|| panic!("Expected at least one argument giving grammar name"))
        .value();

    if let Some(scannar_name) = grammar_arg_exprs.next() {
        let scannar_name = scannar_name.value();
        let mut scannar_name = std::path::PathBuf::from(scannar_name);
        if let Ok(absolute) = std::path::absolute(scannar_name.as_path()) {
            scannar_name = absolute;
        }
        let scanner_c = std::fs::read_to_string(&scannar_name).unwrap_or_else(|err| {
            panic!("could not read file `{}`: {err}", scannar_name.display())
        });
        metadata.scanner_c = Some(scanner_c)
    }

    let (_, contents) = module.content.as_ref().unwrap();

    let root_type = contents
        .iter()
        .find_map(|item| match item {
            Item::Enum(ItemEnum { ident, attrs, .. })
            | Item::Struct(ItemStruct { ident, attrs, .. }) => {
                if attrs
                    .iter()
                    .any(|attr| attr.path == syn::parse_quote!(rust_sitter::language))
                {
                    Some(ident.clone())
                } else {
                    None
                }
            }
            _ => None,
        })
        .expect("Each parser must have the root type annotated with `#[rust_sitter::language]`")
        .to_string();

    // Optionally locate the rule annotated with `#[rust_sitter::word]`.
    let mut word_rule = None;
    contents.iter().for_each(|c| {
        let (symbol, attrs) = match c {
            Item::Enum(e) => (e.ident.to_string(), e.attrs.clone()),

            Item::Struct(s) => (s.ident.to_string(), s.attrs.clone()),

            _ => return,
        };
        if attrs
            .iter()
            .any(|a| a.path == syn::parse_quote!(rust_sitter::external))
        {
            externals_list.push(json!({
                "type": "SYMBOL",
                "name": symbol
            }));
            external_token_types.push(symbol.clone())
        } else {
            match c {
                Item::Enum(e) => {
                    e.variants.iter().for_each(|v| {
                        gen_struct_or_variant(
                            format!("{}_{}", e.ident, v.ident),
                            v.attrs.clone(),
                            v.fields.clone(),
                            &mut rules_map,
                            &mut word_rule,
                        )
                    });

                    let mut members: Vec<Value> = vec![];
                    e.variants.iter().for_each(|v| {
                        let variant_path = format!("{}_{}", e.ident.clone(), v.ident);
                        members.push(json!({
                            "type": "SYMBOL",
                            "name": variant_path
                        }))
                    });

                    let rule = json!({
                        "type": "CHOICE",
                        "members": members
                    });

                    rules_map.insert(e.ident.to_string(), rule);
                }

                Item::Struct(s) => {
                    gen_struct_or_variant(
                        s.ident.to_string(),
                        s.attrs.clone(),
                        s.fields.clone(),
                        &mut rules_map,
                        &mut word_rule,
                    );
                }

                _ => return,
            }
        }

        if attrs
            .iter()
            .any(|a| a.path == syn::parse_quote!(rust_sitter::extra))
        {
            extras_list.push(json!({
                "type": "SYMBOL",
                "name": symbol
            }));
        }
    });

    rules_map.insert(
        "source_file".to_string(),
        rules_map.get(&root_type).unwrap().clone(),
    );

    // If there are any external tokens, generate a header to include by the custom scanner.
    if metadata.scanner_c.is_some() {
        let external_token_types: String = external_token_types
            .into_iter()
            .map(|mut s| {
                s += ",\n";
                s
            })
            .collect();
        let other_header = metadata.extra_headers.insert(
            "token_types".to_owned(),
            format!(
                "enum TokenType {{
                    {external_token_types}
                }};"
            ),
        );
        assert!(other_header.is_none())
    }

    (
        json!({
            "name": grammar_name,
            "word": word_rule,
            "rules": rules_map,
            "extras": extras_list,
            "externals": externals_list
        }),
        metadata,
    )
}
