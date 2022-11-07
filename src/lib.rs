//! This crate provides a macro called [`include_toml!`] which parses properties of `Cargo.toml` at compile time.

use std::path::{Path, PathBuf};

use cargo_toml::Manifest;
use proc_macro::TokenStream;
use proc_macro2::{Literal, Span as Span2, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseBuffer},
    parse_macro_input,
    token::Dot,
    Error as SynError, Lit, LitBool,
};
use toml::Value;

/// Helper that stores either integer or string.
///
/// Used to create vector of indexing items in [`TomlIndex`].
enum Index {
    Int(usize),
    Str(String),
}

/// Struct that parses input of [`include_toml`].
///
/// Input should consist of either string literals or integers separated by dots.
struct TomlIndex(Vec<Index>);

impl Parse for TomlIndex {
    fn parse(input: &ParseBuffer) -> Result<Self, SynError> {
        let mut another_one = true;
        let mut index = Vec::new();
        while another_one {
            index.push(match input.parse::<Lit>() {
                Ok(lit) => match lit {
                    Lit::Str(lit_str) => Index::Str(lit_str.value()),
                    Lit::Int(lit_int) => Index::Int(
                        lit_int
                            .base10_digits()
                            .parse()
                            .map_err(|e| SynError::new(input.span(), e))?,
                    ),
                    _ => return Err(SynError::new(input.span(), "Unsupported literal")),
                },
                Err(e) => {
                    return Err(SynError::new(
                        input.span(),
                        format!("Cannot parse index item: {}", e),
                    ))
                }
            });
            if input.parse::<Dot>().is_err() {
                another_one = false;
            }
        }
        Ok(Self(index))
    }
}

/// Converts any TOML value to valid Rust types.
fn toml_to_ts(input: Value) -> TokenStream2 {
    match input {
        Value::String(s) => Lit::new(Literal::string(&s)).to_token_stream(),
        Value::Integer(i) => Lit::new(Literal::i64_suffixed(i)).to_token_stream(),
        Value::Float(f) => Lit::new(Literal::f64_suffixed(f)).to_token_stream(),
        Value::Datetime(d) => Lit::new(Literal::string(&d.to_string())).to_token_stream(),
        Value::Boolean(b) => Lit::Bool(LitBool::new(b, Span2::call_site())).to_token_stream(),
        Value::Array(a) => {
            let mut ts = TokenStream2::new();
            for value in a {
                let v = toml_to_ts(value);
                ts.extend(quote! (#v,));
            }
            quote! ((#ts))
        }
        Value::Table(t) => {
            let mut ts = TokenStream2::new();
            for (key, value) in t {
                let v = toml_to_ts(value);
                ts.extend(quote! ((#key, #v)));
            }
            quote! ((#ts))
        }
    }
}

/// Parse `Cargo.toml` at compile time.
///
/// # TOML to Rust conversion
///
/// - TOML [string](Value::String) -> Rust [`&str`]
/// - TOML [integer](Value::Integer) -> Rust [`i64`]
/// - TOML [float](Value::Float) -> Rust [`f64`]
/// - TOML [boolean](Value::Boolean) -> Rust [`bool`]
/// - TOML [datetime](Value::Datetime) -> Rust [`&str`]
/// - TOML [array](Value::Array) -> Rust tuple \
///     TOML arrays can hold different types, Rust [`Vec`]s can't.
/// - TOML [table](Value::Table) -> Rust tuple \
///     TOML tables can hold different types, Rust [`HashMap`](std::collections::HashMap)s can't.
///
/// # Example
///
/// Keys to index `Cargo.toml` are parsed as string literals and array / table indexes are parsed as integer literals:
///
/// ```rust
/// use include_cargo_toml::include_toml;
///
/// assert_eq!(
///     include_toml!("package"."version"),
///     "0.1.0"
/// );
/// assert_eq!(
///     include_toml!("package"."name"),
///     "include-cargo-toml"
/// );
/// // indexing array with literal 2
/// assert_eq!(
///     include_toml!("package"."keywords".2),
///     "Cargo-toml"
/// );
/// assert_eq!(
///     include_toml!("lib"."proc-macro"),
///     true
/// );
/// ```
///
/// Because TOML's arrays and tables do not work like [`Vec`] and [`HashMap`](std::collections::HashMap), tuples are used.
///
/// ```rust
/// use include_cargo_toml::include_toml;
///
/// assert_eq!(
///     include_toml!("package"."keywords"),
///     ("macro", "version", "Cargo-toml", "compile-time", "parse")
/// );
/// ```
///
/// Leading or trailing dots are not allowed:
///
/// ```rust,compile_fail
/// use include_cargo_toml::include_toml;
///
/// let this_fails = include_toml!(."package"."name");
/// let this_fails_too = include_toml!("package"."name".);
/// ```
#[proc_macro]
pub fn include_toml(input: TokenStream) -> TokenStream {
    // parse input
    let input: TomlIndex = parse_macro_input!(input);

    _include_toml("Cargo.toml", input)
        .unwrap_or_else(SynError::into_compile_error)
        .into()
}

#[proc_macro]
/// Same as `include_toml` but includes main project Cargo.toml
/// E.g. when building a bin crate, returns bin crates's Cargo.toml even if used from a dependency crate
/// Actually current dir is retrieved using PWD env var, could fail in some build environments
pub fn include_main_toml(input: TokenStream) -> TokenStream {
    // parse input
    let input: TomlIndex = parse_macro_input!(input);

    // match std::env::current_dir() {
    match option_env!("PWD").ok_or("Can't find PWD env var") {
        Ok(pwd) => {
            let mut main_dir = PathBuf::from(pwd);
            main_dir.push("Cargo.toml");

            _include_toml(main_dir, input)
                .unwrap_or_else(SynError::into_compile_error)
                .into()
        }
        Err(e) => SynError::new(Span2::call_site(), e)
            .into_compile_error()
            .into(),
    }
}

fn _include_toml(path: impl AsRef<Path>, input: TomlIndex) -> Result<TokenStream2, SynError> {
    // get Cargo.toml contents
    // using Manifest here eliminates subfolder problems
    let cargo_toml: Manifest = Manifest::from_path_with_metadata(path)
        .map_err(|e| SynError::new(Span2::call_site(), e))?;
    // parse Cargo.toml contents as TOML
    let mut cargo_toml_toml: Value =
        Value::try_from(cargo_toml).map_err(|e| SynError::new(Span2::call_site(), e))?;
    // get wanted field by traversing through TOML structure
    for item in input.0 {
        match item {
            Index::Int(index) => {
                cargo_toml_toml = cargo_toml_toml[index].clone();
            }
            Index::Str(index) => {
                cargo_toml_toml = cargo_toml_toml[index].clone();
            }
        }
    }
    // convert toml value to TokenStream
    Ok(toml_to_ts(cargo_toml_toml))
}
