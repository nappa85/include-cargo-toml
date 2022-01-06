//! This crate provides a macro called [`include_toml!`] which parses properties of `Cargo.toml` at compile time.

extern crate cargo_toml;
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;
extern crate toml;

use crate::{
    cargo_toml::Manifest,
    proc_macro::TokenStream,
    proc_macro2::{Literal, Span as Span2, TokenStream as TokenStream2},
    quote::{quote, ToTokens},
    syn::{
        parse::{Parse, ParseBuffer},
        parse_macro_input,
        token::Dot,
        Error as SynError, Lit, LitBool,
    },
    toml::Value,
};

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
                            .expect("Cannot parse literal integer"),
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
            if let Err(_) = input.parse::<Dot>() {
                another_one = false;
            }
        }
        Ok(Self(index))
    }
}

/// Converts any TOML value to valid Rust types.
fn toml_to_ts(input: Value) -> TokenStream2 {
    match input {
        Value::String(s) => Lit::new(Literal::string(&s)).to_token_stream().into(),
        Value::Integer(i) => Lit::new(Literal::i64_suffixed(i)).to_token_stream().into(),
        Value::Float(f) => Lit::new(Literal::f64_suffixed(f)).to_token_stream().into(),
        Value::Datetime(d) => Lit::new(Literal::string(&d.to_string()))
            .to_token_stream()
            .into(),
        Value::Boolean(b) => Lit::Bool(LitBool::new(b, Span2::call_site()))
            .to_token_stream()
            .into(),
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
///     TOML tables can hold different types, Rust [`Vec`]s can't.
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
    // get Cargo.toml contents
    // using Manifest here eliminates subfolder problems
    let cargo_toml: Manifest =
        Manifest::from_path_with_metadata("Cargo.toml").expect("Cannot read Cargo.toml");
    // parse Cargo.toml contents as TOML
    let mut cargo_toml_toml: Value =
        Value::try_from(cargo_toml).expect("Cannot parse Cargo.toml to json");
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
    toml_to_ts(cargo_toml_toml).into()
}
