use include_cargo_toml::include_toml;

pub const CRATE_NAME: &str = include_toml!("package"."name");
