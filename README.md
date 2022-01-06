# Parse all properties of `Cargo.toml` at compile time

This Rust crate provides a macro to parse `Cargo.toml`.

This can be useful to implement a `--version` flag that does not need to be updated manually each time a new version is released.

Using [clap](https://crates.io/crates/clap) might be a little bit of an overload in some cases.
