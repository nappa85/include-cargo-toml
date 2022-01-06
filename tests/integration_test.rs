#![cfg(test)]

mod submodule;

/// Tests whether the macro is independent of the folder structure.
/// If this passes, the macro can be used inside submodules / folders.
#[test]
pub fn load_version_from_inner_folder() {
    assert_eq!(submodule::CRATE_NAME, "include-cargo-toml");
}
