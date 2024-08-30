#define_import_path test_module
use mod::{a, b, c}

pub fn entry_point() -> f32 {
    return f32(a + b + c);
}
