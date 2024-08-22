#define_import_path test_module

use a::b as partial_path;
use a::b::c as full_path;

pub fn entry_point() -> f32 {
    return a::x::square(partial_path::c::triple(full_path::C));
}
