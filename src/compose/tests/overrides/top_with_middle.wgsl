#define_import_path test_module

use patchset middle;
use mod;

fn entry_point() -> f32 {
    return mod::outer();
}
