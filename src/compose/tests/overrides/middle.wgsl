#define_import_path middle

use mod;

pub patch fn mod::inner(arg: f32) -> f32 {
    return arg * 3.0;
}
