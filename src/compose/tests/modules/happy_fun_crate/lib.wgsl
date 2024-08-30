pub mod happy_submodule_1;
mod happy_submodule_2;

use happy_submodule_2::*;

@group(0) @binding(0)
pub var<storage, read_write> buffer: f32;

@compute @workgroup_size(1, 1, 1)
pub fn run_test() {
    let res = happy_submodule_1::hello() + goodbye();
    buffer = res;
}
