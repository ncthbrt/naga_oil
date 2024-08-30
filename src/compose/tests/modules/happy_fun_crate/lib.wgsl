pub mod happy_submodule_1;
mod happy_submodule_2;

use happy_submodule_2::*;

pub fn entry() -> f32 {
    let res = happy_submodule_1::hello() + goodbye();
    return res;
}
