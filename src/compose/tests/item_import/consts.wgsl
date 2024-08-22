#define_import_path consts

pub const X: u32 = 1u;
pub const Y: u32 = 2u;
pub const Z: u32 = 3u;

@group(0) @binding(0)
pub var something: sampler;

pub fn double(in: u32) -> u32 {
    return in * 2u;
}

pub fn triple(in: u32) -> u32 {
    return in * 3u;
}
