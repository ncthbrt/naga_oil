#define_import_path include

pub fn non_ep(f: f32) -> f32 {
    return f * 2.0;
}

@fragment
pub fn fragment(
    @builtin(position) frag_coord: vec4<f32>,
) -> @location(0) vec4<f32> {
    return vec4<f32>(1.5 * frag_coord);
}
