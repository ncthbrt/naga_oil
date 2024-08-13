#define_import_path imported_entrypoint
#extend middle

@fragment
fn fragment(
    @builtin(position) frag_coord: vec4<f32>,
) -> @location(0) vec4<f32>  {
    return vec4<f32>(top::mult_two(3.0));
}
