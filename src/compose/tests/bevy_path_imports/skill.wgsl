use "shaders/skills/shared.wgsl" Vertex, VertexOutput

#if EFFECT_ID == 0
    use "shaders/skills/sound.wgsl" frag, vert
#else if EFFECT_ID == 1
    use "shaders/skills/orb.wgsl" frag, vert
#else if EFFECT_ID == 2
    use "shaders/skills/slash.wgsl" frag, vert
#else if EFFECT_ID == 3
    use "shaders/skills/railgun_trail.wgsl" frag, vert
#else if EFFECT_ID == 4
    use "shaders/skills/magic_arrow.wgsl" frag, vert
#else if EFFECT_ID == 5
    use "shaders/skills/hit.wgsl" frag, vert
#else if EFFECT_ID == 6
    use "shaders/skills/lightning_ring.wgsl" frag, vert
#else if EFFECT_ID == 7
    use "shaders/skills/lightning.wgsl" frag, vert
#endif

use something_unused

@fragment
pub fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    return frag(in);
}

@vertex
pub fn vertex(vertex: Vertex) -> VertexOutput {
    return vert(vertex);
}
