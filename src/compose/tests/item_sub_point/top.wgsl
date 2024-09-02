use mod Frag, fragment

@fragment
pub fn main() -> @location(0) f32 {
    var f: Frag;
    f.fragment = 3.0;
    return fragment(f);
}
