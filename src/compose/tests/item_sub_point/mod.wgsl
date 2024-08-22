#define_import_path mod

pub struct Frag {
    fragment: f32,
}

pub fn fragment(f: Frag) -> f32 {
    return f.fragment * 2.0;
}
