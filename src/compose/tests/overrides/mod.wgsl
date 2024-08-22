#define_import_path mod

// This is a comment

pub virtual fn inner(arg: f32) -> f32 {
    return arg * 2.0;
}

pub fn outer() -> f32 {
    return inner(1.0);
}
