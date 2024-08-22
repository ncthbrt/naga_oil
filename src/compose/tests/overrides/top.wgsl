use mod;

// This is a comment
pub patch fn mod::inner(arg: f32) -> f32 {
    return arg * 3.0;
}

fn top() -> f32 {
    return mod::outer();
}
