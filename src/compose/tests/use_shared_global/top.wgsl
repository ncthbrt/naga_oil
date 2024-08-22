use mod;

pub fn add() {
    mod::a += 1.0;
}

pub fn main() -> f32 {
    add();
    add();
    return mod::a;
}
