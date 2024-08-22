#import a
#import b

pub fn main() -> f32 {
    let a = a::a();
    let b = b::b();
    return a.value / b.value;
}
