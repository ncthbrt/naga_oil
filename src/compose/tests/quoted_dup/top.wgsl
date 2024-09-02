use "quoted_module" as foo;

pub fn myfunc(foo: u32) -> f32 {
    return f32(foo) * 2.0;
}

pub fn main() -> f32 {
    return myfunc(1u) + foo::foo();
}
