#extend imported_entrypoint

override fn top::func() -> f32 {
    return 4.0;
}

override fn funcTwo() -> f32 {
    return funcTwo() * 6.0;
}
