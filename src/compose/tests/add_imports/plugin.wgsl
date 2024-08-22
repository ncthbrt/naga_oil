use overridable;

pub patch fn overridable::func() -> f32 {
    return overridable::func() + 1.0;
}
