#define_import_path mod

#ifdef DEF_ONE
pub const a: u32 = 1u;
#else
pub const a: u32 = 0u;
#endif

#ifndef DEF_TWO
pub const b: u32 = 0u;
#else
pub const b: u32 = 2u;
#endif

#if DEF_THREE == true
pub const c: u32 = 4u;
#else
pub const c: u32 = 0u;
#endif
