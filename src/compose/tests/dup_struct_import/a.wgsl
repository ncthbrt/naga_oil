#define_import_path a
#import struct

pub fn a() -> struct::MyStruct {
    var s_a: struct::MyStruct;
    s_a.value = 1.0;
    return s_a;
}
