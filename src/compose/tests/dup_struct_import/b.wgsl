#define_import_path b
#import struct

pub fn b() -> struct::MyStruct {
    var s_b: struct::MyStruct;
    s_b.value = 2.0;
    return s_b;
}
