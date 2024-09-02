#version 450

use wgsl_module



void main() {
    gl_Position = vec4(wgsl_module::wgsl_func());
}