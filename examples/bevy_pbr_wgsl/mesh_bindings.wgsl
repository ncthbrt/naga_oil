#define_import_path bevy_pbr::mesh_bindings

use bevy_pbr::mesh_types as Types

@group(2) @binding(0)
var<uniform> mesh: Types::Mesh;
