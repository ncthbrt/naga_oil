Naga Organised Integration Library (`naga-oil`) is a crate for combining and manipulating shaders.

- `compose` presents a modular shader composition framework
- `prune` strips shaders down to required parts

and probably less useful externally:

- `derive` allows importing of items from multiple shaders into a single shader
- `redirect` modifies a shader by substituting function calls and modifying bindings

# Compose

the compose module allows construction of shaders from modules (which are themselves shaders).

it does this by treating shaders as modules, and

- building each module independently to naga IR
- creating "header" files for each supported language, which are used to build dependent modules/shaders
- making final shaders by combining the shader IR with the IR for imported modules

for multiple small shaders with large common imports, this can be faster than parsing the full source for each shader, and it allows for constructing shaders in a cleaner modular manner with better scope control.

## imports

shaders can be added to the composer as modules. this makes their types, constants, variables and functions available to modules/shaders that import them. note that importing a module will affect the final shader's global state if the module defines globals variables with bindings.

modules may include a `#define_import_path` directive that names the module:

```wgsl
#define_import_path my_module

fn my_func() -> f32 {
	return 1.0;
}
```

alternatively the module name can be specified as an argument to `Composer::add_composable_module`.

shaders can then import the module with an `use` directive (with an optional `as` name) :

```wgsl
use my_module;
use my_other_module as mod2;

fn main() -> f32 {
    let x = my_module::my_func();
    let y = mod2::my_other_func();
    return x*y;
}
```

or import a comma-separated list of individual items :

```wgsl
use my_module::{my_func, my_const}

fn main() -> f32 {
    return my_func(my_const);
}
```

Some rust-style import syntax is supported, and items can be directly imported using the fully qualified item name :

```wgsl
use my_package::{
    first_module::{item_one as item, item_two},
    second_module::submodule,
}

fn main() -> f32 {
    return item + item_two + submodule::subitem + my_package::third_module::item;
}
```

`module::self` and `module::*` are not currently supported.

imports can be nested - modules may import other modules, but not recursively. when a new module is added, all its `use`s must already have been added.
the same module can be imported multiple times by different modules in the import tree.
there is no overlap of namespaces, so the same function names (or type, constant, or variable names) may be used in different modules.

note: the final shader will include the required dependencies (bindings, globals, consts, other functions) of any imported items that are used, but will not include the rest of the imported module.


## languages

modules can we written in GLSL or WGSL. shaders with entry points can be imported as modules (provided they have a `#define_import_path` directive). entry points are available to call from imported modules either via their name (for WGSL) or via `module::main` (for GLSL).

final shaders can also be written in GLSL or WGSL. for GLSL users must specify whether the shader is a vertex shader or fragment shader via the ShaderType argument (GLSL compute shaders are not supported).

## preprocessing

when generating a final shader or adding a composable module, a set of `shader_def` string/value pairs must be provided. The value can be a bool (`ShaderDefValue::Bool`), an i32 (`ShaderDefValue::Int`) or a u32 (`ShaderDefValue::UInt`).

these allow conditional compilation of parts of modules and the final shader. conditional compilation is performed with `#if` / `#ifdef` / `#ifndef`, `#else` and `#endif` preprocessor directives:

```wgsl
fn get_number() -> f32 {
    #ifdef BIG_NUMBER
        return 999.0;
    #else
        return 0.999;
    #endif
}
```

the `#ifdef` directive matches when the def name exists in the input binding set (regardless of value). the `#ifndef` directive is the reverse.

the `#if` directive requires a def name, an operator, and a value for comparison:

- the def name must be a provided `shader_def` name.
- the operator must be one of `==`, `!=`, `>=`, `>`, `<`, `<=`
- the value must be an integer literal if comparing to a `ShaderDefValue::Int` or `ShaderDefValue::Uint`, or `true` or `false` if comparing to a `ShaderDef::Bool`.

shader defs can also be used in the shader source with `#SHADER_DEF` or `#{SHADER_DEF}`, and will be substituted for their value.

the preprocessor branching directives (`ifdef`, `ifndef` and `if`) can be prefixed with `#else` to create more complex control flows:

```wgsl
fn get_number() -> f32 {
    #ifdef BIG_NUMBER
        return 999.0;
    #else if USER_NUMBER > 1
        return f32(#USER_NUMBER)
    #else
        return 0.999;
    #endif
}
```

shader defs can be created or overridden at the start of the top-level shader with the `#define` directive:

```wgsl
#define USER_NUMBER 42
```

the created value will default to `true` if not specified.

## error reporting

codespan reporting for errors is available using the error `emit_to_string` method. this requires validation to be enabled, which is true by default. `Composer::non_validating()` produces a non-validating composer that is not able to give accurate error reporting.

# prune

- strips dead code and bindings from shaders based on specified required output. intended to be used for building reduced depth and/or normal shaders from arbitrary vertex/fragment shaders.

proper docs tbd

# redirect

- redirects function calls
- wip: rebinds global bindings
- todo one day: translate between uniform, texture and buffer accesses so shaders written for direct passes can be used in indirect

proper docs tbd

# derive

- builds a single self-contained naga module out of parts of one or more existing modules

proper docs tbd
