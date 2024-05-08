use std::collections::{HashMap, HashSet};

use indexmap::IndexMap;
use winnow::Parser;

use super::{
    composer::ImportDefWithOffset,
    old_parse_imports::substitute_identifiers,
    preprocess1::{self, input_new},
    ComposerErrorInner, ShaderDefValue,
};

#[derive(Debug)]
pub struct Preprocessor;

#[derive(Debug)]
pub struct PreprocessorMetaData {
    pub name: Option<String>,
    pub imports: Vec<ImportDefWithOffset>,
    pub defines: HashMap<String, ShaderDefValue>,
    pub effective_defs: HashSet<String>,
}

#[cfg(test)]
mod test {
    use super::*;

    #[rustfmt::skip]
    const WGSL_ELSE_IFDEF: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;

#ifdef TEXTURE
// Main texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#else ifdef SECOND_TEXTURE
// Second texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#else ifdef THIRD_TEXTURE
// Third texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#else
@group(1) @binding(0)
var sprite_texture: texture_2d_array<f32>;
#endif

struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};

@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

    //preprocessor tests
    #[test]
    fn process_shader_def_unknown_operator() {
        #[rustfmt::skip]
        const WGSL: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
#if TEXTURE !! true
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#endif
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        let processor = Preprocessor::default();

        let result_missing = processor.preprocess(
            WGSL,
            &[("TEXTURE".to_owned(), ShaderDefValue::Bool(true))].into(),
            true,
        );

        let expected: Result<Preprocessor, ComposerErrorInner> =
            Err(ComposerErrorInner::UnknownShaderDefOperator {
                pos: 124,
                operator: "!!".to_string(),
            });

        assert_eq!(format!("{result_missing:?}"), format!("{expected:?}"),);
    }
    #[test]
    fn process_shader_def_equal_int() {
        #[rustfmt::skip]
        const WGSL: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
#if TEXTURE == 3
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#endif
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_EQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
                
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
      
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_NEQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
                
                     
                                    
      
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        let result_eq = processor
            .preprocess(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Int(3))].into(),
                true,
            )
            .unwrap();
        assert_eq!(result_eq.preprocessed_source, EXPECTED_EQ);

        let result_neq = processor
            .preprocess(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Int(7))].into(),
                true,
            )
            .unwrap();
        assert_eq!(result_neq.preprocessed_source, EXPECTED_NEQ);

        let result_missing = processor.preprocess(WGSL, &Default::default(), true);

        let expected_err: Result<
            (Option<String>, String, Vec<ImportDefWithOffset>),
            ComposerErrorInner,
        > = Err(ComposerErrorInner::UnknownShaderDef {
            pos: 124,
            shader_def_name: "TEXTURE".to_string(),
        });
        assert_eq!(format!("{result_missing:?}"), format!("{expected_err:?}"),);

        let result_wrong_type = processor.preprocess(
            WGSL,
            &[("TEXTURE".to_string(), ShaderDefValue::Bool(true))].into(),
            true,
        );

        let expected_err: Result<
            (Option<String>, String, Vec<ImportDefWithOffset>),
            ComposerErrorInner,
        > = Err(ComposerErrorInner::InvalidShaderDefComparisonValue {
            pos: 124,
            shader_def_name: "TEXTURE".to_string(),
            expected: "bool".to_string(),
            value: "3".to_string(),
        });

        assert_eq!(
            format!("{result_wrong_type:?}"),
            format!("{expected_err:?}")
        );
    }

    #[test]
    fn process_shader_def_equal_bool() {
        #[rustfmt::skip]
        const WGSL: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
#if TEXTURE == true
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#endif
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_EQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
                   
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
      
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_NEQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
                   
                     
                                    
      
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        let result_eq = processor
            .preprocess(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Bool(true))].into(),
                true,
            )
            .unwrap();
        assert_eq!(result_eq.preprocessed_source, EXPECTED_EQ);

        let result_neq = processor
            .preprocess(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Bool(false))].into(),
                true,
            )
            .unwrap();
        assert_eq!(result_neq.preprocessed_source, EXPECTED_NEQ);
    }

    #[test]
    fn process_shader_def_not_equal_bool() {
        #[rustfmt::skip]
        const WGSL: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
#if TEXTURE != false
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#endif
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_EQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
                    
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
      
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_NEQ: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
                    
                     
                                    
      
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        let result_eq = processor
            .preprocess(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Bool(true))].into(),
                true,
            )
            .unwrap();
        assert_eq!(result_eq.preprocessed_source, EXPECTED_EQ);

        let result_neq = processor
            .preprocess(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Bool(false))].into(),
                true,
            )
            .unwrap();
        assert_eq!(result_neq.preprocessed_source, EXPECTED_NEQ);

        let result_missing = processor.preprocess(WGSL, &[].into(), true);
        let expected_err: Result<
            (Option<String>, String, Vec<ImportDefWithOffset>),
            ComposerErrorInner,
        > = Err(ComposerErrorInner::UnknownShaderDef {
            pos: 124,
            shader_def_name: "TEXTURE".to_string(),
        });
        assert_eq!(format!("{result_missing:?}"), format!("{expected_err:?}"),);

        let result_wrong_type = processor.preprocess(
            WGSL,
            &[("TEXTURE".to_string(), ShaderDefValue::Int(7))].into(),
            true,
        );

        let expected_err: Result<
            (Option<String>, String, Vec<ImportDefWithOffset>),
            ComposerErrorInner,
        > = Err(ComposerErrorInner::InvalidShaderDefComparisonValue {
            pos: 124,
            shader_def_name: "TEXTURE".to_string(),
            expected: "int".to_string(),
            value: "false".to_string(),
        });
        assert_eq!(
            format!("{result_wrong_type:?}"),
            format!("{expected_err:?}"),
        );
    }

    #[test]
    fn process_shader_def_replace() {
        #[rustfmt::skip]
        const WGSL: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    var a: i32 = #FIRST_VALUE;
    var b: i32 = #FIRST_VALUE * #SECOND_VALUE;
    var c: i32 = #MISSING_VALUE;
    var d: bool = #BOOL_VALUE;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
        const EXPECTED_REPLACED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    var a: i32 = 5;           
    var b: i32 = 5 * 3;                       
    var c: i32 = #MISSING_VALUE;
    var d: bool = true;       
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        let result = processor
            .preprocess(
                WGSL,
                &[
                    ("BOOL_VALUE".to_string(), ShaderDefValue::Bool(true)),
                    ("FIRST_VALUE".to_string(), ShaderDefValue::Int(5)),
                    ("SECOND_VALUE".to_string(), ShaderDefValue::Int(3)),
                ]
                .into(),
                true,
            )
            .unwrap();
        assert_eq!(result.preprocessed_source, EXPECTED_REPLACED);
    }

    #[test]
    fn process_shader_define_in_shader() {
        #[rustfmt::skip]
        const WGSL: &str = r"
#define NOW_DEFINED
#ifdef NOW_DEFINED
defined
#endif
";

        #[rustfmt::skip]
        const EXPECTED: &str = r"
                   
                  
defined
      
";
        let processor = Preprocessor::default();
        let PreprocessorMetaData {
            defines: shader_defs,
            ..
        } = processor.get_preprocessor_metadata(&WGSL, true).unwrap();
        println!("defines: {:?}", shader_defs);
        let result = processor.preprocess(&WGSL, &shader_defs, true).unwrap();
        assert_eq!(result.preprocessed_source, EXPECTED);
    }

    #[test]
    fn process_shader_define_in_shader_with_value() {
        #[rustfmt::skip]
        const WGSL: &str = r"
#define DEFUINT 1
#define DEFINT -1
#define DEFBOOL false
#if DEFUINT == 1
uint: #DEFUINT
#endif
#if DEFINT == -1
int: #DEFINT
#endif
#if DEFBOOL == false
bool: #DEFBOOL
#endif
";

        #[rustfmt::skip]
        const EXPECTED: &str = r"
                 
                 
                     
                
uint: 1       
      
                
int: -1     
      
                    
bool: false   
      
";
        let processor = Preprocessor::default();
        let PreprocessorMetaData {
            defines: shader_defs,
            ..
        } = processor.get_preprocessor_metadata(&WGSL, true).unwrap();
        println!("defines: {:?}", shader_defs);
        let result = processor.preprocess(&WGSL, &shader_defs, true).unwrap();
        assert_eq!(result.preprocessed_source, EXPECTED);
    }

    #[test]
    fn process_shader_def_else_ifdef_ends_up_in_else() {
        #[rustfmt::skip]
        const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
@group(1) @binding(0)
var sprite_texture: texture_2d_array<f32>;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        let result = processor
            .preprocess(&WGSL_ELSE_IFDEF, &[].into(), true)
            .unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_ifdef_no_match_and_no_fallback_else() {
        #[rustfmt::skip]
        const WGSL_ELSE_IFDEF_NO_ELSE_FALLBACK: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;

#ifdef TEXTURE
// Main texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#else ifdef OTHER_TEXTURE
// Other texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
#endif

struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};

@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";

        #[rustfmt::skip]
    const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        let result = processor
            .preprocess(&WGSL_ELSE_IFDEF_NO_ELSE_FALLBACK, &[].into(), true)
            .unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_ifdef_ends_up_in_first_clause() {
        #[rustfmt::skip]
    const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
              
// Main texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
                          
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};

@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        let result = processor
            .preprocess(
                &WGSL_ELSE_IFDEF,
                &[("TEXTURE".to_string(), ShaderDefValue::Bool(true))].into(),
                true,
            )
            .unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_ifdef_ends_up_in_second_clause() {
        #[rustfmt::skip]
    const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
// Second texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        let result = processor
            .preprocess(
                &WGSL_ELSE_IFDEF,
                &[("SECOND_TEXTURE".to_string(), ShaderDefValue::Bool(true))].into(),
                true,
            )
            .unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_ifdef_ends_up_in_third_clause() {
        #[rustfmt::skip]
    const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
// Third texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        let result = processor
            .preprocess(
                &WGSL_ELSE_IFDEF,
                &[("THIRD_TEXTURE".to_string(), ShaderDefValue::Bool(true))].into(),
                true,
            )
            .unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_ifdef_only_accepts_one_valid_else_ifdef() {
        #[rustfmt::skip]
    const EXPECTED: &str = r"
struct View {
    view_proj: mat4x4<f32>,
    world_position: vec3<f32>,
};
@group(0) @binding(0)
var<uniform> view: View;
// Second texture
@group(1) @binding(0)
var sprite_texture: texture_2d<f32>;
struct VertexOutput {
    @location(0) uv: vec2<f32>,
    @builtin(position) position: vec4<f32>,
};
@vertex
fn vertex(
    @location(0) vertex_position: vec3<f32>,
    @location(1) vertex_uv: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    out.uv = vertex_uv;
    out.position = view.view_proj * vec4<f32>(vertex_position, 1.0);
    return out;
}
";
        let processor = Preprocessor::default();
        let result = processor
            .preprocess(
                &WGSL_ELSE_IFDEF,
                &[
                    ("SECOND_TEXTURE".to_string(), ShaderDefValue::Bool(true)),
                    ("THIRD_TEXTURE".to_string(), ShaderDefValue::Bool(true)),
                ]
                .into(),
                true,
            )
            .unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_ifdef_complicated_nesting() {
        // Test some nesting including #else ifdef statements
        // 1. Enter an #else ifdef
        // 2. Then enter an #else
        // 3. Then enter another #else ifdef

        #[rustfmt::skip]
        const WGSL_COMPLICATED_ELSE_IFDEF: &str = r"
#ifdef NOT_DEFINED
// not defined
#else ifdef IS_DEFINED
// defined 1
#ifdef NOT_DEFINED
// not defined
#else
// should be here
#ifdef NOT_DEFINED
// not defined
#else ifdef ALSO_NOT_DEFINED
// not defined
#else ifdef IS_DEFINED
// defined 2
#endif
#endif
#endif
";

        #[rustfmt::skip]
        const EXPECTED: &str = r"
// defined 1
// should be here
// defined 2
";
        let processor = Preprocessor::default();
        let result = processor
            .preprocess(
                &WGSL_COMPLICATED_ELSE_IFDEF,
                &[("IS_DEFINED".to_string(), ShaderDefValue::Bool(true))].into(),
                true,
            )
            .unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_ifndef() {
        #[rustfmt::skip]
        const INPUT: &str = r"
#ifdef NOT_DEFINED
fail 1
#else ifdef ALSO_NOT_DEFINED
fail 2
#else ifndef ALSO_ALSO_NOT_DEFINED
ok
#else
fail 3
#endif
";

        const EXPECTED: &str = r"ok";
        let processor = Preprocessor::default();
        let result = processor.preprocess(&INPUT, &[].into(), true).unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }

    #[test]
    fn process_shader_def_else_if() {
        #[rustfmt::skip]
        const INPUT: &str = r"
#ifdef NOT_DEFINED
fail 1
#else if x == 1
fail 2
#else if x == 2
ok
#else
fail 3
#endif
";

        const EXPECTED: &str = r"ok";
        let processor = Preprocessor::default();
        let result = processor
            .preprocess(
                &INPUT,
                &[("x".to_owned(), ShaderDefValue::Int(2))].into(),
                true,
            )
            .unwrap();
        assert_eq!(
            result
                .preprocessed_source
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", ""),
            EXPECTED
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
        );
    }
}
