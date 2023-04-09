use std::collections::{HashMap, HashSet};

use regex::Regex;
use tracing::warn;

use super::{ComposerErrorInner, ImportDefWithOffset, ImportDefinition, ShaderDefValue};

#[derive(Debug)]
pub struct Preprocessor {
    version_regex: Regex,
    ifdef_regex: Regex,
    ifndef_regex: Regex,
    ifop_regex: Regex,
    else_regex: Regex,
    endif_regex: Regex,
    def_regex: Regex,
    def_regex_delimited: Regex,
    import_custom_path_as_regex: Regex,
    import_custom_path_regex: Regex,
    import_items_regex: Regex,
    identifier_regex: Regex,
    define_import_path_regex: Regex,
}

impl Default for Preprocessor {
    fn default() -> Self {
        Self {
            version_regex: Regex::new(r"^\s*#version\s+([0-9]+)").unwrap(),
            ifdef_regex: Regex::new(r"^\s*#\s*ifdef\s+([\w|\d|_]+)").unwrap(),
            ifndef_regex: Regex::new(r"^\s*#\s*ifndef\s+([\w|\d|_]+)").unwrap(),
            ifop_regex: Regex::new(r"^\s*#\s*if\s+([\w|\d|_]+)\s*([^\s]*)\s*([\w|\d]+)").unwrap(),
            else_regex: Regex::new(r"^\s*#\s*else").unwrap(),
            endif_regex: Regex::new(r"^\s*#\s*endif").unwrap(),
            def_regex: Regex::new(r"#\s*([\w|\d|_]+)").unwrap(),
            def_regex_delimited: Regex::new(r"#\s*\{([\w|\d|_]+)\}").unwrap(),
            import_custom_path_as_regex: Regex::new(r"^\s*#\s*import\s+([^\s]+)\s+as\s+([^\s]+)")
                .unwrap(),
            import_custom_path_regex: Regex::new(r"^\s*#\s*import\s+([^\s]+)").unwrap(),
            import_items_regex: Regex::new(
                r"^\s*#\s*from\s+([^\s]+)\s+import\s*((?:[\w|\d|_]+)(?:\s*,\s*[\w|\d|_]+)*)",
            )
            .unwrap(),
            identifier_regex: Regex::new(r"([\w|\d|_]+)").unwrap(),
            define_import_path_regex: Regex::new(r"^\s*#\s*define_import_path\s+([^\s]+)").unwrap(),
        }
    }
}

impl Preprocessor {
    // process #if[(n)?def]? / #else / #endif preprocessor directives,
    // strip module name and imports
    // also strip "#version xxx"
    pub fn preprocess_defs(
        &self,
        shader_str: &str,
        shader_defs: &HashMap<String, ShaderDefValue>,
        mut validate_len: bool,
    ) -> Result<(Option<String>, String, Vec<ImportDefWithOffset>), ComposerErrorInner> {
        let mut imports = Vec::new();
        let mut scopes = vec![true];
        let mut final_string = String::new();
        let mut name = None;
        let mut offset = 0;

        #[cfg(debug)]
        let len = shader_str.len();

        // this code broadly stolen from bevy_render::ShaderProcessor
        for line in shader_str.lines() {
            let mut output = false;
            if let Some(cap) = self.version_regex.captures(line) {
                let v = cap.get(1).unwrap().as_str();
                if v != "440" && v != "450" {
                    return Err(ComposerErrorInner::GlslInvalidVersion(offset));
                }
            } else if let Some(cap) = self.ifdef_regex.captures(line) {
                let def = cap.get(1).unwrap();
                scopes.push(*scopes.last().unwrap() && shader_defs.contains_key(def.as_str()));
            } else if let Some(cap) = self.ifndef_regex.captures(line) {
                let def = cap.get(1).unwrap();
                scopes.push(*scopes.last().unwrap() && !shader_defs.contains_key(def.as_str()));
            } else if let Some(cap) = self.ifop_regex.captures(line) {
                let def = cap.get(1).unwrap();
                let op = cap.get(2).unwrap();
                let val = cap.get(3).unwrap();

                fn act_on<T: Eq + Ord>(
                    a: T,
                    b: T,
                    op: &str,
                    pos: usize,
                ) -> Result<bool, ComposerErrorInner> {
                    match op {
                        "==" => Ok(a == b),
                        "!=" => Ok(a != b),
                        ">" => Ok(a > b),
                        ">=" => Ok(a >= b),
                        "<" => Ok(a < b),
                        "<=" => Ok(a <= b),
                        _ => Err(ComposerErrorInner::UnknownShaderDefOperator {
                            pos,
                            operator: op.to_string(),
                        }),
                    }
                }

                let def_value =
                    shader_defs
                        .get(def.as_str())
                        .ok_or(ComposerErrorInner::UnknownShaderDef {
                            pos: offset,
                            shader_def_name: def.as_str().to_string(),
                        })?;
                let new_scope = match def_value {
                    ShaderDefValue::Bool(def_value) => {
                        let val = val.as_str().parse().map_err(|_| {
                            ComposerErrorInner::InvalidShaderDefComparisonValue {
                                pos: offset,
                                shader_def_name: def.as_str().to_string(),
                                value: val.as_str().to_string(),
                                expected: "bool".to_string(),
                            }
                        })?;
                        act_on(*def_value, val, op.as_str(), offset)?
                    }
                    ShaderDefValue::Int(def_value) => {
                        let val = val.as_str().parse().map_err(|_| {
                            ComposerErrorInner::InvalidShaderDefComparisonValue {
                                pos: offset,
                                shader_def_name: def.as_str().to_string(),
                                value: val.as_str().to_string(),
                                expected: "int".to_string(),
                            }
                        })?;
                        act_on(*def_value, val, op.as_str(), offset)?
                    }
                    ShaderDefValue::UInt(def_value) => {
                        let val = val.as_str().parse().map_err(|_| {
                            ComposerErrorInner::InvalidShaderDefComparisonValue {
                                pos: offset,
                                shader_def_name: def.as_str().to_string(),
                                value: val.as_str().to_string(),
                                expected: "int".to_string(),
                            }
                        })?;
                        act_on(*def_value, val, op.as_str(), offset)?
                    }
                };
                scopes.push(*scopes.last().unwrap() && new_scope);
            } else if self.else_regex.is_match(line) {
                let mut is_parent_scope_truthy = true;
                if scopes.len() > 1 {
                    is_parent_scope_truthy = scopes[scopes.len() - 2];
                }
                if let Some(last) = scopes.last_mut() {
                    *last = is_parent_scope_truthy && !*last;
                }
            } else if self.endif_regex.is_match(line) {
                scopes.pop();
                if scopes.is_empty() {
                    return Err(ComposerErrorInner::TooManyEndIfs(offset));
                }
            } else if let Some(cap) = self.define_import_path_regex.captures(line) {
                name = Some(cap.get(1).unwrap().as_str().to_string());
            } else if *scopes.last().unwrap() {
                if let Some(cap) = self.import_custom_path_as_regex.captures(line) {
                    imports.push(ImportDefWithOffset {
                        definition: ImportDefinition {
                            import: cap.get(1).unwrap().as_str().to_string(),
                            as_name: Some(cap.get(2).unwrap().as_str().to_string()),
                            items: Default::default(),
                        },
                        offset,
                    });
                } else if let Some(cap) = self.import_custom_path_regex.captures(line) {
                    imports.push(ImportDefWithOffset {
                        definition: ImportDefinition {
                            import: cap.get(1).unwrap().as_str().to_string(),
                            as_name: None,
                            items: Default::default(),
                        },
                        offset,
                    });
                } else if let Some(cap) = self.import_items_regex.captures(line) {
                    imports.push(ImportDefWithOffset {
                        definition: ImportDefinition {
                            import: cap.get(1).unwrap().as_str().to_string(),
                            as_name: None,
                            items: Some(
                                self.identifier_regex
                                    .captures_iter(cap.get(2).unwrap().as_str())
                                    .map(|ident_cap| ident_cap.get(1).unwrap().as_str().to_owned())
                                    .collect(),
                            ),
                        },
                        offset,
                    });
                } else {
                    let mut line_with_defs = line.to_string();
                    for capture in self.def_regex.captures_iter(line) {
                        let def = capture.get(1).unwrap();
                        if let Some(def) = shader_defs.get(def.as_str()) {
                            line_with_defs = self
                                .def_regex
                                .replace(&line_with_defs, def.value_as_string())
                                .to_string();
                        }
                    }
                    for capture in self.def_regex_delimited.captures_iter(line) {
                        let def = capture.get(1).unwrap();
                        if let Some(def) = shader_defs.get(def.as_str()) {
                            line_with_defs = self
                                .def_regex_delimited
                                .replace(&line_with_defs, def.value_as_string())
                                .to_string();
                        }
                    }
                    final_string.push_str(&line_with_defs);
                    let diff = line.len() as i32 - line_with_defs.len() as i32;
                    if diff > 0 {
                        final_string.extend(std::iter::repeat(" ").take(diff as usize));
                    } else if diff < 0 && validate_len {
                        // this sucks
                        warn!("source code map requires shader_def values to be no longer than the corresponding shader_def name, error reporting may not be correct:\noriginal: {}\nreplaced: {}", line, line_with_defs);
                        validate_len = false;
                    }
                    output = true;
                }
            }

            if !output {
                // output spaces for removed lines to keep spans consistent (errors report against substituted_source, which is not preprocessed)
                final_string.extend(std::iter::repeat(" ").take(line.len()));
            }
            final_string.push('\n');
            offset += line.len() + 1;
        }

        if scopes.len() != 1 {
            return Err(ComposerErrorInner::NotEnoughEndIfs(offset));
        }

        #[cfg(debug)]
        if validate_len {
            let revised_len = final_string.len();
            assert_eq!(len, revised_len);
        }

        Ok((name, final_string, imports))
    }

    // extract module name and imports
    pub fn get_preprocessor_data(
        &self,
        shader_str: &str,
    ) -> (Option<String>, Vec<ImportDefWithOffset>) {
        let mut imports = Vec::new();
        let mut name = None;
        let mut offset = 0;
        for line in shader_str.lines() {
            if let Some(cap) = self.import_custom_path_as_regex.captures(line) {
                imports.push(ImportDefWithOffset {
                    definition: ImportDefinition {
                        import: cap.get(1).unwrap().as_str().to_string(),
                        as_name: Some(cap.get(2).unwrap().as_str().to_string()),
                        items: Default::default(),
                    },
                    offset,
                });
            } else if let Some(cap) = self.import_custom_path_regex.captures(line) {
                imports.push(ImportDefWithOffset {
                    definition: ImportDefinition {
                        import: cap.get(1).unwrap().as_str().to_string(),
                        as_name: None,
                        items: Default::default(),
                    },
                    offset,
                });
            } else if let Some(cap) = self.import_items_regex.captures(line) {
                imports.push(ImportDefWithOffset {
                    definition: ImportDefinition {
                        import: cap.get(1).unwrap().as_str().to_string(),
                        as_name: None,
                        items: Some(
                            self.identifier_regex
                                .captures_iter(cap.get(2).unwrap().as_str())
                                .map(|ident_cap| ident_cap.get(1).unwrap().as_str().to_owned())
                                .collect(),
                        ),
                    },
                    offset,
                });
            } else if let Some(cap) = self.define_import_path_regex.captures(line) {
                name = Some(cap.get(1).unwrap().as_str().to_string());
            }

            offset += line.len() + 1;
        }

        (name, imports)
    }

    pub fn effective_defs(&self, source: &str) -> HashSet<String> {
        let mut effective_defs = HashSet::default();

        for line in source.lines() {
            if let Some(cap) = self.ifdef_regex.captures(line) {
                let def = cap.get(1).unwrap();
                effective_defs.insert(def.as_str().to_owned());
            }
            if let Some(cap) = self.ifndef_regex.captures(line) {
                let def = cap.get(1).unwrap();
                effective_defs.insert(def.as_str().to_owned());
            }
        }

        effective_defs
    }
}

#[cfg(test)]
mod test {
    use super::*;

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

        let result_missing = processor.preprocess_defs(
            WGSL,
            &[("TEXTURE".to_owned(), ShaderDefValue::Bool(true))].into(),
            true,
        );

        let expected: Result<
            (Option<String>, String, Vec<ImportDefWithOffset>),
            ComposerErrorInner,
        > = Err(ComposerErrorInner::UnknownShaderDefOperator {
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
            .preprocess_defs(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Int(3))].into(),
                true,
            )
            .unwrap();
        assert_eq!(result_eq.1, EXPECTED_EQ);

        let result_neq = processor
            .preprocess_defs(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Int(7))].into(),
                true,
            )
            .unwrap();
        assert_eq!(result_neq.1, EXPECTED_NEQ);

        let result_missing = processor.preprocess_defs(WGSL, &Default::default(), true);

        let expected_err: Result<
            (Option<String>, String, Vec<ImportDefWithOffset>),
            ComposerErrorInner,
        > = Err(ComposerErrorInner::UnknownShaderDef {
            pos: 124,
            shader_def_name: "TEXTURE".to_string(),
        });
        assert_eq!(format!("{result_missing:?}"), format!("{expected_err:?}"),);

        let result_wrong_type = processor.preprocess_defs(
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
            .preprocess_defs(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Bool(true))].into(),
                true,
            )
            .unwrap();
        assert_eq!(result_eq.1, EXPECTED_EQ);

        let result_neq = processor
            .preprocess_defs(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Bool(false))].into(),
                true,
            )
            .unwrap();
        assert_eq!(result_neq.1, EXPECTED_NEQ);
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
            .preprocess_defs(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Bool(true))].into(),
                true,
            )
            .unwrap();
        assert_eq!(result_eq.1, EXPECTED_EQ);

        let result_neq = processor
            .preprocess_defs(
                WGSL,
                &[("TEXTURE".to_string(), ShaderDefValue::Bool(false))].into(),
                true,
            )
            .unwrap();
        assert_eq!(result_neq.1, EXPECTED_NEQ);

        let result_missing = processor.preprocess_defs(WGSL, &[].into(), true);
        let expected_err: Result<
            (Option<String>, String, Vec<ImportDefWithOffset>),
            ComposerErrorInner,
        > = Err(ComposerErrorInner::UnknownShaderDef {
            pos: 124,
            shader_def_name: "TEXTURE".to_string(),
        });
        assert_eq!(format!("{result_missing:?}"), format!("{expected_err:?}"),);

        let result_wrong_type = processor.preprocess_defs(
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
            .preprocess_defs(
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
        assert_eq!(result.1, EXPECTED_REPLACED);
    }
}
