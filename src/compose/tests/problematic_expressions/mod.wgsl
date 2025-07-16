#define_import_path mod

@group(0) @binding(0) var image: texture_2d<f32>;
@group(0) @binding(1) var s: sampler;

// miscellaneous expressions which have caused issues in the past, to avoid regressions

fn f() -> f32 {
    let x = textureSample(image, s, vec2<f32>(0.0)).r;
    let y = textureSample(image, s, vec2<f32>(0.0), vec2<i32>(1, 1)).r;
    return x * y;
}
