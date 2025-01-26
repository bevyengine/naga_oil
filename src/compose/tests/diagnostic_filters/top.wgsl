#import filters

@group(0) @binding(0) var s : sampler;
@group(0) @binding(2) var tex : texture_2d<f32>;
@group(1) @binding(0) var<storage, read> ro_buffer : array<f32, 4>;

@fragment
fn main(@builtin(position) p : vec4f) -> @location(0) vec4f {
    return filters::diagnostic_test();
}
