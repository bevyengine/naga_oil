#define_import_path filters

diagnostic(warning, derivative_uniformity);

fn diagnostic_test(s : sampler, tex : texture_2d<f32>, ro_buffer : array<f32, 4>) -> vec4f {
    if ro_buffer[0] == 0 {
        // Emits a derivative uniformity error during validation.
        return textureSample(tex, s, vec2(0.,0.));
    }

      return vec4f(0.);
}
