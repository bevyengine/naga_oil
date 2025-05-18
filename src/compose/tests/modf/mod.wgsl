#define_import_path test_module

fn fract_times_whole(x: f32) -> f32 {
    let fw = modf(x);
    let f = fw.fract;
    let w = fw.whole;
    return f * w;
}

fn entry_point() -> f32 {
    let fract_times_whole = fract_times_whole(3.25);
    return fract_times_whole - 0.75;
}
