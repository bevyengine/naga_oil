#import test_module

fn entry_point() -> f32 {
    let fract_times_whole = test_module::fract_times_whole(3.1);
    return fract_times_whole - 0.3;
}
