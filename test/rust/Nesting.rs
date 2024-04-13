fn main() {
    let nums = vec![1, 2, 3, 4, 128, 1024];
    nums.iter()
        .map(|elem| elem * 3)
        .for_each(|elem| match elem {
            3 | 9 => {}
            _ => {}
        });

    loop {
        if 1 == 3 {}
    }
}
