fn factorial(n: u32) -> u32 {
    if n <= 1 {
        return 1;
    }

    n * factorial(n - 1)
}
