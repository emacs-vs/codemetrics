function factorial(num) {
    if (num <= 1) {
        return 1;
    }

    return num * factorial(num - 1);
}
