class MyClass {
    constructor(arg) {
        console.log(`Initializing ${arg}`);
    }

    myMethod(n: number): number {
        if(n > 10) {
            return 10;
        }

        return n;
    }
}
