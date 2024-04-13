let myArr = [1, 3, 5, 10, 23];
let doubledArr = myArr.map(elem => elem * 2);

// Some bogus code to check combination of anonymous functions and nesting
doubledArr.filter(function(elem) {
    if(elem % 3 == 0) {
        return elem < 2;
    }
    return elem >= 10;
}).forEach(elem => {
    if(elem % 10 == 0) {
        console.log("Condition met");
    }
});
