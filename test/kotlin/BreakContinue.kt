fun myfunction() {
    val myList = listOf(1,2,3)

    // Some first pass processing
    for(i in myList) {
        if (i == 2) {
            break;
        }
    }

    // Some second pass processing
    for(i in myList) {
        if (i == 1) {
            continue;
        }
    }
}
