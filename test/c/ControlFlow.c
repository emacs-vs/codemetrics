void printInfo(int n) {
  switch(n) {
  case 1:
    break;
  case 2:
    break;
  default:
    printf("Not 1 or 2\n");
  }
}

void printNumbers(int n) {
  int i = n;
  do {
    printf("Num: %d\n", i);
    i--;
  } while (i > 0)
}

void printNumbers2(int n) {
  int i = n;
 myloop:
  printf("Num: %d\n", i);
  i--;
  if (i > 0) {
    goto myloop;
  }
}
