#include <stdio.h>

#ifdef DEBUG
extern void printDebugInfo(const char* txt);
#define MY_CONST 128
#endif

int main() {
  int i;
  for(i = 0; i < 10; i++) {}

  i = 0;
  while (i < 10) {
    i++;
  }
  
  int myval = 100;
  if (100 == myval) {
    printf("My value: %d\n", myval);
  }
}
