#include <stdio.h>
#include <stdlib.h>

int main(void) {
  unsigned int x;
  unsigned int y;

  while (scanf("%u %u", &x, &y) != EOF) {
    printf("%u\n", x ^ y);
  }

  return 0;
}
