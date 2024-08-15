#include <stdio.h>

extern long gcd(long, long);

int main(int argc, char *argv[]) {
  printf("gcd(25, 20) = %ld\n", gcd(25, 20));
  printf("gcd(3, 5) = %ld\n", gcd(3, 5));
  printf("gcd(21, 28) = %ld\n", gcd(21, 28));
  return 0;
}
