#include <stdio.h>

#define read(x) scanf("%d",&x)
#define write(x) printf("%d\n",x)

int N;

int Calculate(int cnt) {
  res = 0;
  while (cnt > 0)
  {
    res = res + 42;
    cnt = cnt - 1;
  }
  return res;
}

int main (void) {
  printf("Magic positive number is ");
  read(N);
  printf("The meaning of Life is ");
  write(Calculate(N) / N);
}


