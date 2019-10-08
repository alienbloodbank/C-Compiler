#include <stdio.h>
#define read(x) scanf("%d", &x)
#define write(x) printf("%d\n", x)

int x;

int main() {
    int t1, t2;
    x = 0;
    int isqrt(int n) {
        int x, i;
        x = 1;
        i = 0;
        int square(int n) {
            return n*n/x;
        }
        n = square(n);
        while (i < n) {
            if ((i*i) > n) {return i-1;}
            i = i + 1;
        }
        return -1;
    }

    read(t1);
    t2 = isqrt(t1);
    write(t2);
}
