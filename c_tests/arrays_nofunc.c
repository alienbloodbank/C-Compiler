#include <stdio.h>

int main() {
    int arr[10];
    int i, temp, j;
    i = 0;
    while (i < 10) {
        scanf("%d", &arr[i]);
        i = i + 1;
    }
    i = 0;
    while (i < 9) {
        j = i+1;
        while (j < 10) {
            if (arr[i] > arr[j]) {
                temp = arr[j];
                arr[j] = arr[i];
                arr[i] = temp;
            }
            j = j + 1;
        }
        i = i + 1;
    }
    i = 0;
    while (i < 10) {
        printf("%d ", arr[i]);
        i = i + 1;
    }
    printf("\n");
}
