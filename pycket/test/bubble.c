#include "stdlib.h"

int main(int argc, char* argv[]) {
    const long numberOfElements = atoi(argv[1]);
    long* array;
    int swapped = 1;
    array = malloc(sizeof(long) * numberOfElements);
    for (int i = 0; i < numberOfElements; i++) {
        array[i] = numberOfElements - i;
    }
    while (swapped) {
        swapped = 0;
        int i = 0;
        while (i < numberOfElements - 1) {
            long a = array[i];
            long b = array[i + 1];
            if (a > b) {
                array[i] = b;
                array[i + 1] = a;
                swapped = 1;
            }
            i += 1;
        }
    }
    return array[4];
}
