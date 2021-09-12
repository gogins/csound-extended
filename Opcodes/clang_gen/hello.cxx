#include <stdio.h>

extern "C" {
    int my_hook(const char *what) {
        int result = 98;
        printf("what: %s will return: %d\n", what, result);
        return result;
    }
}

int main(int argc, const char **argv) {
    printf("Hello, world, from LLVM land!\n");
    return 0;
}