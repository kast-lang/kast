#include <stdbool.h>
#include <stdio.h>

bool is_prime(int x) {
    for (int i = 2; i < x; i++) {
        if (x % i == 0) {
            return false;
        }
    }
    return true;
}

int main() {
    int sum = 0;
    for (int x = 2; x < 10000; x++) {
        if (is_prime(x)) {
            sum += x;
        }
    }
    printf("%d\n", sum);
}
