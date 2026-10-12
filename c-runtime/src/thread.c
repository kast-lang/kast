#include <Kast/thread.h>

void Kast_sleep_ns(int64_t ns) {
    time_t s = ns / 1000000000;
    ns %= 1000000000;
    struct timespec remaining = {.tv_sec = s, .tv_nsec = ns};
    while (true) {
        int res = nanosleep(&remaining, &remaining);
        if (res == 0) {
            break;
        }
        if (errno != EINTR) {
            exit_errno("Failed to sleep");
        }
    }
}
