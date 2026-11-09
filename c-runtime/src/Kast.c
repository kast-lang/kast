#include <Kast.h>

void Kast_init(int argc, const char* argv[]) {
    Kast_init_type_infos();
    Kast_backtrace_init();
    CLI_ARGS.argc = argc;
    CLI_ARGS.original_argv = argv;
    CLI_ARGS.argv = Kast_allocate_array(&StringView_TypeInfo, argc);
    for (int i = 0; i < argc; i++) {
        CLI_ARGS.argv[i] = (StringView) {
            .buf = argv[i],
            .length = strlen(argv[i]),
        };
    }
}

void Kast_quit(void) {}

noreturn void Kast_match_non_exhaustive(void) {
    exit_with_error("Non exhausitve match");
}

noreturn void default_panic_handler(const StringView s) {
    fprintf(stderr, "Unhandled panic: ");
    Kast_write(stderr, s);
    fprintf(stderr, "\n");
    exit_with_error(NULL);
}
