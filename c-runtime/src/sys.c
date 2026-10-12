#include <Kast/sys.h>

void Kast_chdir(StringView path) {
    char* path_c = StringView_to_C_String(path);
    int res = chdir(path_c);
    if (res == -1) {
        exit_errno("Kast_chdir");
    }
    Kast_free(path_c);
}

Int32 Kast_exec(StringView cmd) {
    char* cmd_c = StringView_to_C_String(cmd);
    int res = system(cmd_c);
    Kast_free(cmd_c);
#ifdef __POSIX__
    if (res == -1) {
        exit_errno("Kast_exec");
    }
    return WEXITSTATUS(res);
#elif defined(_WIN32)
    if (res == -1) {
        exit_errno("Kast_exec");
    }
    return res;
#else
    UNKOWN_SYSTEM
#endif
}

String Kast_getenv(StringView name) {
    char* name_c = StringView_to_C_String(name);
    char* buf = getenv(name_c);
    Kast_free(name_c);
    return String_from_C_StringView(buf);
}
