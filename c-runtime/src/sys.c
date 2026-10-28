#include <Kast/io.h>
#include <Kast/sys.h>
#include <stdio.h>

void Kast_chdir(StringView path) {
    char* path_c = StringView_to_C_String(path);
    int res = chdir(path_c);
    if (res == -1) {
        exit_errno("Kast_chdir");
    }
    Kast_free(path_c);
}

Int32 get_exit_code(int exit_status) {
#ifdef __POSIX__
    if (exit_status == -1) {
        exit_errno("exit status -1");
    }
    return WEXITSTATUS(exit_status);
#elif defined(_WIN32)
    if (exit_status == -1) {
        exit_errno("exit status -1");
    }
    return exit_status;
#else
    UNKOWN_SYSTEM
#endif
}

Int32 Kast_exec(StringView cmd) {
    char* cmd_c = StringView_to_C_String(cmd);
    int res = system(cmd_c);
    Kast_free(cmd_c);
    return get_exit_code(res);
}

void Kast_ExecResult_drop(Kast_ExecResult self) {
    String_drop(self.sout);
    String_drop(self.serr);
}

Kast_ExecResult Kast_exec_and_capture_output(StringView cmd) {
    char* cmd_c = StringView_to_C_String(cmd);
    FILE* p = popen(cmd_c, "r");
    if (p == NULL) {
        exit_errno("Failed to run %s", cmd_c);
    }
    Kast_free(cmd_c);
    String output = Kast_read_to_end(p);
    Int32 exit_code = get_exit_code(pclose(p));
    return (Kast_ExecResult) {
        .exit_code = exit_code,
        .sout = output,
        /* TODO implement stderr */
        .serr = String_from_C_StringView(""),
    };
}

bool Kast_getenv(StringView name, String* result) {
    char* name_c = StringView_to_C_String(name);
    char* buf = getenv(name_c);
    Kast_free(name_c);
    if (buf == NULL) {
        return false;
    } else {
        *result = String_from_C_StringView(buf);
        return true;
    }
}
