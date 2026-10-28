#ifndef __KAST_SYS_H__

#include "./__common__.h"
#include "./string.h"

void Kast_chdir(StringView path);
Int32 Kast_exec(StringView cmd);

typedef struct {
    Int32 exit_code;
    /* not stdout/stderr because those are macros??? */
    String sout;
    String serr;
} Kast_ExecResult;

void Kast_ExecResult_drop(Kast_ExecResult self);

Kast_ExecResult Kast_exec_and_capture_output(StringView cmd);

bool Kast_getenv(StringView name, String* result);

#endif
