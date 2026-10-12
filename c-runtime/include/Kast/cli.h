#ifndef __KAST_CLI_H__
#define __KAST_CLI_H__

#include "./__common__.h"
#include "./string.h"

typedef struct {
    int argc;
    const char** original_argv;
    StringView* argv;
} CliArgs;

extern CliArgs CLI_ARGS;

#endif
