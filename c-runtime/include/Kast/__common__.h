#ifndef __KAST_COMMON_H__
#define __KAST_COMMON_H__

/* idk where this is documented,
 * but we want old winsock.h instead of winsock2.h */
#ifdef _WIN32
#define _WINSOCKAPI_
#include <windows.h>
#undef near
#undef far
#endif

#if defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
#define __POSIX__
#endif

// #define _GNU_SOURCE
// #define _POSIX_C_SOURCE 200112L
#ifdef __EMSCRIPTEN__
#include <emscripten/html5.h>
#define thread_local
#else
// #ifndef __TINYC__
#define USE_LIBBACKTRACE
// #endif
#include <pthread.h>
#define thread_local __thread
// TODO tcc doesnt have thread locals so noone has it now
// #define thread_local
// NOTE: never use <threads.h> its borked (with beohmgc specifically)
#define GC_THREADS
#endif
#ifdef USE_LIBBACKTRACE
#include <backtrace.h>
#else
#ifndef __EMSCRIPTEN__
#include <execinfo.h>
#endif
#endif

#ifdef _WIN32
// TODO windows networking
#else
#include <netdb.h>
#include <sys/socket.h>
#endif

#include <assert.h>
#include <errno.h>
#include <stdalign.h>
#include <stdarg.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

typedef struct {
} Unit;

static const Unit UNIT;

typedef bool Bool;
typedef char Byte;
typedef int32_t Int32;
typedef uint32_t UInt32;
typedef int64_t Int64;
typedef uint64_t UInt64;
typedef float Float32;
typedef double Float64;
typedef uint32_t Char;

typedef struct TypeInfo TypeInfo;
typedef struct Context Context;

noreturn void exit_with_error(const char* fmt, ...);
noreturn void exit_errno(const char* fmt, ...);

#endif
