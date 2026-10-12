#ifndef __KAST_TCP_H__
#define __KAST_TCP_H__

#include "./__common__.h"
#include "./string.h"

typedef struct {
    int sock_fd;
    FILE* reader;
    FILE* writer;
} tcp_Stream;

typedef struct {
    int fd;
} tcp_Listener;

tcp_Stream tcp_Stream_from_fd(int fd);

tcp_Stream tcp_Stream_connect(StringView addr);

void tcp_Stream_close(tcp_Stream s);

String tcp_Stream_read_line(tcp_Stream* s);

void tcp_Stream_write(tcp_Stream* s, StringView data);

tcp_Listener tcp_Listener_bind(StringView addr);

void tcp_Listener_listen(tcp_Listener* l, int max_pending);

typedef struct {
    tcp_Stream stream;
    String addr;
} tcp_Listener_accepted;

tcp_Listener_accepted tcp_Listener_accept(tcp_Listener* l, bool close_on_exec);

void tcp_Listener_close(tcp_Listener l);

#endif
