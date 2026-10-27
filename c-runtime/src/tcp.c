#include <Kast/io.h>
#include <Kast/tcp.h>

tcp_Stream tcp_Stream_from_fd(int fd) {
    FILE* reader = fdopen(fd, "r");
    if (!reader) {
        exit_errno("tcp_Stream_from_fd");
    }
    FILE* writer = fdopen(fd, "w");
    if (!writer) {
        exit_errno("tcp_Stream_from_fd");
    }
    return (tcp_Stream) {
        .sock_fd = fd,
        .reader = reader,
        .writer = writer,
    };
}

tcp_Stream tcp_Stream_connect(StringView addr) {
#ifdef _WIN32
    exit_with_error("TODO tcp_Stream_connect windows");
#else
    char* colon_pos = memchr(addr.buf, ':', addr.length);
    if (!colon_pos) {
        exit_with_error("Expected host:port");
    }
    StringView host = {
        .buf = addr.buf,
        .length = colon_pos - addr.buf,
    };
    char* host_c = StringView_to_C_String(host);
    StringView port_s = {
        .buf = colon_pos + 1,
        .length = addr.buf + addr.length - colon_pos - 1,
    };
    // Int32 port = Int32_from_String(port_s);
    char* port_c = StringView_to_C_String(port_s);
    struct addrinfo *ai, *rp;
    int res = getaddrinfo(host_c, port_c, NULL, &ai);
    if (res) {
        if (res == EAI_SYSTEM) {
            exit_errno("tcp_Stream_connect.getaddrinfo");
        } else {
            fprintf(stderr, "getaddrinfo failed with %d", res);
            exit(-1);
        }
    }
    Kast_free(host_c);
    Kast_free(port_c);
    for (rp = ai; rp != NULL; rp = rp->ai_next) {
        if (rp->ai_socktype != SOCK_STREAM) {
            continue;
        }
        int sock_fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (sock_fd == -1) {
            exit_errno("tcp_Stream_connect.socket");
        }
        int res = connect(sock_fd, rp->ai_addr, rp->ai_addrlen);
        if (res == 0) {
            freeaddrinfo(ai);
            return tcp_Stream_from_fd(sock_fd);
        };
        // ignore errno, try next addr
    }
    freeaddrinfo(ai);
    exit_with_error("Failed to connect");
#endif
}

void tcp_Stream_close(tcp_Stream s) {
    int res = fclose(s.reader);
    if (res != 0) {
        exit_errno("tcp_Stream_close.reader");
    }
    // Dont need to close writer since reader closes underlying fd
    // res = fclose(s.writer);
    // if (res != 0) {
    //     exit_errno("tcp_Stream_close.writer");
    // }
}

String tcp_Stream_read_line(tcp_Stream* s) {
    return Kast_read_until(s->reader, '\n');
}

void tcp_Stream_write(tcp_Stream* s, StringView data) {
    Kast_write(s->writer, data);
    if (fflush(s->writer) != 0) {
        exit_errno("tcp_Stream_write.fflush");
    }
}

tcp_Listener tcp_Listener_bind(StringView addr) {
#ifdef _WIN32
    exit_with_error("TODO tcp_Listener_bind windows");
#else
    char* colon_pos = memchr(addr.buf, ':', addr.length);
    if (!colon_pos) {
        exit_with_error("Expected host:port");
    }
    StringView host = {
        .buf = addr.buf,
        .length = colon_pos - addr.buf,
    };
    char* host_c = StringView_to_C_String(host);
    StringView port_s = {
        .buf = colon_pos + 1,
        .length = addr.buf + addr.length - colon_pos - 1,
    };
    // Int32 port = Int32_from_String(port_s);
    char* port_c = StringView_to_C_String(port_s);
    struct addrinfo *ai, *rp;
    int res = getaddrinfo(host_c, port_c, NULL, &ai);
    if (res) {
        if (res == EAI_SYSTEM) {
            exit_errno("tcp_Listener_bind.getaddrinfo");
        } else {
            fprintf(stderr, "getaddrinfo failed with %d", res);
            exit(-1);
        }
    }
    Kast_free(host_c);
    Kast_free(port_c);
    for (rp = ai; rp != NULL; rp = rp->ai_next) {
        int fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (fd == -1) {
            exit_errno("tcp_Listener_bind.socket");
        }
        int so_reuseaddr = true;
        setsockopt(
            fd,
            SOL_SOCKET,
            SO_REUSEADDR,
            &so_reuseaddr,
            sizeof(so_reuseaddr)
        );
        int res = bind(fd, rp->ai_addr, rp->ai_addrlen);
        if (res == 0) {
            freeaddrinfo(ai);
            return (tcp_Listener) {
                .fd = fd,
            };
        };
        // ignore errno, try next addr
    }
    freeaddrinfo(ai);
    exit_with_error("Failed to bind");
#endif
}

void tcp_Listener_listen(tcp_Listener* l, int max_pending) {
#ifdef _WIN32
    exit_with_error("TODO tcp_Listener_listen windows");
#else
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wanalyzer-fd-leak"
    int res = listen(l->fd, max_pending);
    if (res == -1) {
        exit_errno("tcp_Listener_listen");
    }
#pragma GCC diagnostic pop
#endif
}

tcp_Listener_accepted tcp_Listener_accept(tcp_Listener* l, bool close_on_exec) {
#ifdef _WIN32
    exit_with_error("TODO tcp_Listener_accept windows");
#else
    int flags = 0;
    if (close_on_exec) {
        flags |= SOCK_CLOEXEC;
    }
    struct sockaddr addr;
    socklen_t addr_len = sizeof(addr);
    // int fd = accept4(l->fd, &addr, &addr_len, flags);
    int fd = accept(l->fd, &addr, &addr_len);
    if (fd == -1) {
        exit_errno("tcp_Listener_accept.accept4");
    }
    size_t host_len = 100;
    char host[host_len];
    size_t port_len = 100;
    char port[port_len];
    int res = getnameinfo(&addr, addr_len, host, host_len, port, port_len, 0);
    if (res) {
        if (res == EAI_SYSTEM) {
            exit_errno("tcp_Listener_accept.getnameinfo");
        } else {
            fprintf(stderr, "getnameinfo errored with %d\n", res);
            exit(-1);
        }
    }
    host_len = strlen(host);
    port_len = strlen(port);
    char* addr_c = Kast_allocate_raw(host_len + 1 + port_len);
    memcpy(addr_c, host, host_len);
    addr_c[host_len] = ':';
    memcpy(addr_c + host_len + 1, port, port_len);
    String addr_s = String_from_raw_parts(addr_c, host_len + 1 + port_len);
    return (tcp_Listener_accepted) {
        .stream = tcp_Stream_from_fd(fd),
        .addr = addr_s,
    };
#endif
}

void tcp_Listener_close(tcp_Listener l) {
    int res = close(l.fd);
    if (res == -1) {
        exit_errno("tcp_Listener_close");
    }
}
