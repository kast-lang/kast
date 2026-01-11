import type * as readline from "node:readline/promises";
import type * as fs from "node:fs";
import type * as util from "node:util";

type Fn<A, R> = (arg: A) => Promise<R>;

interface Ref<T> {
  get: () => T;
}

interface RefMut<T> extends Ref<T> {
  set: (value: T) => void;
}

type Id = number;
interface Type {
  id: Id;
  name: string;
}
type Value = any;

type Option<T> =
  | {
      tag: "Some";
      data: T;
    }
  | { tag: "None" };

namespace Backend {
  export type TcpStream = {
    socket: import("node:net").Socket;
    error?: Error;
    buffer: string;
  };
  export type TcpListener = {
    server: import("node:net").Server;
    error?: Error;
  };
}

interface Backend<isNode> {
  dbg: {
    print: (value: Value) => Promise<void>;
  };
  io: {
    input: (prompt: string) => Promise<string>;
  };
  fs: isNode extends true
    ? {
        read_file: (path: string) => Promise<string>;
      }
    : undefined;
  sys: isNode extends true
    ? {
        chdir: (path: string) => Promise<void>;
        argc: () => Promise<number>;
        argv_at: (i: number) => Promise<string>;
        exec: (cmd: string) => Promise<number>;
        get_env: (name: string) => Promise<Option<string>>;
        exit: (code: number) => never;
      }
    : undefined;
  net: isNode extends true
    ? {
        tcp: {
          connect: (addr: string) => Promise<Backend.TcpStream>;
          stream: {
            read_line: (stream: RefMut<Backend.TcpStream>) => Promise<string>;
            write: (args: {
              0: RefMut<Backend.TcpStream>;
              1: Ref<string>;
            }) => Promise<void>;
            close: (stream: Backend.TcpStream) => Promise<void>;
          };
          bind: (addr: string) => Promise<Backend.TcpListener>;
          listener: {
            listen: (args: {
              0: RefMut<Backend.TcpListener>;
              1: number;
            }) => Promise<void>;
            accept: (args: {
              0: RefMut<Backend.TcpListener>;
              close_on_exec: boolean;
            }) => Promise<{ stream: Backend.TcpStream; addr: string }>;
            close: (listener: Backend.TcpListener) => Promise<void>;
          };
        };
      }
    : undefined;
  cleanup(): Promise<void>;
}

const Kast = await (async () => {
  const isNode: true | false =
    typeof process !== "undefined" &&
    process.versions != null &&
    process.versions.node != null;

  const backend: Backend<true> | Backend<false> = await (async () => {
    if (isNode) {
      const util = await import("node:util");
      const readline = await import("node:readline/promises");
      const fs = await import("node:fs");
      const net = await import("node:net");
      const child_process = await import("node:child_process");

      let readline_interface: readline.Interface | null = null;
      function ensure_readline_interface() {
        if (readline_interface === null) {
          readline_interface = readline.createInterface({
            input: process.stdin,
            output: process.stdout,
          });
        }
        return readline_interface;
      }
      function cleanup_readline_interface() {
        if (readline_interface !== null) {
          readline_interface.close();
        }
      }

      type TcpStream = Backend.TcpStream;
      type TcpListener = Backend.TcpListener;

      function setup_tcp_stream(socket: import("node:net").Socket): TcpStream {
        const stream: TcpStream = { socket, buffer: "" };
        socket.setEncoding("utf8");
        socket.addListener("data", (data) => {
          stream.buffer += data;
        });
        socket.addListener("error", (e) => {
          stream.error = e;
        });
        return stream;
      }

      function waitForSocketEvent<
        E extends keyof import("node:net").SocketEventMap,
      >(
        stream: TcpStream,
        event: E
      ): Promise<import("node:net").SocketEventMap[E]> {
        return new Promise((resolve, reject) => {
          if (stream.error !== undefined) {
            reject(stream.error);
          } else {
            const on_error = (e: Error) => {
              reject(e);
            };
            const on_close = (hadError: boolean) => {
              reject(new Error("connection was closed"));
            };
            stream.socket.once(event, (...result) => {
              stream.socket.removeListener("error", on_error);
              stream.socket.removeListener("close", on_close);
              resolve(result);
            });
            stream.socket.once("error", on_error);
            stream.socket.once("close", on_close);
          }
        });
      }

      function waitForServerEvent<
        E extends keyof import("node:net").ServerEventMap,
      >(
        listener: TcpListener,
        event: E
      ): Promise<import("node:net").ServerEventMap[E]> {
        return new Promise((resolve, reject) => {
          if (listener.error !== undefined) {
            reject(listener.error);
          } else {
            const on_error = (e: Error) => {
              reject(e);
            };
            listener.server.once(event, (...result) => {
              listener.server.removeListener("error", on_error);
              resolve(result);
            });
            listener.server.once("error", on_error);
          }
        });
      }

      return {
        dbg: {
          async print(value: Value) {
            console.log(util.inspect(value, { depth: null, colors: true }));
          },
        },
        io: {
          async input(prompt: string) {
            return await ensure_readline_interface().question(prompt);
          },
        },
        fs: {
          async read_file(path: string) {
            return await new Promise((resolve, reject) => {
              fs.readFile(path, "utf8", (err, data) => {
                if (err) reject(err);
                else resolve(data);
              });
            });
          },
        },
        net: {
          tcp: {
            async connect(addr: string): Promise<TcpStream> {
              const colon_idx = addr.lastIndexOf(":");
              if (colon_idx < 0) {
                throw Error("no : in tcp.connect");
              }
              const host = addr.substring(0, colon_idx);
              const port = parseInt(addr.substring(colon_idx + 1));
              const socket = net.connect({ host, port });
              const stream = setup_tcp_stream(socket);
              await waitForSocketEvent(stream, "connect");
              return stream;
            },
            stream: {
              async read_line(stream_ref: RefMut<TcpStream>): Promise<string> {
                const stream = stream_ref.get();
                while (true) {
                  let newline_idx = stream.buffer.indexOf("\n");
                  if (newline_idx < 0) {
                    await waitForSocketEvent(stream, "data");
                  } else {
                    const line = stream.buffer.substring(0, newline_idx);
                    stream.buffer = stream.buffer.substring(newline_idx + 1);
                    return line;
                  }
                }
              },
              async write({
                0: stream_ref,
                1: data_ref,
              }: {
                0: RefMut<TcpStream>;
                1: Ref<string>;
              }): Promise<void> {
                const stream = stream_ref.get();
                const data = data_ref.get();
                return new Promise((resolve, reject) => {
                  stream.socket.write(data, (err) => {
                    if (err) {
                      reject(err);
                    } else {
                      resolve();
                    }
                  });
                });
              },
              async close(stream: TcpStream): Promise<void> {
                stream.socket.destroy();
              },
            },
            async bind(addr: string): Promise<TcpListener> {
              const colon_idx = addr.lastIndexOf(":");
              if (colon_idx < 0) {
                throw Error("no : in tcp.bind");
              }
              const host = addr.substring(0, colon_idx);
              const port = parseInt(addr.substring(colon_idx + 1));

              const server = new net.Server();
              server.listen(port, host);
              const listener: TcpListener = { server };
              await waitForServerEvent(listener, "listening");
              return listener;
            },
            listener: {
              async listen(args: {
                0: RefMut<TcpListener>;
                1: number;
              }): Promise<void> {
                const listener = args[0].get();
                const max_pending = args[1];
                // Don't need to do anything?
              },
              async accept(args: {
                0: RefMut<TcpListener>;
                close_on_exec: boolean;
              }): Promise<{ stream: TcpStream; addr: string }> {
                const listener = args[0].get();
                args.close_on_exec;
                const [socket] = await waitForServerEvent(
                  listener,
                  "connection"
                );
                return {
                  stream: setup_tcp_stream(socket),
                  addr: JSON.stringify(socket.address()),
                };
              },
              async close(listener: TcpListener): Promise<void> {
                return new Promise((resolve, reject) => {
                  listener.server.close((err) => {
                    if (err) {
                      reject(err);
                    } else {
                      resolve();
                    }
                  });
                });
              },
            },
          },
        },
        sys: {
          async chdir(path: string): Promise<void> {
            process.chdir(path);
          },
          async argc(): Promise<number> {
            return process.argv.length - 1;
          },
          async argv_at(i: number): Promise<string> {
            return process.argv[i + 1];
          },
          async exec(cmd: string): Promise<number> {
            return new Promise((resolve, reject) => {
              const child = child_process.spawn("/bin/sh", ["-c", cmd], {
                stdio: "inherit",
              });
              child.on("exit", (code, signal) => {
                if (code !== null) {
                  resolve(code);
                } else {
                  reject(new Error(`Process exited with signal ${signal}`));
                }
              });
              child.on("error", reject);
            });
          },
          async get_env(name: string): Promise<Option<string>> {
            const value = process.env[name];
            if (value === undefined) {
              return { tag: "None" };
            } else {
              return { tag: "Some", data: value };
            }
          },
          exit(code: number): never {
            process.exit(code);
          },
        },
        async cleanup() {
          cleanup_readline_interface();
        },
      };
    } else {
      return {
        dbg: {
          async print(value: Value) {
            console.debug(value);
          },
        },
        io: {
          async input(p: string) {
            const result = prompt(p);
            if (result === null) {
              throw Error("No input was provided");
            }
            return result;
          },
        },
        fs: undefined,
        net: undefined,
        sys: undefined,
        async cleanup() {},
      };
    }
  })();

  async function call<A, R>(f: Fn<A, R>, arg: A): Promise<R> {
    return await f(arg);
  }

  let next_id = 0;
  const Id = {
    gen: () => {
      return next_id++;
    },
  };
  function newtype(name: string): Type {
    return { id: Id.gen(), name };
  }

  function check_todo(value: Value) {
    if (value.todo) {
      throw Error(value.todo);
    }
  }
  const cast_impls = {
    by_target: new Map(),
  };
  function add_cast_impl({
    value,
    target,
    impl,
  }: {
    value: Value;
    target: Value;
    impl: Value;
  }) {
    if (!cast_impls.by_target.get(target)) {
      cast_impls.by_target.set(target, new Map());
    }
    const by_target = cast_impls.by_target.get(target);
    by_target.set(value, impl);
  }
  function get_cast_impl({ value, target }: { value: Value; target: Value }) {
    check_todo(target);
    check_todo(value);
    const by_target = cast_impls.by_target.get(target);
    let result;
    if (!by_target) {
      result = undefined;
    } else {
      result = by_target.get(value);
    }
    if (result === undefined) {
      console.error("no cast impl found", value, "as", target);
      throw Error("no cast impl found");
    }
    return result;
  }
  const types = {
    todo(s: string) {
      return {
        todo: s,
      };
    },
    primitive: {
      Unit: newtype("Unit"),
      Bool: newtype("Bool"),
      Char: newtype("Char"),
      Int32: newtype("Int32"),
      Int64: newtype("Int64"),
      Float64: newtype("Float64"),
      String: newtype("String"),
    },
  };
  return {
    ...backend,
    Id,
    check_todo,
    cmp: {
      async less(args: { 0: Value; 1: Value }) {
        return args["0"] < args["1"];
      },
      async less_or_equal(args: { 0: Value; 1: Value }) {
        return args["0"] <= args["1"];
      },
      async equal(args: { 0: Value; 1: Value }) {
        return args["0"] === args["1"];
      },
      async not_equal(args: { 0: Value; 1: Value }) {
        return args["0"] !== args["1"];
      },
      async greater_or_equal(args: { 0: Value; 1: Value }) {
        return args["0"] >= args["1"];
      },
      async greater(args: { 0: Value; 1: Value }) {
        return args["0"] > args["1"];
      },
    },
    op: {
      add: async (args: { 0: Value; 1: Value }) => args["0"] + args["1"],
      sub: async (args: { 0: Value; 1: Value }) => args["0"] - args["1"],
      mul: async (args: { 0: Value; 1: Value }) => args["0"] * args["1"],
      rem: async ({ 0: lhs, 1: rhs }: { 0: Value; 1: Value }) => lhs % rhs,
      div_temp: async ({
        0: T,
        1: lhs,
        2: rhs,
      }: {
        0: Type;
        1: Value;
        2: Value;
      }) => {
        if (T === types.primitive.Float64) return lhs / rhs;
        return Math.floor(lhs / rhs);
      },
      neg: async (x: Value) => -x,
      bit_and: async ({ 0: lhs, 1: rhs }: { 0: Value; 1: Value }) => lhs & rhs,
      bit_or: async ({ 0: lhs, 1: rhs }: { 0: Value; 1: Value }) => lhs | rhs,
      bit_xor: async ({ 0: lhs, 1: rhs }: { 0: Value; 1: Value }) => lhs ^ rhs,
      bit_not: async (x: Value) => ~x,
      bit_shift_left: async ({ 0: lhs, 1: rhs }: { 0: Value; 1: Value }) =>
        lhs << rhs,
      bit_shift_right: async ({ 0: lhs, 1: rhs }: { 0: Value; 1: Value }) =>
        lhs >> rhs,
    },
    Char: {
      code: async (c: string) => c.charCodeAt(0),
    },
    String: {
      substring: async ({
        0: s,
        1: start,
        2: len,
      }: {
        0: string;
        1: number;
        2: number;
      }) => {
        return s.substring(start, start + len);
      },
      iter: async ({ 0: s, 1: f }: { 0: string; 1: Fn<string, void> }) => {
        for (let c of s) {
          await call(f, c);
        }
      },
      at: async ({ 0: s, 1: i }: { 0: string; 1: number }) => {
        return s.at(i);
      },
      length: async (s: string) => {
        return s.length;
      },
      to_string: async (x: Value) => {
        return x.toString();
      },
    },
    types,
    panic: async (s: string) => {
      throw Error(s);
    },
    casts: {
      add_impl: add_cast_impl,
      get_impl: get_cast_impl,
    },
  };
})();
