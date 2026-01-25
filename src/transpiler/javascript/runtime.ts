import type * as readline from "node:readline/promises";
import type * as fs from "node:fs";
import type * as util from "node:util";

type Context = unknown;
type Fn<A extends any[], R> = (ctx: Context, ...args: A) => Promise<R>;

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
  todo?: string;
}
type Value = any;

type Option<T> =
  | {
      tag: "Some";
      data: T;
    }
  | { tag: "None" };

export type TcpStream = {
  socket: import("node:net").Socket;
  error?: Error;
  buffer: string;
};
export type TcpListener = {
  server: import("node:net").Server;
  error?: Error;
};

interface Backend<isNode> {
  dbg: {
    print: Fn<Value, void>;
  };
  io: {
    input: Fn<[string], string>;
  };
  fs: isNode extends true
    ? {
        read_file: Fn<[string], string>;
      }
    : undefined;
  sys: isNode extends true
    ? {
        chdir: Fn<[string], void>;
        argc: Fn<[], number>;
        argv_at: Fn<[number], string>;
        exec: Fn<[string], number>;
        get_env: Fn<[string], Option<string>>;
        exit: Fn<[number], never>;
      }
    : undefined;
  net: isNode extends true
    ? {
        tcp: {
          connect: Fn<[string], TcpStream>;
          stream: {
            read_line: Fn<[RefMut<TcpStream>], string>;
            write: Fn<[RefMut<TcpStream>, Ref<string>], void>;
            close: Fn<[TcpStream], void>;
          };
          bind: Fn<[string], TcpListener>;
          listener: {
            listen: Fn<[RefMut<TcpListener>, number], void>;
            accept: Fn<
              [
                RefMut<TcpListener>,
                {
                  close_on_exec: boolean;
                },
              ],
              { stream: TcpStream; addr: string }
            >;
            close: Fn<[TcpListener], void>;
          };
        };
      }
    : undefined;
  cleanup(): Promise<void>;
}

type Char = string;

interface Kast<isNode> extends Backend<isNode> {
  io: Backend<isNode>["io"] & {
    print: Fn<[string], void>;
    eprint: Fn<[string], void>;
  };
  cmp: {
    less: Fn<[Value, Value], boolean>;
    less_or_equal: Fn<[Value, Value], boolean>;
    equal: Fn<[Value, Value], boolean>;
    not_equal: Fn<[Value, Value], boolean>;
    greater_or_equal: Fn<[Value, Value], boolean>;
    greater: Fn<[Value, Value], boolean>;
  };
  op: {
    add: Fn<[Value, Value], Value>;
    sub: Fn<[Value, Value], Value>;
    mul: Fn<[Value, Value], Value>;
    rem: Fn<[Value, Value], Value>;
    div_temp: Fn<[Type, Value, Value], Value>;
    neg: Fn<[Value], Value>;
    bit_and: Fn<[Value, Value], Value>;
    bit_or: Fn<[Value, Value], Value>;
    bit_xor: Fn<[Value, Value], Value>;
    bit_not: Fn<[Value], Value>;
    bit_shift_left: Fn<[Value, Value], Value>;
    bit_shift_right: Fn<[Value, Value], Value>;
  };
  Char: {
    code: Fn<[Char], number>;
    from_code: Fn<[number], Char>;
  };
  String: {
    substring: Fn<[string, number, number], string>;
    iter: Fn<[string, Fn<[Char], void>], void>;
    at: Fn<[string, number], Char>;
    length: Fn<[string], number>;
    to_string: Fn<[Value], string>;
  };
  parse: {
    Int32: Fn<[string], number>;
    Int64: Fn<[string], bigint>;
    Float64: Fn<[string], number>;
  };
  types: {
    todo: (s: string) => Type;
    create_or_find: (s: string) => Type;
    primitive: {
      Unit: Type;
      Bool: Type;
      Char: Type;
      Int32: Type;
      Int64: Type;
      Float64: Type;
      String: Type;
    };
  };
  panic: Fn<[string], void>;
  casts: {
    add_impl: (args: { value: Value; target: Value; impl: Value }) => void;
    get_impl: (args: { value: Value; target: Value }) => Value;
  };
}

const Kast = await (async (): Promise<Kast<true> | Kast<false>> => {
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
        event: E,
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
        event: E,
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
          async print(ctx, value: Value) {
            console.log(util.inspect(value, { depth: null, colors: true }));
          },
        },
        io: {
          async input(ctx, prompt: string) {
            return await ensure_readline_interface().question(prompt);
          },
        },
        fs: {
          async read_file(ctx, path: string) {
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
            async connect(ctx, addr: string): Promise<TcpStream> {
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
              async read_line(
                ctx: Context,
                stream_ref: RefMut<TcpStream>,
              ): Promise<string> {
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
              async write(
                ctx: Context,
                stream_ref: RefMut<TcpStream>,
                data_ref: Ref<string>,
              ): Promise<void> {
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
              async close(ctx: Context, stream: TcpStream): Promise<void> {
                stream.socket.destroy();
              },
            },
            async bind(ctx: Context, addr: string): Promise<TcpListener> {
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
              async listen(
                ctx: Context,
                listener: RefMut<TcpListener>,
                max_pending: number,
              ): Promise<void> {
                // Don't need to do anything?
              },
              async accept(
                ctx: Context,
                listener_ref: RefMut<TcpListener>,
                { close_on_exec },
              ): Promise<{ stream: TcpStream; addr: string }> {
                const listener = listener_ref.get();
                const [socket] = await waitForServerEvent(
                  listener,
                  "connection",
                );
                return {
                  stream: setup_tcp_stream(socket),
                  addr: JSON.stringify(socket.address()),
                };
              },
              async close(ctx: Context, listener: TcpListener): Promise<void> {
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
          async chdir(ctx: Context, path: string): Promise<void> {
            process.chdir(path);
          },
          async argc(ctx: Context): Promise<number> {
            return process.argv.length - 1;
          },
          async argv_at(ctx: Context, i: number): Promise<string> {
            return process.argv[i + 1];
          },
          async exec(ctx: Context, cmd: string): Promise<number> {
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
          async get_env(ctx: Context, name: string): Promise<Option<string>> {
            const value = process.env[name];
            if (value === undefined) {
              return { tag: "None" };
            } else {
              return { tag: "Some", data: value };
            }
          },
          exit(ctx: Context, code: number): never {
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
          async print(ctx: Context, value: Value) {
            console.debug(value);
          },
        },
        io: {
          async input(ctx: Context, p: string) {
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

  async function call<A extends any[], R>(
    f: Fn<A, R>,
    ctx: Context,
    ...args: A
  ): Promise<R> {
    return await f(ctx, ...args);
  }

  let next_id = 0;
  const Id = {
    gen: () => {
      return next_id++;
    },
  };

  const named_types: { [name: string]: Type } = {};
  function newtype(name: string): Type {
    const type = { id: Id.gen(), name };
    named_types[name] = type;
    return type;
  }

  function check_todo(value: Value) {
    if (value.todo) {
      throw Error(value.todo);
    }
  }
  const cast_impls: {
    by_target: Map<any, Map<any, any>>;
  } = {
    by_target: new Map(),
  };

  function single_arg(arg: Value): Value {
    if ("0" in arg) {
      return arg["0"];
    } else {
      throw new Error("expected single arg");
    }
  }

  function add_cast_impl({
    value,
    target,
    impl,
  }: {
    value: Value;
    target: Value;
    impl: Value;
  }) {
    value = single_arg(value);
    if (!cast_impls.by_target.get(target)) {
      cast_impls.by_target.set(target, new Map());
    }
    const by_target = cast_impls.by_target.get(target)!;
    by_target.set(value, impl);
  }
  function get_cast_impl({ value, target }: { value: Value; target: Value }) {
    value = single_arg(value);
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
        id: Id.gen(),
        name: "<todo>",
        todo: s,
      };
    },
    create_or_find(name: string) {
      const existing = named_types[name];
      if (existing === undefined) {
        return newtype(name);
      } else {
        return existing;
      }
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
    io: {
      ...backend.io,
      async print(ctx: Context, s: string) {
        console.log(s);
      },
      async eprint(ctx: Context, s: string) {
        console.error(s);
      },
    },
    cmp: {
      async less(ctx, lhs, rhs) {
        return lhs < rhs;
      },
      async less_or_equal(ctx, lhs, rhs) {
        return lhs <= rhs;
      },
      async equal(ctx, lhs, rhs) {
        return lhs === rhs;
      },
      async not_equal(ctx, lhs, rhs) {
        return lhs !== rhs;
      },
      async greater_or_equal(ctx, lhs, rhs) {
        return lhs >= rhs;
      },
      async greater(ctx, lhs, rhs) {
        return lhs > rhs;
      },
    },
    op: {
      add: async (ctx, lhs, rhs) => lhs + rhs,
      sub: async (ctx, lhs, rhs) => lhs - rhs,
      mul: async (ctx, lhs, rhs) => lhs * rhs,
      rem: async (ctx, lhs, rhs) => lhs % rhs,
      div_temp: async (ctx, T, lhs, rhs) => {
        if (T === types.primitive.Float64) return lhs / rhs;
        if (T === types.primitive.Int64) return lhs / rhs;
        return Math.floor(lhs / rhs);
      },
      neg: async (ctx, x: Value) => -x,
      bit_and: async (ctx, lhs, rhs) => lhs & rhs,
      bit_or: async (ctx, lhs, rhs) => lhs | rhs,
      bit_xor: async (ctx, lhs, rhs) => lhs ^ rhs,
      bit_not: async (ctx, x: Value) => ~x,
      bit_shift_left: async (ctx, lhs, rhs) => lhs << rhs,
      bit_shift_right: async (ctx, lhs, rhs) => lhs >> rhs,
    },
    Char: {
      async code(ctx, c: string): Promise<number> {
        return c.charCodeAt(0);
      },
      async from_code(ctx, code: number): Promise<string> {
        return String.fromCharCode(code);
      },
    },
    String: {
      substring: async (ctx, s, start, len) => {
        return s.substring(start, start + len);
      },
      iter: async (ctx, s, f) => {
        for (let c of s) {
          await call(f, ctx, c);
        }
      },
      at: async (ctx, s, i) => {
        const c = s.at(i);
        if (c === undefined) {
          throw new Error("out of bounds");
        }
        return c;
      },
      length: async (ctx, s: string) => {
        return s.length;
      },
      to_string: async (x: Value) => {
        return x.toString();
      },
    },
    parse: {
      async Int32(ctx, s) {
        return parseInt(s);
      },
      async Int64(ctx, s) {
        return BigInt(s);
      },
      async Float64(ctx, s) {
        return parseFloat(s);
      },
    },
    types,
    panic: async (ctx, s: string) => {
      throw Error(s);
    },
    casts: {
      add_impl: add_cast_impl,
      get_impl: get_cast_impl,
    },
  };
})();
