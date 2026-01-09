import type * as readline from "node:readline/promises";
import type * as fs from "node:fs";
import type * as util from "node:util";

const Kast = await (async () => {
  // Node.js
  const isNode =
    typeof process !== "undefined" &&
    process.versions != null &&
    process.versions.node != null;

  // Browser
  const isBrowser =
    typeof window !== "undefined" && typeof window.document !== "undefined";

  interface Backend {
    dbg: {
      print: (value: Value) => Promise<void>;
    };
    io: {
      input: (prompt: string) => Promise<string>;
    };
    fs: {
      read_file: (path: string) => Promise<string>;
    };
    cleanup(): Promise<void>;
  }

  const backend: Backend = await (async () => {
    if (isBrowser) {
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
        fs: {
          async read_file(path) {
            throw Error("No fs in browser");
          },
        },
        async cleanup() {},
      };
    } else if (isNode) {
      const util = await import("node:util");
      const readline = await import("node:readline/promises");
      const fs = await import("node:fs");

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
        async cleanup() {
          cleanup_readline_interface();
        },
      };
    } else {
      throw Error("Not browser and not nodejs???");
    }
  })();

  type Fn<A, R> = (arg: A) => Promise<R>;
  async function call<A, R>(f: Fn<A, R>, arg: A): Promise<R> {
    return await f(arg);
  }

  type Id = number;
  let next_id = 0;
  const Id = {
    gen: () => {
      return next_id++;
    },
  };
  interface Type {
    id: Id;
    name: string;
  }
  function newtype(name: string): Type {
    return { id: Id.gen(), name };
  }

  type Value = any;

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
    sys: {
      chdir: async (path: string) => {
        process.chdir(path);
      },
      argc: async () => {
        return process.argv.length - 1;
      },
      argv_at: async (i: number) => {
        return process.argv[i + 1];
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
