// @ts-nocheck
const Kast = (() => {
  async function call(f, arg) {
    return await f(arg);
  }
  const util = require("node:util");
  const readline = require("node:readline/promises");
  const fs = require("node:fs");
  let readline_interface = null;
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
  let next_id = 0;
  const Id = {
    gen: () => {
      return next_id++;
    },
  };
  function newtype(name) {
    return { id: Id.gen(), name };
  }
  function check_todo(value) {
    if (value.todo) {
      throw Error(value.todo);
    }
  }
  const cast_impls = {
    by_target: new Map(),
  };
  function add_cast_impl({ value, target, impl }) {
    if (!cast_impls.by_target.get(target)) {
      cast_impls.by_target.set(target, new Map());
    }
    const by_target = cast_impls.by_target.get(target);
    by_target.set(value, impl);
  }
  function get_cast_impl({ value, target }) {
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
    todo: (s) => {
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
    Id,
    check_todo,
    cmp: {
      less: async (args) => args["0"] < args["1"],
      less_or_equal: async (args) => args["0"] <= args["1"],
      equal: async (args) => args["0"] === args["1"],
      not_equal: async (args) => args["0"] !== args["1"],
      greater_or_equal: async (args) => args["0"] >= args["1"],
      greater: async (args) => args["0"] > args["1"],
    },
    dbg: {
      print: async (value) => {
        console.log(util.inspect(value, { depth: null, colors: true }));
      },
    },
    op: {
      add: async (args) => args["0"] + args["1"],
      sub: async (args) => args["0"] - args["1"],
      mul: async (args) => args["0"] * args["1"],
      div_temp: async ({ 0: T, 1: lhs, 2: rhs }) => {
        if (T === types.primitive.Float64) return lhs / rhs;
        return Math.floor(lhs / rhs);
      },
      neg: async (x) => -x,
    },
    io: {
      input: async (args) => {
        const prompt = args;
        return await ensure_readline_interface().question(prompt);
      },
    },
    Char: {
      code: async (c) => c.charCodeAt(0),
    },
    String: {
      substring: async ({ 0: s, 1: start, 2: len }) => {
        return s.substring(start, start + len);
      },
      iter: async ({ 0: s, 1: f }) => {
        for (let c of s) {
          await call(f, c);
        }
      },
      at: async ({ 0: s, 1: i }) => {
        return s.at(i);
      },
      length: async (s) => {
        return s.length;
      },
      to_string: async (x) => {
        return x.toString();
      },
    },
    sys: {
      chdir: async (path) => {
        process.chdir(path);
      },
      argc: async () => {
        return process.argv.length - 1;
      },
      argv_at: async (i) => {
        return process.argv[i + 1];
      },
    },
    fs: {
      read_file: async (path) => {
        return await new Promise((resolve, reject) => {
          fs.readFile(path, "utf8", (err, data) => {
            if (err) reject(err);
            else resolve(data);
          });
        });
      },
    },
    types,
    panic: async (s) => {
      throw Error(s);
    },
    casts: {
      add_impl: add_cast_impl,
      get_impl: get_cast_impl,
    },
    cleanup: () => {
      cleanup_readline_interface();
    },
  };
})();
