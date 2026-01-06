// @ts-nocheck
const Kast = (() => {
  const readline = require("node:readline/promises");
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
  return {
    Id,
    check_todo,
    cmp: {
      less: async (args) => args["0"] < args["1"],
      less_or_equal: async (args) => args["0"] <= args["1"],
      equal: async (args) => args["0"] == args["1"],
      not_equal: async (args) => args["0"] != args["1"],
      greater_or_equal: async (args) => args["0"] >= args["1"],
      greater: async (args) => args["0"] > args["1"],
    },
    dbg: {
      print: async (args) => {
        const value = args;
        console.debug(value);
      },
    },
    op: {
      add: async (args) => args["0"] + args["1"],
      sub: async (args) => args["0"] - args["1"],
      mul: async (args) => args["0"] * args["1"],
    },
    io: {
      input: async (args) => {
        const prompt = args;
        return await ensure_readline_interface().question(prompt);
      },
    },
    types: {
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
