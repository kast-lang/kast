// @ts-nocheck
const Kast = (() => {
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
    op: {
      add: (args) => args["0"] + args["1"],
      sub: (args) => args["0"] - args["1"],
      mul: (args) => args["0"] * args["1"],
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
  };
})();
