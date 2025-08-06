open Std
open Kast_core

module TyImpl = struct
  type t = unit
  type Ty.Shape.t += T of t

  let print fmt () = fprintf fmt "bool"
end

let init_ty () = Plugin.Ty.register (module TyImpl)

module ValueImpl = struct
  type t = bool
  type Value.Shape.t += T of t

  let print fmt value = fprintf fmt "%b" value
  let typeof _value = Ty.inferred (TyImpl.T ())
end

let init_value () =
  Plugin.Value.register (module ValueImpl);
  let register name value =
    Compiler.register_core_syntax
      {
        name;
        impl =
          (fun (type a)
            compiler
            (kind : a Compilable.t)
            (ast : Ast.t)
            _group
            :
            a
          ->
            let span = ast.span in
            match kind with
            | Expr ->
                Expr.Shape.Const { shape = ValueImpl.T value }
                |> Compiler.Init.expr span compiler
            | TyExpr ->
                Error.throw span "%s must be expr" name;
                Compilable.error span kind
            | Pattern ->
                Error.throw span "%s must be expr" name;
                Compilable.error span kind
            | Assignee ->
                Error.throw span "%s must be expr" name;
                Compilable.error span kind);
      }
  in
  register "true" true;
  register "false" false

module Ty = TyImpl
module Value = ValueImpl

let init () =
  init_ty ();
  init_value ()
