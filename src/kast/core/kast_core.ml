open Std
open Kast_util
module Ast = Kast_ast

module CoreWithoutPlugins = struct
  module Print = struct
    module type T = sig
      type t
    end

    module Make (T : T) = struct
      type print_fn = T.t -> (formatter -> unit) option

      let printers : print_fn list Atomic.t = Atomic.make []

      let register_print : print_fn -> unit =
       fun f -> Atomic.set printers (f :: Atomic.get printers)

      let print : formatter -> T.t -> unit =
       fun fmt value ->
        let f =
          Atomic.get printers
          |> List.find_map (fun f -> f value)
          |> Option.unwrap_or_else (fun () -> failwith __LOC__)
        in
        f fmt
    end

    module type GadT = sig
      type _ t
    end

    module MakeGadt (T : GadT) = struct
      type print_fn = { f : 'a. 'a T.t -> (formatter -> unit) option }

      let printers : print_fn list Atomic.t = Atomic.make []

      let register_print : print_fn -> unit =
       fun f -> Atomic.set printers (f :: Atomic.get printers)

      let print : 'a. formatter -> 'a T.t -> unit =
       fun fmt value ->
        let f =
          Atomic.get printers
          |> List.find_map (fun f -> f.f value)
          |> Option.unwrap_or_else (fun () -> failwith __LOC__)
        in
        f fmt
    end

    module type GadT2 = sig
      type (_, _) t
    end

    module MakeGadt2 (T : GadT2) = struct
      type print_fn = { f : 'a 'b. ('a, 'b) T.t -> (formatter -> unit) option }

      let printers : print_fn list Atomic.t = Atomic.make []

      let register_print : print_fn -> unit =
       fun f -> Atomic.set printers (f :: Atomic.get printers)

      let print : 'a 'b. formatter -> ('a, 'b) T.t -> unit =
       fun fmt value ->
        let f =
          Atomic.get printers
          |> List.find_map (fun f -> f.f value)
          |> Option.unwrap_or_else (fun () -> failwith __LOC__)
        in
        f fmt
    end
  end

  module Ty = struct
    module T = struct
      type t = ..
    end

    include T
    include Print.Make (T)

    module type Plugin = sig
      type t
      type T.t += T of t

      val print : formatter -> t -> unit
    end

    let register_plugin : (module Plugin) -> unit =
     fun (module P) ->
      register_print (function
        | P.T value -> Some (fun fmt -> P.print fmt value)
        | _ -> None)
  end

  module Value = struct
    module T = struct
      type t = ..
    end

    include T
    include Print.Make (T)

    type typeof_fn = t -> Ty.t option

    let typeof_impls : typeof_fn list Atomic.t = Atomic.make []

    let register_typeof : typeof_fn -> unit =
     fun f -> Atomic.set typeof_impls (f :: Atomic.get typeof_impls)

    let typeof : t -> Ty.t =
     fun value ->
      Atomic.get typeof_impls
      |> List.find_map (fun f -> f value)
      |> Option.unwrap_or_else (fun () -> failwith __LOC__)
  end

  module Expr = struct
    module T = struct
      type _ t = ..
    end

    include T
    include Print.MakeGadt (T)
  end

  module Compiler = struct
    type t = unit
    type 'a compilable = Expr : Value.t Expr.t compilable

    let init () : t = ()

    type core_syntax = {
      name : string;
      impl : 'a. 'a compilable -> Ast.t -> t -> 'a;
    }

    let register_core_syntax : core_syntax -> unit = fun core_syntax -> ()
  end

  module Interpreter = struct
    type t = unit
    type eval_fn = { f : 'a. 'a Expr.t -> (t -> 'a) option }

    let init () : t = ()
    let eval_impls : eval_fn list Atomic.t = Atomic.make []

    let register_eval : eval_fn -> unit =
     fun f -> Atomic.set eval_impls (f :: Atomic.get eval_impls)

    let eval : 'a. 'a Expr.t -> t -> 'a =
     fun expr state ->
      let f =
        Atomic.get eval_impls
        |> List.find_map (fun f -> f.f expr)
        |> Option.unwrap_or_else (fun () -> failwith __LOC__)
      in
      f state
  end
end

module Core = struct
  module Ty = struct
    include CoreWithoutPlugins.Ty
  end

  module Value = struct
    include CoreWithoutPlugins.Value

    module type Plugin = sig
      type t
      type T.t += T of t

      val print : formatter -> t -> unit
      val typeof : t -> Ty.t
    end

    let register_plugin : (module Plugin) -> unit =
     fun (module P) ->
      register_print (function
        | P.T value -> Some (fun fmt -> P.print fmt value)
        | _ -> None);
      register_typeof (function
        | P.T value -> Some (P.typeof value)
        | _ -> None)
  end

  module Interpreter = struct
    include CoreWithoutPlugins.Interpreter
  end

  module Expr = struct
    include CoreWithoutPlugins.Expr

    module type Plugin = sig
      type t
      type result
      type _ T.t += T : t -> result T.t

      val eval : t -> Interpreter.t -> result
    end

    let register_plugin : (module Plugin) -> unit =
     fun (module P) ->
      Interpreter.register_eval
        {
          f =
            (fun (type a) (value : a T.t) : (Interpreter.t -> a) option ->
              match value with
              | P.T value -> Some (fun i -> P.eval value i)
              | _ -> None);
        }
  end

  module Compiler = struct
    include CoreWithoutPlugins.Compiler
  end
end

open Core

module ErrorTy = struct
  type t = unit
  type Ty.t += T of t

  let make () = ()

  let print (fmt : formatter) (_value : t) : unit =
    fprintf fmt "@{<red><error>@}"
end

let () = Ty.register_plugin (module ErrorTy)

module ErrorValue = struct
  type t = unit
  type Value.t += T of t

  let make () = T ()

  let print (fmt : formatter) (_value : t) : unit =
    fprintf fmt "@{<red><error>@}"

  let typeof (() : t) : Ty.t = ErrorTy.T (ErrorTy.make ())
end

let () = Value.register_plugin (module ErrorValue)

module Error = struct
  module Ty = ErrorTy
  module Value = ErrorValue
end

module ConstExpr = struct
  type t = { value : Value.t }
  type result = Value.t
  type _ Expr.t += T : t -> result Expr.t

  let eval (expr : t) (_interpreter : Interpreter.t) : result = expr.value
end

let () = Expr.register_plugin (module ConstExpr)

module Const = struct
  module Expr = ConstExpr
end

module UnitTy = struct
  type t = unit
  type Ty.t += T of t

  let print fmt () = fprintf fmt "()"
end

let () = Ty.register_plugin (module UnitTy)

module UnitValue = struct
  type t = unit
  type Value.t += T of t

  let print fmt () = fprintf fmt "()"
  let typeof () = UnitTy.T ()
end

let () = Value.register_plugin (module UnitValue)

let () =
  Compiler.register_core_syntax
    {
      name = "unit";
      impl =
        (fun (type a) (kind : a Compiler.compilable) _ast _compiler : a ->
          match kind with
          | Expr -> Const.Expr.T { value = UnitValue.T () });
    }

module Unit = struct
  module Ty = UnitTy
  module Value = UnitValue
end

include Core

let main () =
  let interpreter = Interpreter.init () in
  let expr = Const.Expr.T { value = Error.Value.make () } in
  let result = interpreter |> Interpreter.eval expr in
  println "%a" Value.print result

let () = main ()
