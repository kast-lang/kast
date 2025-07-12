open Std
open Kast_util
module Token = Kast_token
module Syntax = Kast_syntax
module Ast = Kast_ast

module Scope = struct
  type scope = {
    parent : scope option;
    bindings : Binding.t StringMap.t;
  }

  type t = scope

  let init () : scope = { parent = None; bindings = StringMap.empty }

  let enter ~(parent : scope) : scope =
    { parent = Some parent; bindings = StringMap.empty }

  let rec find_binding_opt : span -> string -> scope -> Binding.t option =
   fun span ident scope ->
    match StringMap.find_opt ident scope.bindings with
    | Some binding ->
        binding.references <- span :: binding.references;
        Some binding
    | None -> scope.parent |> Option.and_then (find_binding_opt span ident)

  let find_binding : span -> string -> scope -> Binding.t =
   fun span ident scope ->
    scope
    |> find_binding_opt span ident
    |> Option.unwrap_or_else (fun () : Binding.t ->
           Error.throw span "Could not find %S in scope" ident;
           {
             name = Symbol.create ident;
             span;
             ty = Ty.new_not_inferred ();
             references = [];
           })

  let inject_binding : Binding.t -> scope -> scope =
   fun binding { parent; bindings } ->
    { parent; bindings = bindings |> StringMap.add binding.name.name binding }
end

module Import = struct
  module Result = struct
    type t = {
      custom_syntax_impls : (Id.t, Value.t) Hashtbl.t;
      value : Value.t;
    }
  end

  module State = struct
    type t =
      | InProgress
      | Imported of Result.t
  end

  module Cache = struct
    type t = { mutable by_uri : State.t UriMap.t }

    let init () = { by_uri = UriMap.empty }
  end
end

module Plugin = struct
  module type S = sig
    type t

    val id : Id.t
    val init : unit -> t
  end

  type data = Data : 'a. (module S with type t = 'a) * 'a -> data

  let registered : (Id.t, (module S)) Hashtbl.t = Hashtbl.create 0

  let register : (module S) -> unit =
   fun (module P) -> Hashtbl.add registered P.id (module P)

  module Storage = struct
    type t = (Id.t, data) Hashtbl.t

    let init () = Hashtbl.create 0

    let get : 'a. (module S with type t = 'a) -> t -> 'a =
     fun (type a) (module P : S with type t = a) (storage : t) : a ->
      let (Data ((module AlsoP), data)) =
        Hashtbl.find_opt storage P.id
        |> Option.unwrap_or_else (fun () ->
               fail "Compiler plugin wasn't registered")
      in
      (* HAHA *)
      Obj.magic data
  end
end

type t = {
  (* TODO do this properly *)
  mutable scope : Scope.t;
  mutable currently_compiled_file : Uri.t option;
  import_cache : Import.Cache.t;
  interpreter : Interpreter.t;
  plugins : Plugin.Storage.t;
}

type state = t

let blank ~import_cache =
  {
    scope = Scope.init ();
    currently_compiled_file = None;
    import_cache;
    interpreter = Interpreter.default ();
    plugins = Plugin.Storage.init ();
  }

let enter_scope : state -> state =
 fun { scope; currently_compiled_file; interpreter; import_cache; plugins } ->
  {
    scope = Scope.enter ~parent:scope;
    currently_compiled_file;
    interpreter;
    import_cache;
    plugins;
  }

type core_syntax = {
  name : string;
  impl : 'a. t -> 'a Compilable.t -> Ast.t -> Ast.group -> 'a;
}

let registered_core_syntax : core_syntax StringMap.t Atomic.t =
  Atomic.make StringMap.empty

let register_core_syntax : core_syntax -> unit =
 fun core_syntax ->
  Log.info (fun log -> log "Registered core syntax %S" core_syntax.name);
  Atomic.set registered_core_syntax
    (Atomic.get registered_core_syntax
    |> StringMap.add core_syntax.name core_syntax)

type registered_expr = Expr.Shape.t -> (span -> t -> Ty.t) option

let registered_exprs : registered_expr list Atomic.t = Atomic.make []

let register_expr : registered_expr -> unit =
 fun f -> Atomic.set registered_exprs (f :: Atomic.get registered_exprs)

type registered_assignee = Assignee.Shape.t -> (span -> t -> Ty.t) option

let registered_assignees : registered_assignee list Atomic.t = Atomic.make []

let register_assignee : registered_assignee -> unit =
 fun f -> Atomic.set registered_assignees (f :: Atomic.get registered_assignees)

type registered_pattern = Pattern.Shape.t -> (span -> t -> Ty.t) option

let registered_patterns : registered_pattern list Atomic.t = Atomic.make []

let register_pattern : registered_pattern -> unit =
 fun f -> Atomic.set registered_patterns (f :: Atomic.get registered_patterns)

module Init = struct
  let expr : span -> t -> Expr.Shape.t -> Expr.t =
   fun span compiler shape ->
    {
      shape;
      span;
      ty =
        (let f =
           Atomic.get registered_exprs
           |> List.find_map (fun f -> f shape)
           |> Option.unwrap_or_else (fun () -> failwith __LOC__)
         in
         f span compiler);
    }

  let assignee : span -> t -> Assignee.Shape.t -> Assignee.t =
   fun span compiler shape ->
    {
      shape;
      span;
      ty =
        (let f =
           Atomic.get registered_assignees
           |> List.find_map (fun f -> f shape)
           |> Option.unwrap_or_else (fun () -> failwith __LOC__)
         in
         f span compiler);
    }

  let pattern : span -> t -> Pattern.Shape.t -> Pattern.t =
   fun span compiler shape ->
    {
      shape;
      span;
      ty =
        (let f =
           Atomic.get registered_patterns
           |> List.find_map (fun f -> f shape)
           |> Option.unwrap_or_else (fun () -> failwith __LOC__)
         in
         f span compiler);
    }

  let ty : span -> t -> Expr.Ty.Shape.t -> Expr.Ty.t =
   fun span _compiler shape -> { shape; span; ty = Ty.inferred Ty.Shape.Ty }
end

type custom_syntax_handler = {
  expand :
    'a. span -> Ast.complex -> t -> 'a Compilable.t -> (Ast.t, unit) Result.t;
}

let custom_syntax_handler : custom_syntax_handler option Atomic.t =
  Atomic.make None

let register_custom_syntax_handler : custom_syntax_handler -> unit =
 fun f ->
  if Atomic.get custom_syntax_handler |> Option.is_some then
    fail "Custom syntax handler was set twice";
  Atomic.set custom_syntax_handler (Some f)

let rec compile : 'a. 'a Compilable.t -> Ast.t -> t -> 'a =
 fun (type a) (kind : a Compilable.t) (ast : Ast.t) (compiler : t) : a ->
  let span = ast.span in
  try
    match ast.shape with
    | Ast.Error _ ->
        Error.throw span "Trying to compile error node";
        Compilable.error span kind
    | Ast.Simple { token; _ } -> (
        match kind with
        | Expr -> (
            match token.shape with
            | Token.Shape.Ident ident ->
                Expr.Shape.Binding
                  (Scope.find_binding ast.span ident.name compiler.scope)
                |> Init.expr span compiler
            | Token.Shape.String s ->
                failwith __LOC__
                (* E_Constant { shape = V_String s.contents } |> init_expr span *)
            | Token.Shape.Number { raw; _ } ->
                failwith __LOC__
                (* let value = Int32.of_string raw in
                E_Constant { shape = V_Int32 value } |> init_expr span *)
            | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
                unreachable "!")
        | TyExpr -> (
            match token.shape with
            | Token.Shape.Ident ident ->
                Expr.Ty.Shape.Binding
                  (Scope.find_binding ast.span ident.name compiler.scope)
                |> Init.ty span compiler
            | Token.Shape.String _ ->
                Error.throw span "String literals dont work as types";
                Compilable.error span kind
            | Token.Shape.Number _ ->
                Error.throw span "Number literals dont work as types";
                Compilable.error span kind
            | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
                unreachable "!")
        | Assignee -> (
            match token.shape with
            | Token.Shape.Ident ident ->
                Assignee.Shape.Binding
                  (Scope.find_binding ast.span ident.name compiler.scope)
                |> Init.assignee span compiler
            | Token.Shape.String _ ->
                Error.throw span "String literals dont work as assignee";
                Compilable.error span kind
            | Token.Shape.Number _ ->
                Error.throw span "Number literals dont work as assignee";
                Compilable.error span kind
            | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
                unreachable "!")
        | Pattern -> (
            match token.shape with
            | Token.Shape.Ident ident ->
                let binding : Binding.t =
                  {
                    name = Symbol.create ident.name;
                    ty = Ty.new_not_inferred ();
                    span = ast.span;
                    references = [];
                  }
                in
                Pattern.Shape.Binding binding |> Init.pattern span compiler
            | Token.Shape.String _ ->
                Error.throw span "String literals dont work as pattern";
                Compilable.error span kind
            | Token.Shape.Number _ ->
                Error.throw span "Number literals dont work as pattern";
                Compilable.error span kind
            | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
                unreachable "!"))
    | Ast.Complex { rule; root } -> (
        match rule.name |> String.strip_prefix ~prefix:"core:" with
        | Some name -> (
            match
              Atomic.get registered_core_syntax |> StringMap.find_opt name
            with
            | Some core_syntax -> core_syntax.impl compiler kind ast root
            | None ->
                Error.throw span "Unknown core syntax %S" name;
                Compilable.error span kind)
        | None -> (
            let custom_syntax_handler =
              Atomic.get custom_syntax_handler
              |> Option.unwrap_or_else (fun () ->
                     fail "Custom syntax handler not set")
            in
            match
              custom_syntax_handler.expand span { rule; root } compiler kind
            with
            | Ok expanded -> compiler |> compile kind expanded
            | Error () ->
                Error.throw span "Failed to expand custom syntax";
                Compilable.error span kind))
    | Ast.Syntax { mode; value_after; comments_before = _; tokens = _ } -> (
        match value_after with
        | Some value -> compiler |> compile kind value
        | None -> Compilable.unit span kind)
  with exc ->
    Log.error (fun log ->
        log "While compiling %a %a at %a" Compilable.print kind
          Ast.Shape.print_short ast.shape Span.print ast.span);
    raise exc

module Eval = struct
  type _ Effect.t += Evaled : 'a. ('a Compilable.t * 'a) -> unit Effect.t

  let ty ast compiler =
    let compiled = compiler |> compile TyExpr ast in
    Effect.perform (Evaled (TyExpr, compiled));
    let result = compiler.interpreter |> Interpreter.Eval.ty compiled in
    result

  let expr ast compiler =
    let compiled = compiler |> compile Expr ast in
    Effect.perform (Evaled (Expr, compiled));
    let result = compiler.interpreter |> Interpreter.Eval.expr compiled in
    result
end

let init () = ()
