open Std
open Kast_util
open Kast_types

module InitAst = struct
  type ctx = (int, Ast.t) Hashtbl.t
  type _ Effect.t += GetCtx : ctx Effect.t

  let rec init_ast : Kast_ast.T.t -> Ast.t =
    fun ast ->
    let id = Obj.magic ast in
    let ctx = Effect.perform GetCtx in
    match Hashtbl.find_opt ctx id with
    | Some result -> result
    | None ->
      let shape : Ast.shape =
        match ast.shape with
        | Empty -> Empty
        | Simple simple -> Simple simple
        | Complex { rule; root } -> Complex { rule; root = init_ast_group root }
        | Syntax { comments_before; mode; tokens; value_after } ->
          Syntax
            { comments_before
            ; mode
            ; tokens
            ; value_after = value_after |> Option.map init_ast
            }
        | Error { parts } -> Error { parts = parts |> List.map init_ast_part }
      in
      let data : Ast.Data.t = { span = ast.data; hygiene = DefSite; def_site = None } in
      let result : Ast.t = { shape; data } in
      Hashtbl.add ctx id result;
      result

  and init_ast_child : Kast_ast.T.child -> Ast.child =
    fun child ->
    match child with
    | Ast ast -> Ast (init_ast ast)
    | Group g -> Group (init_ast_group g)

  and init_ast_group : Kast_ast.T.group -> Ast.group =
    fun { rule; parts; children; span } ->
    let children = children |> Tuple.map init_ast_child in
    { rule; parts = parts |> List.map init_ast_part; children; span }

  and init_ast_part : Kast_ast.T.part -> Ast.part =
    fun part ->
    match part with
    | Comment c -> Comment c
    | Value ast -> Value (init_ast ast)
    | Keyword k -> Keyword k
    | Group g -> Group (init_ast_group g)
  ;;

  let init_ast ast =
    let ctx : ctx = Hashtbl.create 0 in
    try init_ast ast with
    | effect GetCtx, k -> Effect.continue k ctx
  ;;
end

let init_ast = InitAst.init_ast

module InitAstDefSite = struct
  type ctx =
    { inited : (int, Ast.t) Hashtbl.t
    ; scope : Types.compiler_scope
    }

  type _ Effect.t += GetCtx : ctx Effect.t

  let rec init_ast : Ast.t -> Ast.t =
    fun ast ->
    let id = Obj.magic ast in
    let ctx = Effect.perform GetCtx in
    match Hashtbl.find_opt ctx.inited id with
    | Some result -> result
    | None ->
      let shape : Ast.shape =
        match ast.shape with
        | Empty -> Empty
        | Simple simple -> Simple simple
        | Complex { rule; root } -> Complex { rule; root = init_ast_group root }
        | Syntax { comments_before; mode; tokens; value_after } ->
          Syntax
            { comments_before
            ; mode
            ; tokens
            ; value_after = value_after |> Option.map init_ast
            }
        | Error { parts } -> Error { parts = parts |> List.map init_ast_part }
      in
      let data : Ast.Data.t =
        { span = ast.data.span
        ; hygiene = ast.data.hygiene
        ; def_site = ast.data.def_site |> Option.or_ (Some ctx.scope)
        }
      in
      let result : Ast.t = { shape; data } in
      Hashtbl.add ctx.inited id result;
      result

  and init_ast_child : Ast.child -> Ast.child =
    fun child ->
    match child with
    | Ast ast -> Ast (init_ast ast)
    | Group g -> Group (init_ast_group g)

  and init_ast_group : Ast.group -> Ast.group =
    fun { rule; parts; children; span } ->
    let children = children |> Tuple.map init_ast_child in
    { rule; parts = parts |> List.map init_ast_part; children; span }

  and init_ast_part : Ast.part -> Ast.part =
    fun part ->
    match part with
    | Comment c -> Comment c
    | Value ast -> Value (init_ast ast)
    | Keyword k -> Keyword k
    | Group g -> Group (init_ast_group g)
  ;;

  let init_ast scope ast =
    let ctx : ctx = { inited = Hashtbl.create 0; scope } in
    try init_ast ast with
    | effect GetCtx, k -> Effect.continue k ctx
  ;;
end

let init_ast_def_site_delete = InitAstDefSite.init_ast
