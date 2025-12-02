open Std
open Kast_util
open Kast_types
module Lsp = Linol_lsp
module Compiler = Kast_compiler

type 'a compiled_kind = 'a Compiler.compiled_kind

let rec complete_from_compiler_scope (scope : Types.compiler_scope) :
    Lsp.Types.CompletionItem.t StringMap.t =
  let parent_completions =
    match scope.parent with
    | None -> StringMap.empty
    | Some parent -> complete_from_compiler_scope parent
  in
  let locals =
    scope.bindings
    |> StringMap.map (fun (binding : binding) ->
        let label = binding.name.name in
        let detail = make_string "@[<v>%a@]" Ty.print binding.ty in
        let kind : Lsp.Types.CompletionItemKind.t =
          match binding.ty.var |> Kast_inference_base.Var.inferred_opt with
          | None -> Variable
          | Some ty -> (
              match ty with
              | Types.T_Unit -> Unit
              | Types.T_Ast | Types.T_UnwindToken _ | Types.T_Error
              | Types.T_Bool | Types.T_Int32 | Types.T_Int64 | Types.T_Char
              | Types.T_String | Types.T_Binding _ | Types.T_Tuple _
              | Types.T_Target | Types.T_Variant _ ->
                  Variable
              | Types.T_CompilerScope -> Folder
              | Types.T_Ty -> TypeParameter
              | Types.T_ContextTy -> TypeParameter
              | Types.T_Fn _ -> Function
              | Types.T_Generic _ -> Function)
        in
        Lsp.Types.CompletionItem.create ~label ~detail ~kind ())
  in
  StringMap.merge (fun _ a b -> b |> Option.or_ a) parent_completions locals

let rec find_expr :
    'a. 'a compiled_kind -> 'a -> position -> Common.compiled_thing option =
 fun (type a) (kind : a compiled_kind) (compiled : a) (pos : position) ->
  let data = Compiler.get_data kind compiled in
  Log.trace (fun log -> log "Considering %a" Span.print data.span);
  if Position.compare data.span.finish pos <= 0 then
    Some (Common.CompiledThing (kind, compiled))
  else if data.span |> Span.contains_position pos then
    let inner =
      Common.inner_compiled kind compiled
      |> Seq.filter_map (fun (Common.CompiledThing (kind, compiled)) ->
          find_expr kind compiled pos)
      |> Seq.fold_left
           (fun acc (Common.CompiledThing (kind, compiled) as b) ->
             match acc with
             | Some (Common.CompiledThing (akind, acompiled) as a) ->
                 let adata = Compiler.get_data akind acompiled in
                 let data = Compiler.get_data kind compiled in
                 Some
                   (if Position.compare adata.span.finish data.span.finish > 0
                    then a
                    else b)
             | None -> Some b)
           None
    in
    inner (* |> Option.or_ (Some (Common.CompiledThing (kind, compiled))) *)
  else None

let complete (type a) (kind : a compiled_kind) (compiled : a) (pos : position) =
  let* (CompiledThing (kind, compiled)) = find_expr kind compiled pos in
  let data = Compiler.get_data kind compiled in
  Log.trace (fun log -> log "Completing from %a" Span.print data.span);
  let completions = complete_from_compiler_scope data.compiler_scope in
  Log.trace (fun log ->
      log "Completed with %a"
        (List.print String.print_dbg)
        (completions |> StringMap.to_list |> List.map (fun (key, _) -> key)));
  Some
    (completions |> StringMap.to_list |> List.map (fun (_key, value) -> value))

let completions (pos : Lsp.Types.Position.t)
    ({ compiled; _ } : Processing.file_state) : Lsp.Types.CompletionItem.t list
    =
  let pos = Common.lsp_to_kast_pos pos in
  Log.info (fun log -> log "Completing at %a" Position.print pos);
  match compiled with
  | Some expr -> (
      match complete Expr expr pos with
      | Some list -> list
      | None -> [])
  | None -> []

(* [ Lsp.Types.CompletionItem.create ~label:"Hello" () ] *)
