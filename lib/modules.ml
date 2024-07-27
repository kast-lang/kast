open Prelude
open Types

module type Cast = sig
  val find_trait_id : value -> id
  val perform : value -> trait:value -> value
  val check : value -> trait:value -> bool
  val add_rule : value -> trait:value -> impl:value -> unit
end

module type TypeId = sig
  val get : value_type -> id
  val to_type : id -> value_type
end

module type Builtins = sig
  val all : unit -> value StringMap.t
end

module type Inference = sig
  val get_inferred : inference_var -> value option
  val get_inferred_as_type : inference_var -> value_type
  val get_type_of_var : inference_var -> value_type
  val new_var : unit -> inference_var
  val new_set_var : value -> inference_var
  val make_same : inference_var -> inference_var -> unit
  val set : inference_var -> value -> unit
  val add_check : inference_var -> (value -> bool) -> unit
  val show_id : inference_var -> string
  val get_id : inference_var -> Id.t
  val unite_types : value_type -> value_type -> value_type
end

module type Show = sig
  val show_or : 'a. string -> ('a -> string) -> 'a option -> string
  val show : value -> string
  val show_type : value_type -> string
  val show_pattern : pattern -> string
  val show_ir : ir -> string
  val show_fn_ast : fn_ast -> string
  val show_fn_type : fn_type -> string
  val show_contexts : contexts -> string
  val ir_name : 'a. 'a ir_node -> string
end

module type Interpreter = sig
  exception Unwind of id * value

  val empty_state : unit -> state
  val eval : state ref -> string -> filename:string -> value
  val eval_file : state ref -> filename:string -> value
  val eval_ast : state -> ast -> evaled
  val eval_ir : state -> ir -> evaled
  val eval_call : value -> value -> contexts -> value
  val default_contexts_type : contexts_type
  val empty_contexts : contexts
  val empty_contexts_type : contexts_type
  val get_local_opt : state -> string -> local option
  val get_local_value_opt : state -> string -> value option
  val call_compiled : contexts -> compiled_fn -> value -> value
  val discard : value -> unit
  val get_field_opt : value -> string -> value option
  val log_state : Log.level -> state -> unit
end

module type Compiler = sig
  val type_of_value : value -> ensure:bool -> value_type
  val expand_ast : state -> ast -> new_bindings:bool -> expanded_macro
  val compile_pattern : state -> ast option -> pattern
  val compile_ast_to_ir : state -> ast -> compiled
  val init_pattern : no_data pattern_node -> pattern
  val init_ir : no_data ir_node -> ir
  val pattern_bindings : pattern -> binding StringMap.t
  val new_fn_type_vars : unit -> fn_type_vars
  val fn_type_vars_to_type : fn_type_vars -> fn_type
  val value_to_contexts_type : value -> contexts_type
  val ensure_compiled : fn -> compiled_fn
end

module type Utils = sig
  val value_to_type : value -> value_type

  val update_locals :
    local StringMap.t -> local StringMap.t -> local StringMap.t
end
