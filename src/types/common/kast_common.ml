include Std
include Kast_util
module Inference = Kast_inference_base
module Ast = Kast_ast
module Syntax = Kast_syntax
module Label = Label
module Binding = Binding

type 'a tuple_part_of =
  | Field of 'a tuple_field_of
  | Unpack of 'a

and 'a tuple_of = { parts : 'a tuple_part_of list }

and 'a tuple_field_of = {
  label_span : Span.t;
  label : Label.t option;
  field : 'a;
}
[@@deriving eq, ord]

module InferrableBool = struct
  type t = bool

  let equal = Bool.equal
  let compare = Bool.compare

  module Scope = Inference.NoScope

  let scope _ = Scope.root ()

  let unite ~span a b =
    if Bool.equal a b then a
    else (
      Inference.Error.error span "bool %b != %b" a b;
      a)

  let error () = failwith __LOC__
end

module BoolVar = Inference.Var.Make (InferrableBool)

module Template_ = struct
  module type Deps = sig end

  module type S = sig
    module Deps : Deps
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps
  end
end
