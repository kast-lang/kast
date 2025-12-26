module Inference = Kast_inference_base
module Impl = Kast_types_impl

module rec Unused : sig end = struct end

(* and All : sig
  module BlockedValueShape : Impl.BlockedValue.Shape.S
  module BlockedValue : Impl.BlockedValue.S

  module ValueShape :
    Impl.Value.Shape.S
      with type Deps.VarScope.t = VarScope.t
       and type Scope.t = VarScope.t

  module ValueVar :
    Inference.Var.S
      with type Value.t = ValueShape.t
       and type Value.Scope.t = VarScope.t

  module Value : Impl.Value.S with type Deps.VarScope.t = VarScope.t
  module ValueMap : Impl.Value.Map.S
  module TyShape : Impl.Ty.Shape.S
  module TyVar : Inference.Var.S
  module Ty : Impl.Ty.S
  module Place : Impl.Place.S
  module Binding : Impl.Binding.S
  module ExprShape : Impl.Ir.Expr.Shape.S
  module Expr : Impl.Ir.Expr.S
  module AssigneeExprShape : Impl.Ir.AssigneeExpr.Shape.S
  module AssigneeExpr : Impl.Ir.AssigneeExpr.S
  module TyExprShape : Impl.Ir.TyExpr.Shape.S
  module TyExpr : Impl.Ir.TyExpr.S
  module PlaceExprShape : Impl.Ir.PlaceExpr.Shape.S
  module PlaceExpr : Impl.Ir.PlaceExpr.S
  module PatternShape : Impl.Ir.Pattern.Shape.S
  module Pattern : Impl.Ir.Pattern.S
  module CompilerScope : Impl.Compiler.Scope.S
  module InterpreterScope : Impl.Interpreter.Scope.S
  module Interpreter : Impl.Interpreter.S
  module IrData : Impl.Ir.Data.S
  module Unsorted : Impl.Unsorted.S
  module NameShape : Impl.Name.Shape.S
  module Name : Impl.Name.S
  module OptionalName : Impl.Name.Optional.S
  module TyVariantRow : Impl.Ty.VariantRow.S
  module VarScope : Inference.Scope
end = struct
  module BlockedValueShape = BlockedValueShape
  module BlockedValue = BlockedValue
  module ValueShape = ValueShape
  module ValueVar = ValueVar
  module Value = Value
  module ValueMap = ValueMap
  module TyShape = TyShape
  module TyVar = TyVar
  module Ty = Ty
  module Place = Place
  module Binding = Binding
  module ExprShape = ExprShape
  module Expr = Expr
  module AssigneeExprShape = AssigneeExprShape
  module AssigneeExpr = AssigneeExpr
  module TyExprShape = TyExprShape
  module TyExpr = TyExpr
  module PlaceExprShape = PlaceExprShape
  module PlaceExpr = PlaceExpr
  module PatternShape = PatternShape
  module Pattern = Pattern
  module InterpreterScope = InterpreterScope
  module Interpreter = Interpreter
  module CompilerScope = CompilerScope
  module IrData = IrData
  module Unsorted = Unsorted
  module Name = Name
  module NameShape = NameShape
  module OptionalName = OptionalName
  module TyVariantRow = TyVariantRow
  module VarScope = VarScope
end *)
and BlockedValueShape : Impl.BlockedValue.Shape.S =
Impl.BlockedValue.Shape.Make (struct
  module Value = Value
  module Binding = Binding
  module BlockedValue = BlockedValue
end)

and BlockedValue : Impl.BlockedValue.S = Impl.BlockedValue.Make (struct
  module Ty = Ty
  module BlockedValueShape = BlockedValueShape
end)

and TyShape : (Impl.Ty.Shape.S with type Deps.VarScope.t = VarScope.t) =
Impl.Ty.Shape.Make (struct
  module Ty = Ty
  module VarScope = VarScope
  module OptionalName = OptionalName
  module Name = Name
  module Pattern = Pattern
  module TyVariantRow = TyVariantRow
  module BlockedValue = BlockedValue
  module Unsorted = Unsorted
end)

and TyVar :
  (Inference.Var.S
    with type Value.t = TyShape.t
     and type Value.Scope.t = VarScope.t) =
  Inference.Var.Make (TyShape)

and Ty : (Impl.Ty.S with type Deps.VarScope.t = VarScope.t) =
Impl.Ty.Make (struct
  module VarScope = VarScope
  module TyVar = TyVar
end)

and ValueShape :
  (Impl.Value.Shape.S
    with type Deps.VarScope.t = VarScope.t
     and type Scope.t = VarScope.t) = Impl.Value.Shape.Make (struct end)

and ValueVar :
  (Inference.Var.S
    with type Value.t = ValueShape.t
     and type Value.Scope.t = VarScope.t) =
  Inference.Var.Make (ValueShape)

and ValueMap : Impl.Value.Map.S = Impl.Value.Map.Make (struct end)

and Value : (Impl.Value.S with type Deps.VarScope.t = VarScope.t) =
Impl.Value.Make (struct
  module VarScope = VarScope
  module ValueVar = ValueVar
  module Ty = Ty
end)

and Place : Impl.Place.S = Impl.Place.Make (struct end)
and Binding : Impl.Binding.S = Impl.Binding.Make (struct end)
and ExprShape : Impl.Ir.Expr.Shape.S = Impl.Ir.Expr.Shape.Make (struct end)
and Expr : Impl.Ir.Expr.S = Impl.Ir.Expr.Make (struct end)

and AssigneeExprShape : Impl.Ir.AssigneeExpr.Shape.S =
Impl.Ir.AssigneeExpr.Shape.Make (struct end)

and AssigneeExpr : Impl.Ir.AssigneeExpr.S = Impl.Ir.AssigneeExpr.Make (struct end)

and TyExprShape : Impl.Ir.TyExpr.Shape.S = Impl.Ir.TyExpr.Shape.Make (struct end)
and TyExpr : Impl.Ir.TyExpr.S = Impl.Ir.TyExpr.Make (struct end)

and PlaceExprShape : Impl.Ir.PlaceExpr.Shape.S =
Impl.Ir.PlaceExpr.Shape.Make (struct end)

and PlaceExpr : Impl.Ir.PlaceExpr.S = Impl.Ir.PlaceExpr.Make (struct end)

and PatternShape : Impl.Ir.Pattern.Shape.S = Impl.Ir.Pattern.Shape.Make (struct end)

and Pattern : Impl.Ir.Pattern.S = Impl.Ir.Pattern.Make (struct end)
and IrData : Impl.Ir.Data.S = Impl.Ir.Data.Make (struct end)
and CompilerScope : Impl.Compiler.Scope.S = Impl.Compiler.Scope.Make (struct end)

and InterpreterScope : Impl.Interpreter.Scope.S =
Impl.Interpreter.Scope.Make (struct end)

and Interpreter : Impl.Interpreter.S = Impl.Interpreter.Make (struct end)
and Unsorted : Impl.Unsorted.S = Impl.Unsorted.Make (struct end)
and NameShape : Impl.Name.Shape.S = Impl.Name.Shape.Make (struct end)
and NameVar : Inference.Var.S = Inference.Var.Make (NameShape)
and Name : Impl.Name.S = Impl.Name.Make (struct end)
and OptionalName : Impl.Name.Optional.S = Impl.Name.Optional.Make (struct end)
and TyVariantRow : Impl.Ty.VariantRow.S = Impl.Ty.VariantRow.Make (struct end)
and VarScope : Inference.Scope = InterpreterScope.VarScope
