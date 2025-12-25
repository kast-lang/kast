module Inference = Kast_inference_base
module Impl = Kast_types_impl

module rec Unused : sig end = struct end

and All : sig
  module BlockedValueShape : Impl.BlockedValue.Shape.S
  module BlockedValue : Impl.BlockedValue.S
  module ValueShape : Impl.Value.Shape.S
  module ValueVar : Inference.Var.S
  module Value : Impl.Value.S
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
end

and BlockedValueShape : Impl.BlockedValue.Shape.S =
  Impl.BlockedValue.Shape.Make (All)

and BlockedValue : Impl.BlockedValue.S = Impl.BlockedValue.Make (All)
and TyShape : Impl.Ty.Shape.S = Impl.Ty.Shape.Make (All)
and Ty : Impl.Ty.S = Impl.Ty.Make (All)
and ValueShape : Impl.Value.Shape.S = Impl.Value.Shape.Make (All)
and ValueVar : Inference.Var.S = Inference.Var.Make (ValueShape)
and ValueMap : Impl.Value.Map.S = Impl.Value.Map.Make (All)
and Value : Impl.Value.S = Impl.Value.Make (All)
and Place : Impl.Place.S = Impl.Place.Make (All)
and Binding : Impl.Binding.S = Impl.Binding.Make (All)
and ExprShape : Impl.Ir.Expr.Shape.S = Impl.Ir.Expr.Shape.Make (All)
and Expr : Impl.Ir.Expr.S = Impl.Ir.Expr.Make (All)

and AssigneeExprShape : Impl.Ir.AssigneeExpr.Shape.S =
  Impl.Ir.AssigneeExpr.Shape.Make (All)

and AssigneeExpr : Impl.Ir.AssigneeExpr.S = Impl.Ir.AssigneeExpr.Make (All)
and TyExprShape : Impl.Ir.TyExpr.Shape.S = Impl.Ir.TyExpr.Shape.Make (All)
and TyVar : Inference.Var.S = Inference.Var.Make (TyShape)
and TyExpr : Impl.Ir.TyExpr.S = Impl.Ir.TyExpr.Make (All)

and PlaceExprShape : Impl.Ir.PlaceExpr.Shape.S =
  Impl.Ir.PlaceExpr.Shape.Make (All)

and PlaceExpr : Impl.Ir.PlaceExpr.S = Impl.Ir.PlaceExpr.Make (All)
and PatternShape : Impl.Ir.Pattern.Shape.S = Impl.Ir.Pattern.Shape.Make (All)
and Pattern : Impl.Ir.Pattern.S = Impl.Ir.Pattern.Make (All)
and IrData : Impl.Ir.Data.S = Impl.Ir.Data.Make (All)
and CompilerScope : Impl.Compiler.Scope.S = Impl.Compiler.Scope.Make (All)

and InterpreterScope : Impl.Interpreter.Scope.S =
  Impl.Interpreter.Scope.Make (All)

and Interpreter : Impl.Interpreter.S = Impl.Interpreter.Make (All)
and Unsorted : Impl.Unsorted.S = Impl.Unsorted.Make (All)
and NameShape : Impl.Name.Shape.S = Impl.Name.Shape.Make (All)
and NameVar : Inference.Var.S = Inference.Var.Make (NameShape)
and Name : Impl.Name.S = Impl.Name.Make (All)
and OptionalName : Impl.Name.Optional.S = Impl.Name.Optional.Make (All)
and TyVariantRow : Impl.Ty.VariantRow.S = Impl.Ty.VariantRow.Make (All)
and VarScope : Inference.Scope = InterpreterScope.VarScope
