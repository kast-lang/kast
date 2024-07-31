module rec Builtins : Modules.Builtins =
  BuiltinsF.Make (Interpreter) (Compiler) (Inference) (Utils) (Show) (TypeId) (Javascript)

and Interpreter : Modules.Interpreter =
  InterpreterF.Make (Compiler) (Show) (Utils) (Inference) (Builtins) (TypeId)
    (Cast)

and Show : Modules.Show = ShowF.Make (Inference) (TypeId)

and Cast : Modules.Cast =
  CastF.Make (TypeId) (Show) (Interpreter) (Inference) (Utils)

and Utils : Modules.Utils = UtilsF.Make (Inference) (Show)

and Inference : Modules.Inference =
  InferenceF.Make (Show) (Compiler) (Interpreter) (Utils)

and TypeId : Modules.TypeId = TypeIdF.Make (Inference)

and Compiler : Modules.Compiler =
  CompilerF.Make (Interpreter) (Inference) (Show) (Utils) (TypeId)

and Javascript : Modules.Javascript =
  JavascriptF.Make (Show) (Interpreter) (Compiler) (Utils)
