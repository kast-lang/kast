use (import "../../../output.ks").*;
use std.collections.OrdSet;

module:

const Ast = (
    module:

    const Ident = newtype {
        .name :: String,
    };

    const Ty = newtype (
        | :SizeT
        | :Char
        | :Int
        | :Int32
        | :Int64
        | :Float64
        | :Bool
        | :Void
        | :Pointer Ty
        | :Named Ident
        | :Struct Ident
        | :Raw String
    );

    const FnArg = newtype {
        .name :: Ident,
        .ty :: Ty,
    };

    const FnSignature = newtype {
        .name :: Ident,
        .args :: ArrayList.t[FnArg],
        .result_ty :: Ty,
    };

    const Fn = newtype {
        .signature :: FnSignature,
        .body :: Block,
    };

    const Literal = newtype (
        | :Bool Bool
        | :Int String
        | :Float64 Float64
        | :Char Char
        | :String String
    );

    const FieldInitializer = newtype {
        .name :: Ident,
        .value :: Expr,
    };

    const Expr = newtype (
        | :Stmt Block
        | :Raw String
        | :RawParts ArrayList.t[Expr]
        | :Ref Expr
        | :Deref Expr
        | :Ident Ident
        | :Literal Literal
        | :Equal { Expr, Expr }
        | :ArrayLiteral {
            .ty :: Ty,
            .elements :: ArrayList.t[Expr],
        }
        | :CompoundLiteral {
            .ty :: Ty,
            .fields :: ArrayList.t[FieldInitializer],
        }
        | :Apply {
            .f :: Expr,
            .args :: ArrayList.t[Expr],
        }
        | :Field {
            .obj :: Expr,
            .field :: Ident,
        }
    );

    const Stmt = newtype (
        | :RawParts ArrayList.t[Expr]
        | :Expr Expr
        | :Return Expr
        | :ReturnVoid
        | :LetVar {
            .ty :: Ty,
            .ident :: Ident,
            .value :: Option.t[Expr],
        }
        | :Assign {
            .assignee :: Expr,
            .value :: Expr,
        }
        | :If {
            .cond :: Expr,
            .then_case :: Block,
            .else_case :: Option.t[Block],
        }
        | :For {
            .init :: Option.t[Stmt],
            .test :: Option.t[Expr],
            .incr :: Option.t[Stmt],
            .body :: Block,
        }
        | :Goto Ident
        | :GotoLabel Ident
    );

    const Block = newtype {
        .stmts :: ArrayList.t[Stmt],
    };

    const FieldDef = newtype {
        .name :: Ident,
        .ty :: Ty,
    };

    const TyDefShape = newtype (
        | :Struct {
            .fields :: ArrayList.t[FieldDef],
        }
        | :Union {
            .fields :: ArrayList.t[FieldDef],
        }
        | :Enum {
            .variants :: ArrayList.t[Ident],
        }
        | :FnPointer {
            .args :: ArrayList.t[Ty],
            .result_ty :: Ty,
        }
        | :Alias Ty
    );

    const TyDef = newtype {
        .name :: Ident,
        .def :: TyDefShape,
    };

    const Static = newtype {
        .@"const" :: Bool,
        .ty :: Ty,
        .name :: Ident,
        .value :: Option.t[Expr],
    };

    const Program = newtype {
        .includes :: OrdSet.t[String],
        .types :: ArrayList.t[TyDef],
        .fns :: ArrayList.t[Fn],
        .statics :: ArrayList.t[Static],
    };
);
