use (import "../../../output.ks").*;
use (import "../../../source.ks").*;
use (import "../../../source_path.ks").*;
use (import "../../../rc_str.ks").*;
use (import "./ast.ks").*;
use std.collections.OrdSet;
use std.collections.OrdMap;

module:

const Print = (
    module:

    const Print = @current_scope; # TODO transpiler bug without this

    const write_str = (s :: &str) => locked_output.write_str(s);
    const inc_indentation = () => locked_output.inc_indentation();
    const dec_indentation = () => locked_output.dec_indentation();

    const write_keyword = (s :: &str) => (
        ansi.with_mode(
            :Magenta,
            () => write_str(s),
        );
    );

    const literal = (literal :: &Ast.Literal) => (
        match literal^ with (
            | :Bool x => ansi.with_mode(
                :Magenta,
                () => write_str(&to_string(x) |> as_str),
            )
            | :Int ref x => ansi.with_mode(
                :Italic,
                () => locked_output.write(x),
            )
            | :Float ref x => ansi.with_mode(
                :Italic,
                () => locked_output.write(x),
            )
            | :Char c => ansi.with_mode(
                :Green,
                () => (
                    write_str("'");
                    write_str(&String.escape_contents(&to_string(c) |> as_str, .delimiter = "'") |> as_str);
                    write_str("'");
                ),
            )
            | :String ref s => ansi.with_mode(
                :Green,
                () => locked_output.write_escaped(s),
            )
        )
    );

    const ident = (ident :: &Ast.Ident) => (
        locked_output.write(&ident^.name);
    );

    const raw = (s :: &RcStr) => (
        ansi.with_mode(:Cyan, () => locked_output.write(s));
    );

    const expr_priority = (expr :: &Ast.Expr) -> Int32 => (
        match expr^.shape^ with (
            | :Stmt _ => -1000
            | :Raw _ => 0
            | :RawParts _ => 0
            | :Equal _ => 50
            | :Ref _ => 100
            | :Deref _ => 100
            | :Apply _ => 200
            | :Field _ => 200
            | :Ident _ => 1000
            | :Literal _ => 1000
            | :ArrayLiteral _ => 1000
            | :CompoundLiteral _ => 1000
        )
    );

    const expr = (
        expr :: &Ast.Expr,
        .min_priority :: Int32,
    ) => with_return (
        if expr^.shape^ is :Literal ref literal then (
            Print.literal(literal);
            return;
        );
        let priority = expr_priority(expr);
        let need_surround_parens = priority < min_priority;
        if need_surround_parens then (
            write_str("(");
        );
        match expr^.shape^ with (
            | :Raw ref s => Print.raw(s)
            | :RawParts ref parts => (
                for part in parts |> ArrayList.iter do (
                    if part^.shape^ is :Raw ref s then (
                        Print.raw(s)
                    ) else (
                        Print.expr(part, .min_priority = 1000)
                    );
                );
            )
            | :Equal { ref a, ref b } => (
                Print.expr(a, .min_priority = 51);
                write_str(" == ");
                Print.expr(b, .min_priority = 51);
            )
            | :Stmt ref block => Print.block(block)
            | :Ref ref referenced => (
                write_str("&");
                Print.expr(referenced, .min_priority = 100);
            )
            | :Deref ref pointer => (
                write_str("*");
                Print.expr(pointer, .min_priority = 100);
            )
            | :Ident ref ident => Print.ident(ident)
            | :Literal ref literal => Print.literal(literal)
            | :ArrayLiteral { .ty = ref ty, .elements = ref elements } => (
                write_str("(");
                Print.ty(ty);
                write_str("[])");
                write_str("{\n");
                inc_indentation();
                for element in elements |> ArrayList.iter do (
                    Print.expr(element, .min_priority = 0);
                    write_str(",\n");
                );
                dec_indentation();
                write_str("}")
            )
            | :CompoundLiteral { .ty = ref ty, .fields = ref fields } => (
                write_str("(");
                Print.ty(ty);
                write_str(")");
                write_str("{\n");
                inc_indentation();
                for field in fields |> ArrayList.iter do (
                    write_str(".");
                    Print.ident(&field^.name);
                    write_str(" = ");
                    Print.expr(&field^.value, .min_priority = 0);
                    write_str(",\n");
                );
                dec_indentation();
                write_str("}")
            )
            | :Apply { .f = ref f, .args = ref args } => (
                Print.expr(f, .min_priority = 200);
                write_str("(");
                for { i, arg } in args |> ArrayList.iter |> std.iter.enumerate do (
                    if i != 0 then write_str(", ");
                    Print.expr(arg, .min_priority = 0);
                );
                write_str(")");
            )
            | :Field { .obj = ref obj, .field = ref field } => (
                Print.expr(obj, .min_priority = 200);
                write_str(".");
                Print.ident(field);
            )
        );
        if need_surround_parens then (
            write_str(")");
        );
    );

    const ty = (ty :: &Ast.Ty) => (
        match ty^ with (
            | :Char => write_keyword("char")
            | :SizeT => write_keyword("size_t")
            | :Void => write_keyword("void")
            | :Int => write_keyword("int")
            | :Bool => write_keyword("bool")
            | :Int32 => write_keyword("int32_t")
            | :Int64 => write_keyword("int64_t")
            | :Float32 => write_keyword("float")
            | :Float64 => write_keyword("double")
            | :Pointer ref t => (
                Print.ty(&t^^);
                write_str("*")
            )
            | :Named ref ident => Print.ident(ident)
            | :Struct ref ident => (
                write_keyword("struct ");
                Print.ident(ident);
            )
            | :Enum ref ident => (
                write_keyword("enum ");
                Print.ident(ident);
            )
            | :Union ref ident => (
                write_keyword("union ");
                Print.ident(ident);
            )
            | :Raw ref s => Print.raw(s)
        )
    );

    const stmt = (stmt :: &Ast.Stmt) => (
        match stmt^.shape^ with (
            | :RawParts ref parts => (
                for part in parts |> ArrayList.iter do (
                    if part^.shape^ is :Raw ref s then (
                        Print.raw(s)
                    ) else (
                        Print.expr(part, .min_priority = 1000)
                    );
                );
            )
            | :Expr ref expr => Print.expr(expr, .min_priority = 0)
            | :Return ref expr => (
                write_keyword("return ");
                Print.expr(expr, .min_priority = 0);
            )
            | :ReturnVoid => (
                write_keyword("return");
            )
            | :LetVar { .ty = ref ty, .ident = ref ident, .value = ref value } => (
                Print.ty(ty);
                write_str(" ");
                Print.ident(ident);
                if value^ is :Some ref value then (
                    write_str(" = ");
                    Print.expr(value, .min_priority = 0);
                );
            )
            | :Assign { .assignee = ref assignee, .value = ref value } => (
                Print.expr(assignee, .min_priority = 0);
                write_str(" = ");
                Print.expr(value, .min_priority = 0);
            )
            | :If {
                .cond = ref cond,
                .then_case = ref then_case,
                .else_case = ref else_case,
            } => (
                write_keyword("if");
                write_str(" (");
                Print.expr(cond, .min_priority = 0);
                write_str(") ");
                Print.block(then_case);
                if else_case^ is :Some ref else_case then (
                    write_keyword(" else ");
                    Print.block(else_case);
                );
            )
            | :For {
                .init = ref init,
                .test = ref test,
                .incr = ref incr,
                .body = ref body,
            } => (
                write_keyword("for");
                write_str(" (");
                if init^ is :Some ref init then (
                    Print.stmt(init);
                );
                write_str(";");
                if test^ is :Some ref test then (
                    write_str(" ");
                    Print.expr(test, .min_priority = 0);
                );
                write_str(";");
                if incr^ is :Some ref incr then (
                    write_str(" ");
                    Print.stmt(incr);
                );
                write_str(") ");
                Print.block(body);
            )
            | :Goto ref label => (
                write_keyword("goto ");
                Print.ident(label);
            )
            | :GotoLabel ref label => (
                Print.ident(label);
                write_str(":");
            )
        );
    );

    const block = (block :: &Ast.Block) => (
        write_str("{\n");
        inc_indentation();
        for stmt in &block^.stmts |> ArrayList.iter do (
            if stmt^.shape^ is :GotoLabel _ then (
                dec_indentation();
                Print.stmt(stmt);
                write_str("\n");
                inc_indentation();
            ) else (
                Print.stmt(stmt);
                write_str(";\n");
            );
        );
        dec_indentation();
        write_str("}");
    );

    const fn_signature = (signature :: &Ast.FnSignature) => (
        Print.ty(&signature^.result_ty);
        write_str(" ");
        Print.ident(&signature^.name);
        write_str("(");
        for { i, arg } in &signature^.args |> ArrayList.iter |> std.iter.enumerate do (
            if i != 0 then (
                write_str(", ");
            );
            Print.ty(&arg^.ty);
            write_str(" ");
            Print.ident(&arg^.name);
        );
        write_str(")");
    );

    const f = (f :: &Ast.Fn) => (
        Print.fn_signature(&f^.signature);
        write_str(" ");
        Print.block(&f^.body);
        write_str("\n");
    );

    const ty_def = (def :: &Ast.TyDef) => with_return (
        match def^.def with (
            | :Alias ref ty => (
                write_keyword("typedef ");
                Print.ty(ty);
                write_str(" ");
                Print.ident(&def^.name);
            )
            | :Struct { .fields = ref fields } => (
                write_keyword("struct ");
                Print.ident(&def^.name);
                write_str(" {\n");
                inc_indentation();
                for field in fields |> ArrayList.iter do (
                    Print.ty(&field^.ty);
                    write_str(" ");
                    Print.ident(&field^.name);
                    write_str(";\n");
                );
                dec_indentation();
                write_str("}");
            )
            | :Union { .fields = ref fields } => (
                write_keyword("union ");
                Print.ident(&def^.name);
                write_str(" {\n");
                inc_indentation();
                for field in fields |> ArrayList.iter do (
                    Print.ty(&field^.ty);
                    write_str(" ");
                    Print.ident(&field^.name);
                    write_str(";\n");
                );
                dec_indentation();
                write_str("}");
            )
            | :Enum { .variants = ref variants } => (
                write_keyword("enum ");
                Print.ident(&def^.name);
                write_str(" {\n");
                inc_indentation();
                for variant in variants |> ArrayList.iter do (
                    Print.ident(variant);
                    write_str(",\n");
                );
                dec_indentation();
                write_str("}");
            )
            | :FnPointer { .args = ref args, .result_ty = ref result_ty } => (
                write_keyword("typedef ");
                Print.ty(result_ty);
                write_str(" (*");
                Print.ident(&def^.name);
                write_str(") (");
                for { i, arg } in args |> ArrayList.iter |> std.iter.enumerate do (
                    if i != 0 then (
                        write_str(", ");
                    );
                    Print.ty(arg);
                );
                write_str(")");
            )
        );
    );

    const static = (static :: &Ast.Static) => (
        if static^.@"const" then (
            write_keyword("const ");
        );
        Print.ty(&static^.ty);
        write_str(" ");
        Print.ident(&static^.name);
        if static^.value is :Some ref value then (
            write_str(" = ");
            Print.expr(value, .min_priority = 0);
        );
    );

    const program = (program :: &Ast.Program) => (
        for @"include" in &program^.includes |> OrdSet.iter do (
            write_keyword("#include ");
            ansi.with_mode(
                :Green,
                () => locked_output.write(@"include"),
            );
            write_str("\n");
        );
        write_str("#define main minikast_main\n");
        write_str("\n");
        for ty in &program^.types |> ArrayList.iter do (
            if ty^.def is :Struct _ then (
                write_keyword("struct ");
                Print.ident(&ty^.name);
                write_str(";\n");
            );
        );
        write_str("\n");
        for ty in &program^.types |> ArrayList.iter do (
            Print.ty_def(ty);
            write_str(";\n");
        );
        write_str("\n");
        for f in &program^.fns |> ArrayList.iter do (
            Print.fn_signature(&f^.signature);
            write_str(";\n");
        );
        write_str("\n");
        for static in &program^.statics |> ArrayList.iter do (
            Print.static(static);
            write_str(";\n");
        );
        for f in &program^.fns |> ArrayList.iter do (
            Print.f(f);
        );
        write_str("\n");
        locked_output.write(&Source.read(&SourcePath.parse("kast:///mini/backends/c/main.c")).contents);
    );
);
