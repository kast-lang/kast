use (import "../../../output.ks").*;
use (import "./ast.ks").*;
use std.collections.OrdSet;
use std.collections.OrdMap;

module:

const Print = (
    module:

    const write = (s :: String) => (
        (@current Output).write(s);
    );

    const inc_indentation = () => (@current Output).inc_indentation();
    const dec_indentation = () => (@current Output).dec_indentation();

    const write_keyword = (s :: String) => (
        ansi.with_mode(
            :Magenta,
            () => (@current Output).write(s),
        );
    );

    const literal = (literal :: &Ast.Literal) => (
        match literal^ with (
            | :Bool x => ansi.with_mode(
                :Magenta,
                () => write(to_string(x)),
            )
            | :Int x => ansi.with_mode(
                :Italic,
                () => write(x),
            )
            | :Float64 x => ansi.with_mode(
                :Italic,
                () => write(to_string(x)),
            )
            | :Char c => ansi.with_mode(
                :Green,
                () => (
                    write("'");
                    write(String.escape_contents(to_string(c), .delimiter = "'"));
                    write("'");
                ),
            )
            | :String s => ansi.with_mode(
                :Green,
                () => write(String.escape(s)),
            )
        )
    );

    const ident = (ident :: &Ast.Ident) => (
        write(ident^.name);
    );

    const raw = (s :: String) => (
        ansi.with_mode(:Cyan, () => write(s));
    );

    const expr_priority = (expr :: &Ast.Expr) -> Int32 => (
        match expr^ with (
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
        if expr^ is :Literal ref literal then (
            Print.literal(literal);
            return;
        );
        let priority = expr_priority(expr);
        let need_surround_parens = priority < min_priority;
        if need_surround_parens then (
            write("(");
        );
        match expr^ with (
            | :Raw s => Print.raw(s)
            | :RawParts ref parts => (
                for part in parts |> ArrayList.iter do (
                    if part^ is :Raw s then (
                        Print.raw(s)
                    ) else (
                        Print.expr(part, .min_priority = 1000)
                    );
                );
            )
            | :Equal { ref a, ref b } => (
                Print.expr(a, .min_priority = 51);
                write(" == ");
                Print.expr(b, .min_priority = 51);
            )
            | :Stmt ref block => Print.block(block)
            | :Ref ref referenced => (
                write("&");
                Print.expr(referenced, .min_priority = 100);
            )
            | :Deref ref pointer => (
                write("*");
                Print.expr(pointer, .min_priority = 100);
            )
            | :Ident ref ident => Print.ident(ident)
            | :Literal ref literal => Print.literal(literal)
            | :ArrayLiteral { .ty = ref ty, .elements = ref elements } => (
                write("(");
                Print.ty(ty);
                write(")");
                write("{\n");
                inc_indentation();
                for element in elements |> ArrayList.iter do (
                    Print.expr(element, .min_priority = 0);
                    write(",\n");
                );
                dec_indentation();
                write("}")
            )
            | :CompoundLiteral { .ty = ref ty, .fields = ref fields } => (
                write("(");
                Print.ty(ty);
                write(")");
                write("{\n");
                inc_indentation();
                for field in fields |> ArrayList.iter do (
                    write(".");
                    Print.ident(&field^.name);
                    write(" = ");
                    Print.expr(&field^.value, .min_priority = 0);
                    write(",\n");
                );
                dec_indentation();
                write("}")
            )
            | :Apply { .f = ref f, .args = ref args } => (
                Print.expr(f, .min_priority = 200);
                write("(");
                for { i, arg } in args |> ArrayList.iter |> std.iter.enumerate do (
                    if i != 0 then write(", ");
                    Print.expr(arg, .min_priority = 0);
                );
                write(")");
            )
            | :Field { .obj = ref obj, .field = ref field } => (
                Print.expr(obj, .min_priority = 200);
                write(".");
                Print.ident(field);
            )
        );
        if need_surround_parens then (
            write(")");
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
            | :Float64 => write_keyword("double")
            | :Pointer ref t => (
                Print.ty(t);
                write("*")
            )
            | :Named ref ident => Print.ident(ident)
            | :Struct ref ident => (
                write_keyword("struct ");
                Print.ident(ident);
            )
            | :Raw s => Print.raw(s)
        )
    );

    const stmt = (stmt :: &Ast.Stmt) => (
        match stmt^ with (
            | :RawParts ref parts => (
                for part in parts |> ArrayList.iter do (
                    if part^ is :Raw s then (
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
                write(" ");
                Print.ident(ident);
                if value^ is :Some ref value then (
                    write(" = ");
                    Print.expr(value, .min_priority = 0);
                );
            )
            | :Assign { .assignee = ref assignee, .value = ref value } => (
                Print.expr(assignee, .min_priority = 0);
                write(" = ");
                Print.expr(value, .min_priority = 0);
            )
            | :If {
                .cond = ref cond,
                .then_case = ref then_case,
                .else_case = ref else_case,
            } => (
                write_keyword("if");
                write(" (");
                Print.expr(cond, .min_priority = 0);
                write(") ");
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
                write(" (");
                if init^ is :Some ref init then (
                    Print.stmt(init);
                );
                write(";");
                if test^ is :Some ref test then (
                    write(" ");
                    Print.expr(test, .min_priority = 0);
                );
                write(";");
                if incr^ is :Some ref incr then (
                    write(" ");
                    Print.stmt(incr);
                );
                write(") ");
                Print.block(body);
            )
            | :Goto ref label => (
                write_keyword("goto ");
                Print.ident(label);
            )
            | :GotoLabel ref label => (
                Print.ident(label);
                write(":");
            )
        );
    );

    const block = (block :: &Ast.Block) => (
        write("{\n");
        inc_indentation();
        for stmt in &block^.stmts |> ArrayList.iter do (
            if stmt^ is :GotoLabel _ then (
                dec_indentation();
                Print.stmt(stmt);
                write("\n");
                inc_indentation();
            ) else (
                Print.stmt(stmt);
                write(";\n");
            );
        );
        dec_indentation();
        write("}");
    );

    const fn_signature = (signature :: &Ast.FnSignature) => (
        Print.ty(&signature^.result_ty);
        write(" ");
        Print.ident(&signature^.name);
        write("(");
        for { i, arg } in &signature^.args |> ArrayList.iter |> std.iter.enumerate do (
            if i != 0 then (
                write(", ");
            );
            Print.ty(&arg^.ty);
            write(" ");
            Print.ident(&arg^.name);
        );
        write(")");
    );

    const fn = (fn :: &Ast.Fn) => (
        Print.fn_signature(&fn^.signature);
        write(" ");
        Print.block(&fn^.body);
        write("\n");
    );

    const ty_def = (def :: &Ast.TyDef) => with_return (
        write_keyword("typedef ");
        match def^.def with (
            | :Alias ref ty => Print.ty(ty)
            | :Struct { .fields = ref fields } => (
                write_keyword("struct ");
                Print.ident(&def^.name);
                write(" {\n");
                inc_indentation();
                for field in fields |> ArrayList.iter do (
                    Print.ty(&field^.ty);
                    write(" ");
                    Print.ident(&field^.name);
                    write(";\n");
                );
                dec_indentation();
                write("}");
            )
            | :Union { .fields = ref fields } => (
                write_keyword("union");
                write(" {\n");
                inc_indentation();
                for field in fields |> ArrayList.iter do (
                    Print.ty(&field^.ty);
                    write(" ");
                    Print.ident(&field^.name);
                    write(";\n");
                );
                dec_indentation();
                write("}");
            )
            | :Enum { .variants = ref variants } => (
                write_keyword("enum");
                write(" {\n");
                inc_indentation();
                for variant in variants |> ArrayList.iter do (
                    Print.ident(variant);
                    write(",\n");
                );
                dec_indentation();
                write("}");
            )
            | :FnPointer { .args = ref args, .result_ty = ref result_ty } => (
                Print.ty(result_ty);
                write(" (*");
                Print.ident(&def^.name);
                write(") (");
                for { i, arg } in args |> ArrayList.iter |> std.iter.enumerate do (
                    if i != 0 then (
                        write(", ");
                    );
                    Print.ty(arg);
                );
                write(");\n");
                return;
            )
        );
        write(" ");
        Print.ident(&def^.name);
        write(";\n");
    );

    const static = (static :: &Ast.Static) => (
        if static^.@"const" then (
            write_keyword("const ");
        );
        Print.ty(&static^.ty);
        write(" ");
        Print.ident(&static^.name);
        if static^.value is :Some ref value then (
            write(" = ");
            Print.expr(value, .min_priority = 0);
        );
    );

    const program = (program :: &Ast.Program) => (
        for &@"include" in &program^.includes |> OrdSet.iter do (
            write_keyword("#include ");
            ansi.with_mode(
                :Green,
                () => write(@"include"),
            );
            write("\n");
        );
        write("#define main minikast_main\n");
        write("\n");
        for ty in &program^.types |> ArrayList.iter do (
            Print.ty_def(ty);
        );
        write("\n");
        for fn in &program^.fns |> ArrayList.iter do (
            Print.fn_signature(&fn^.signature);
            write(";\n");
        );
        write("\n");
        for static in &program^.statics |> ArrayList.iter do (
            Print.static(static);
            write(";\n");
        );
        for fn in &program^.fns |> ArrayList.iter do (
            Print.fn(fn);
        );
        write("\n");
        write(@eval std.fs.read_file(std.path.dirname(__FILE__) + "/main.c"));
    );
);
