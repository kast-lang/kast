use (import "./_common.ks").*;

const compile_apply = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> AnyExpr => (
    let f_ast = (&root^.children |> Tuple.get_named("f"))^
        |> Ast.unwrap_child_value;
    let args_ast = (&root^.children |> Tuple.get_unnamed(0))^
        |> Ast.unwrap_child_group
        |> AstHelpers.expect_single_child(:Some "args");
    let f = compile[Expr](&f_ast, .expected_ty = :None);
    let f_ty = match f.ty.shape with (
        | :Fn ty => ty
        | _ => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = f_ast.span,
                .message = () => (
                    let output = @current Output;
                    output.write("Expected a fn, got ");
                    Ty.print(&f.ty);
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        )
    );
    let mut args = ArrayList.new();
    for { i, arg_ast } in (
        Ast.iter_list(
            args_ast,
            .binary_rule_name = "core:comma",
            .trailing_or_leading_rule_name = :Some "core:trailing comma",
        )
            |> std.iter.enumerate
    ) do (
        if i >= &f_ty.args |> ArrayList.length then (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = arg_ast.span,
                .message = () => (
                    let output = @current Output;
                    output.write("Too many arguments provided");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        );
        let arg_ty = (&f_ty.args |> ArrayList.at(i))^;
        let arg = compile[Expr](&arg_ast, .expected_ty = :Some arg_ty);
        &mut args |> ArrayList.push_back(arg);
    );
    if &args |> ArrayList.length != &f_ty.args |> ArrayList.length then (
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .span = args_ast.span,
            .message = () => (
                let output = @current Output;
                output.write("Not enough arguments provided");
                output.write(": expected ");
                output.write(to_string(&f_ty.args |> ArrayList.length));
                output.write(", got ");
                output.write(to_string(&args |> ArrayList.length));
            ),
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );
    {
        .shape = :Expr :Apply { .f, .args },
        .ty = f_ty.result,
    }
);

include_ast impl_any_expr_syntax(
    "apply",
    compile_apply,
)
