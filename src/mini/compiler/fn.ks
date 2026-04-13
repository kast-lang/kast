use (import "./common.ks").*;
use (import "./scope.ks").*;

module:

const process_toplevel_fn_declaration = (
    name :: String,
    def :: Ast.t,
) -> Ir.FnType => (
    let { args, result_ty, body } = def
        |> AstHelpers.expect_rule("fn")
        |> AstHelpers.expect_three_children("args", "result_ty", "body");
    let args = args
        |> AstHelpers.expect_rule("scope")
        |> AstHelpers.expect_single_child(:None);
    let mut arg_types = ArrayList.new();
    for arg in Ast.iter_list(
        args,
        .binary_rule_name = "comma",
        .trailing_or_leading_rule_name = :Some "trailing comma",
    ) do (
        let { name, ty } = arg
            |> AstHelpers.expect_rule("type ascribe")
            |> AstHelpers.expect_two_children(:Some { "expr", "type" });
        let ty = (@current Compiler).parse_type(ty);
        &mut arg_types |> ArrayList.push_back(ty);
    );
    let result_ty = (@current Compiler).parse_type(result_ty);
    let fn_type = {
        .args = arg_types,
        .result = result_ty,
    };
    Log.debug(
        () => (
            let output = @current Output;
            output.write("fn ");
            output.write(name);
            output.write(" :: ");
            Ir.Print.fn_type(&fn_type);
        )
    );
    fn_type
);

const parse_fn_def = (
    def :: Ast.t,
    .parent_scope :: Option.t[Scope],
) -> Ir.FnDef => (
    let { args_ast, result_ty, body } = def
        |> AstHelpers.expect_rule("fn")
        |> AstHelpers.expect_three_children("args", "result_ty", "body");
    let args_ast = args_ast
        |> AstHelpers.expect_rule("scope")
        |> AstHelpers.expect_single_child(:None);
    let mut args :: ArrayList.t[Ir.FnArg] = ArrayList.new();
    for arg in Ast.iter_list(
        args_ast,
        .binary_rule_name = "comma",
        .trailing_or_leading_rule_name = :Some "trailing comma",
    ) do (
        let { name, ty } = arg
            |> AstHelpers.expect_rule("type ascribe")
            |> AstHelpers.expect_two_children(:Some { "expr", "type" });
        let name = name |> AstHelpers.expect_ident;
        let ty = (@current Compiler).parse_type(ty);
        &mut args |> ArrayList.push_back({ .name, .ty });
    );
    let result_ty = (@current Compiler).parse_type(result_ty);
    let mut scope = {
        .parent = parent_scope,
        .vars = OrdMap.new(),
    };
    for arg in &args |> ArrayList.iter do (
        &mut scope.vars |> OrdMap.add(arg^.name, arg^.ty);
    );
    with ScopeContext = scope;
    let body = (@current Compiler).parse_expr(:Some result_ty, body);
    {
        .args,
        .result_ty = result_ty,
        .body,
        .span = def.span,
    }
);

const process_toplevel_fn = (
    name :: String,
    def :: Ast.t,
) -> Ir.FnDef => (
    parse_fn_def(def, .parent_scope = :None)
);
