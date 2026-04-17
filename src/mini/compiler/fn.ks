use (import "./common.ks").*;
use (import "./scope.ks").*;

module:

const parse_call_convention = (
    root :: Ast.Group
) -> Option.t[String] => (
    match (
        &root.children
            |> Tuple.get_named_opt("call_convention")
    ) with (
        | :None => :None
        | :Some &child => :Some (
            child
                |> Ast.unwrap_child_group
                |> AstHelpers.expect_single_child(:None)
                |> AstHelpers.expect_string
        )
    )
);

const process_toplevel_fn_declaration = (
    name :: String,
    def :: Ast.t,
) -> Ir.FnType => (
    let root = def |> AstHelpers.expect_rule("fn");
    let args_ast = root
        |> AstHelpers.get_child_ast("args");
    let result_ty = &root.children
        |> Tuple.get_named_opt("result_ty")
        |> Option.map(
            &child => child
                |> Ast.unwrap_child_group
                |> AstHelpers.expect_single_child(:None)
        );
    let body = root
        |> AstHelpers.get_child_ast("body");
    let call_convention = parse_call_convention(root);
    let args = args_ast
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
    let result_ty = match result_ty with (
        | :None => :Unit
        | :Some ast => (@current Compiler).parse_type(ast)
    );
    let fn_type = {
        .call_convention,
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
    let root = def |> AstHelpers.expect_rule("fn");
    let capture_mode = match (
        &root.children |> Tuple.get_named_opt("move")
    ) with (
        | :Some _ => :Move
        | :None => :ByRef
    );
    let args_ast = root
        |> AstHelpers.get_child_ast("args");
    let result_ty = &root.children
        |> Tuple.get_named_opt("result_ty")
        |> Option.map(
            &child => child
                |> Ast.unwrap_child_group
                |> AstHelpers.expect_single_child(:None)
        );
    let body = root
        |> AstHelpers.get_child_ast("body");
    let call_convention = parse_call_convention(root);
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
    let result_ty = match result_ty with (
        | :None => :Unit
        | :Some ast => (@current Compiler).parse_type(ast)
    );
    let mut captures = OrdMap.new();
    let mut scope = {
        .parent = parent_scope,
        .vars = OrdMap.new(),
        .found_in_parent = (name, ty) => (
            &mut captures |> OrdMap.add(name, ty);
        ),
    };
    for arg in &args |> ArrayList.iter do (
        &mut scope.vars |> OrdMap.add(arg^.name, arg^.ty);
    );
    with ScopeContext = scope;
    let body = (@current Compiler).parse_expr(:Some result_ty, body);
    {
        .capture_mode,
        .captures,
        .call_convention,
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
