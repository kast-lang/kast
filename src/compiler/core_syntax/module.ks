use (import "./_common.ks").*;

const compile_module = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> AnyExpr => (
    let body = root
        |> AstHelpers.expect_single_child(:None);
    with Scope.Context = Scope.new(
        .parent = :Some (@current Scope.Context),
        .recursive = true,
    );
    let body = compile[Expr](body, .expected_ty = :Some Ty.UNIT);
    let mut bindings = ArrayList.new();
    for &{ .key = ref name, .value = ref local } in (
        &(@current Scope.Context)^.locals |> OrdMap.iter
    ) do (
        &mut bindings |> ArrayList.push_back(local^.binding);
    );
    let module_ty :: Ty = Ty.new_inferred(:Tuple Rc.new({
        .fields = (
            let mut fields = Tuple.new();
            for binding in &bindings |> ArrayList.iter do (
                &mut fields |> Tuple.add_named(
                    binding^^.name,
                    binding^^.ty,
                );
            );
            fields
        ),
    }));
    {
        .shape = :Expr :Module {
            .body,
            .bindings,
        },
        .ty = module_ty,
    }
);

include_ast impl_any_expr_syntax(
    "module",
    compile_module,
)
