use (import "../common.ks").*;

module:

const type_def = (
    name :: String,
    kind :: TypeKind,
    root :: Ast.Group,
) -> Ir.TypeDefShape => (
    match kind with (
        | :Alias => (
            let def = root |> AstHelpers.expect_single_child(:None);
            :Alias (@current Compiler).parse_type(def)
        )
        | :Opaque => (
            :Opaque
        )
        | _ => (
            let def = (
                &root.children
                    |> Tuple.get_named("def")
            )^
                |> Ast.unwrap_child_value;
            let def = def
                |> AstHelpers.expect_rule("record")
                |> AstHelpers.expect_single_child(:None);
            match kind with (
                | :Enum => process_enum(.name, .def)
                | :Struct => process_struct_or_union(.name, .kind, .def)
                | :Union => process_struct_or_union(.name, .kind, .def)
                | _ => panic("unreachable")
            )
        )
    )
);

const process_enum = (
    .name :: String,
    .def :: Ast.t,
) -> Ir.TypeDefShape => (
    let mut variants = OrdSet.new();
    for variant in Ast.iter_list(
        def,
        .binary_rule_name = "union",
        .trailing_or_leading_rule_name = :Some "leading union",
    ) do (
        let variant_name = variant
            |> AstHelpers.expect_rule("variant")
            |> AstHelpers.expect_single_child(:Some "label")
            |> AstHelpers.expect_ident;
        &mut variants |> OrdSet.add(variant_name);
    );
    :Enum { .variants }
);

const process_struct_or_union = (
    .name :: String,
    .kind :: TypeKind,
    .def :: Ast.t,
) -> Ir.TypeDefShape => (
    let mut fields = OrdMap.new();
    for field in Ast.iter_list(
        def,
        .binary_rule_name = "comma",
        .trailing_or_leading_rule_name = :Some "trailing comma",
    ) do (
        let { name, ty } = field
            |> AstHelpers.expect_rule("field def")
            |> AstHelpers.expect_two_children(:Some { "label", "type" });
        let name = name |> AstHelpers.expect_ident;
        let ty = (@current Compiler).parse_type(ty);
        &mut fields |> OrdMap.add(name, ty);
    );
    match kind with (
        | :Struct => :Struct { .fields }
        | :Union => :Union { .variants = fields }
        | _ => panic("unreachable")
    )
);