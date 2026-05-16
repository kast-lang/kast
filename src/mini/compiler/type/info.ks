use (import "../common.ks").*;
use (import "../template.ks").*;

module:

const type_info_const_name = (ty :: &Ir.Type) -> String => with_return (
    let span :: Span = {
        .start = Position.beginning(),
        .end = Position.beginning(),
        .path = :Special __FILE__
    };
    let name = output_to_string(
        () => (
            Ir.Print.type_name_as_ident(ty);
            (@current Output).write("_TypeInfo");
        )
    );
    let mut ctx = @current Compiler;
    if &ctx.program.consts |> OrdMap.get(name) is :Some _ then (
        return name;
    );
    let type_info_ty :: Ir.Type = {
        .shape = :Named "TypeInfo",
        .alias_name = :None,
    };
    # Temporarily add as uninitialized to prevent cycles
    &mut ctx.program.consts
        |> OrdMap.add(
            name,
            {
                .shape = :Uninitialized,
                .ty = type_info_ty,
                .span,
            }
        );
    let mut members = ArrayList.new();
    let add_members = (map :: &OrdMap.t[String, Ir.Type]) => (
        for &{ .key = name, .value = ty } in map |> OrdMap.iter do (
            let member = {
                .name,
                .type_info_const_name = type_info_const_name(&ty),
                .ty,
            };
            &mut members |> ArrayList.push_back(member);
        );
    );
    match Ir.type_repr(ty)^.shape with (
        | :Named name => (
            let def = &(@current Compiler).program.types
                |> OrdMap.get(name)
                |> Option.unwrap;
            match def^.shape with (
                | :Struct { .fields = ref fields } => (
                    add_members(fields);
                )
                | :Union { .variants = ref variants } => (
                    add_members(variants);
                )
                | _ => ()
            )
        )
        | _ => ()
    );
    &mut ctx.program.consts
        |> OrdMap.add(
            name,
            {
                .shape = :ConstructTypeInfo {
                    .ty = ty^,
                    .members,
                },
                .ty = type_info_ty,
                .span,
            }
        );
    &mut ctx.program.consts_order |> ArrayList.push_back(name);
    name
);
# const type_info_js = (ty :: &Ir.Type) -> Ir.ExprShape => (
#     let ctx = @current Compiler;
#     let span :: Span = {
#         .start = Position.beginning(),
#         .end = Position.beginning(),
#         .path = :Special __FILE__
#     };
#     let details_members = (
#         members_map :: &OrdMap.t[String, Ir.Type],
#     ) -> Ir.Expr => (
#         let mut members = ArrayList.new();
#         for &{
#             .key = name,
#             .value = ref ty,
#         } in members_map |> OrdMap.iter do (
#             let mut fields = ArrayList.new();
#             let offset_or_name = {
#                 .name = "offset_or_name",
#                 .value = {
#                     .shape = :Literal :String name,
#                     .ty = :Named "StringView",
#                     .span,
#                 },
#             };
#             &mut fields |> ArrayList.push_back(offset_or_name);
#             let ty = {
#                 .name = "ty",
#                 .value = {
#                     .shape = :Ref type_info(ty),
#                     .ty = :Ref type_info_ty,
#                     .span,
#                 },
#             };
#             &mut fields |> ArrayList.push_back(ty);
#             &mut members
#                 |> ArrayList.push_back(
#                     {
#                         .shape = :Record fields,
#                         .ty = :Named "MemberInfo",
#                         .span,
#                     }
#                 );
#         );
#         let mut details = ArrayList.new();
#         &mut details
#             |> ArrayList.push_back(
#                 {
#                     .name = "members",
#                     .value = {
#                         .shape = :Native (
#                             let mut parts = ArrayList.new();
#                             &mut parts |> ArrayList.push_back(:Raw "[");
#                             for member in members |> ArrayList.into_iter do (
#                                 &mut parts |> ArrayList.push_back(:Interpolated member);
#                                 &mut parts
#                                     |> ArrayList.push_back(:Raw ", ");
#                             );
#                             &mut parts |> ArrayList.push_back(:Raw "]");
#                             { .parts }
#                         ),
#                         .ty = instantiate_ty(
#                             "List",
#                             single_element_list(:Named "MemberInfo"),
#                             .span,
#                         ),
#                         .span,
#                     },
#                 }
#             );
#         {
#             .shape = :Record details,
#             .ty = :Named "TypeInfoDetails",
#             .span,
#         }
#     );
#     let details_primitive = () -> Ir.Expr => {
#         .shape = :Record (
#             let mut fields = ArrayList.new();
#             &mut fields
#                 |> ArrayList.push_back(
#                     {
#                         .name = "primitive",
#                         .value = {
#                             .shape = :Unit,
#                             .ty = :Unit,
#                             .span,
#                         },
#                     }
#                 );
#             fields
#         ),
#         .ty = :Named "TypeInfoDetails",
#         .span,
#     };
#     let details_inner_ty = (
#         inner_ty :: &Ir.Type,
#     ) -> Ir.Expr => {
#         .shape = :Record (
#             let mut fields = ArrayList.new();
#             &mut fields
#                 |> ArrayList.push_back(
#                     {
#                         .name = "inner_ty",
#                         .value = {
#                             .shape = :Ref type_info(inner_ty),
#                             .ty = :Unit,
#                             .span,
#                         },
#                     }
#                 );
#             fields
#         ),
#         .ty = :Named "TypeInfoDetails",
#         .span,
#     };
#     let { kind, details } = match ty^ with (
#         | :Named name => (
#             let def = &ctx.program.types
#                 |> OrdMap.get(name)
#                 |> Option.unwrap;
#             match def^.shape with (
#                 | :Struct { .fields = ref fields } => (
#                     { "Object", details_members(fields) }
#                 )
#                 | :Union { .variants = ref variants } => (
#                     { "Object", details_members(variants) }
#                 )
#                 | :Enum _ => { "Primitive", details_primitive() }
#                 | :Opaque => { "Primitive", details_primitive() }
#                 | :Alias _ => (
#                     let diagnostic = {
#                         .severity = :Error,
#                         .source = :Internal,
#                         .message = () => (
#                             let output = @current Output;
#                             output.write("type info is supposed to get alias-resolved type, got ");
#                             Ir.Print.type_name(ty);
#                         ),
#                         .span,
#                         .related = ArrayList.new(),
#                     };
#                     Diagnostic.report_and_unwind(diagnostic)
#                 )
#             )
#         )
#         | _ => { "Primitive", details_primitive() }
#     );
#     let mut fields = ArrayList.new();
#     let details = {
#         .name = "details",
#         .value = details,
#     };
#     &mut fields |> ArrayList.push_back(details);
#     let kind = {
#         .name = "kind",
#         .value = {
#             .shape = :Variant kind,
#             .ty = :Named "TypeKind",
#             .span,
#         },
#     };
#     &mut fields |> ArrayList.push_back(kind);
#     :Record fields
# );

