use (import "../common.ks").*;
use (import "../template.ks").*;

module:

const type_info_const_name = (ty :: &Ir.Type) -> RcStr => with_return (
    let span :: Span = {
        .start = Position.beginning(),
        .end = Position.beginning(),
        .path = :Special RcStr.allocate(__FILE__)
    };
    let name = RcStr.from_String(output_to_string(() => (
        Ir.Print.type_name_as_ident(ty);
        locked_output.write_str("_TypeInfo");
    )));
    let mut ctx = @current Compiler;
    if &ctx.program^.consts |> OrdMap.get(&name) is :Some _ then (
        return name;
    );
    let type_info_ty :: Ir.Type = {
        # TODO allocate once?
        .shape = Rc.new(:Named RcStr.allocate("TypeInfo")),
        .alias_name = :None,
    };
    # Temporarily add as uninitialized to prevent cycles
    &mut ctx.program^.consts
        |> OrdMap.add(
            name,
            {
                .shape = Rc.new(:Uninitialized),
                .ty = type_info_ty,
                .span,
            }
        );
    let mut members = ArrayList.new();
    let add_members = (map :: &OrdMap.t[RcStr, Ir.Type]) => (
        for &{ .key = name, .value = ty } in map |> OrdMap.iter do (
            let member = {
                .name,
                .type_info_const_name = type_info_const_name(&ty),
                .ty,
            };
            &mut members |> ArrayList.push_back(member);
        );
    );
    match Ir.type_repr(ty)^.shape^ with (
        | :Named ref name => (
            let def = &(@current Compiler).program^.types
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
    &mut ctx.program^.consts
        |> OrdMap.add(
            name,
            {
                .shape = Rc.new(:ConstructTypeInfo {
                    .ty = ty^,
                    .members,
                }),
                .ty = type_info_ty,
                .span,
            }
        );
    &mut ctx.program^.consts_order |> ArrayList.push_back(name);
    name
);
