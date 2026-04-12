use (import "./common.ks").*;

module:

const expect_ty_enum = (
    ty :: &Ir.Type,
    .span :: Span,
) -> &OrdSet.t[String] => with_return (
    match (@current Compiler).resolve_type_aliases(ty^) with (
        | :Named name => (
            let def = &(@current Compiler).program.types
                |> OrdMap.get(name)
                |> Option.unwrap;
            match def^ with (
                | :Enum { .variants = ref variants } => (
                    return variants;
                )
                | _ => ()
            );
        )
        | _ => ()
    );
    let diagnostic = {
        .severity = :Error,
        .source = :Compiler,
        .message = () => (
            let output = @current Output;
            output.write("Expected an enum type, got ");
            Ir.Print.type_name(ty);
        ),
        .span,
        .related = ArrayList.new(),
    };
    Diagnostic.report_and_unwind(diagnostic)
);

const WithSpan = [T] newtype {
    T,
    .span :: Span,
};

const check_variant = (
    .variant :: WithSpan[String],
    .ty :: WithSpan[type (&Ir.Type)],
) -> () => (
    let variant_names = expect_ty_enum(...ty);
    if not variant_names |> OrdSet.contains(variant.0) then (
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                let output = @current Output;
                output.write("This variant doesn't exist in type ");
                Ir.Print.type_name(ty.0);
            ),
            .span = variant.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );
);

const TypeCheckContext = @context newtype {
    .fail :: [T] (() -> ()) -> T,
};

const short_type_name = (ty :: &Ir.Type) -> String => (
    match ty^ with (
        | :Any => "Any"
        | :Ref _ => "a reference"
        | :Unit => "()"
        | :Int32 => "Int32"
        | :Int64 => "Int64"
        | :Float64 => "Float64"
        | :Bool => "Bool"
        | :Char => "Char"
        | :Native s => "native " + String.escape(s)
        | :Named name => (
            let def = &(@current Compiler).program.types
                |> OrdMap.get(name)
                |> Option.unwrap;
            let short_def = match def^ with (
                | :Opaque => "opaque"
                | :Enum _ => "enum"
                | :Union _ => "union"
                | :Struct _ => "struct"
                | :Alias _ => "alias"
            );
            name + " (" + short_def + ")"
        )
        | :Fn _ => "a function"
    )
);

const type_check_impl = (expected :: &Ir.Type, actual :: &Ir.Type) => (
    let fail = () => (
        (@current TypeCheckContext).fail(
            () => (
                let output = @current Output;
                output.write("Expected ");
                output.write(short_type_name(expected));
                output.write(", got ");
                output.write(short_type_name(actual));
            )
        );
    );
    match { expected^, actual^ } with (
        | { :Any, _ } => ()
        | { _, :Any } => ()
        | { :Ref ref a, :Ref ref b } => (
            let parent_ctx = @current TypeCheckContext;
            with TypeCheckContext = {
                .fail = [T] msg -> T => (
                    parent_ctx.fail(
                        () => (
                            let output = @current Output;
                            output.write("referenced types are different");
                            msg()
                        )
                    )
                ),
            };
            type_check_impl(a, b);
        )
        | { :Unit, :Unit } => ()
        | { :Int32, :Int32 } => ()
        | { :Int64, :Int64 } => ()
        | { :Float64, :Float64 } => ()
        | { :Bool, :Bool } => ()
        | { :Char, :Char } => ()
        | { :Named a, :Named b } => (
            if a != b then (
                fail();
            );
        )
        | { :Fn ref a, :Fn ref b } => (
            if &a^.args |> ArrayList.length != &b^.args |> ArrayList.length then (
                (@current TypeCheckContext).fail(
                    () => (
                        let output = @current Output;
                        output.write("Expected ");
                        output.write(to_string(&a^.args |> ArrayList.length));
                        output.write(" args, got ");
                        output.write(to_string(&b^.args |> ArrayList.length));
                    )
                );
            );
            for i in 0..&a^.args |> ArrayList.length do (
                let parent_ctx = @current TypeCheckContext;
                with TypeCheckContext = {
                    .fail = [T] msg -> T => (
                        parent_ctx.fail(
                            () => (
                                let output = @current Output;
                                output.write("argument #");
                                output.write(to_string(i));
                                output.write(" is different\n");
                                msg()
                            )
                        )
                    ),
                };
                type_check_impl(
                    &a^.args |> ArrayList.at(i),
                    &b^.args |> ArrayList.at(i),
                );
            );
            let parent_ctx = @current TypeCheckContext;
            with TypeCheckContext = {
                .fail = [T] msg -> T => (
                    parent_ctx.fail(
                        () => (
                            let output = @current Output;
                            output.write("result type is different\n");
                            msg()
                        )
                    )
                ),
            };
            type_check_impl(
                &a^.result,
                &b^.result,
            );
        )
        | { :Native a, :Native b } => (
            if a != b then (
                fail();
            );
        )
        | _ => fail()
    );
);

const type_check = (
    .expected :: Ir.Type,
    .actual :: Ir.Type,
    .span :: Span,
) => (
    with TypeCheckContext = {
        .fail = [T] (inner_msg) -> T => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    let output = @current Output;
                    output.write("Expected type ");
                    Ir.Print.type_name(&expected);
                    output.write(", got ");
                    Ir.Print.type_name(&actual);
                    output.write("\n");
                    inner_msg();
                ),
                .span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        )
    };
    type_check_impl(
        &(@current Compiler).resolve_type_aliases(expected),
        &(@current Compiler).resolve_type_aliases(actual),
    );
);
