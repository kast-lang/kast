use (import "../common.ks").*;

module:

const expect_ty_enum = (
    ty :: &Ir.Type,
    .span :: Span,
) -> &OrdSet.t[RcStr] => with_return (
    match ty^.shape^ with (
        | :Named ref name => (
            let def = &(@current Compiler).program^.types
                |> OrdMap.get(name)
                |> Option.unwrap;
            match def^.shape with (
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
            locked_output.write_str("Expected an enum type, got ");
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
    .variant :: WithSpan[type (&RcStr)],
    .ty :: WithSpan[type (&Ir.Type)],
) -> () => (
    let variant_names = expect_ty_enum(...ty);
    if not variant_names |> OrdSet.contains(variant.0) then (
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                locked_output.write_str("This variant doesn't exist in type ");
                Ir.Print.type_name(ty.0);
            ),
            .span = variant.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );
);

const TypeCheckContext = @context newtype {
    .fail :: (() -> ()) -> Never,
};

const short_type_name = (ty :: &Ir.Type) -> RcStr => (
    RcStr.from_String(match ty^.shape^ with (
        | :Any => String.from_str("Any")
        | :Ref _ => String.from_str("a reference")
        | :Unit => String.from_str("()")
        | :Int => String.from_str("Int")
        | :UInt => String.from_str("UInt")
        | :IntSpecific _ => output_to_string(() => Ir.Print.type_name(ty))
        | :Float32 => String.from_str("Float32")
        | :Float64 => String.from_str("Float64")
        | :Bool => String.from_str("Bool")
        | :Char => String.from_str("Char")
        | :Native ref s => format!("native \(String.escape(s |> RcStr.as_str))")
        | :Named ref name => (
            let def = &(@current Compiler).program^.types
                |> OrdMap.get(name)
                |> Option.unwrap;
            let short_def = match def^.shape with (
                | :Opaque => "opaque"
                | :Enum _ => "enum"
                | :Union _ => "union"
                | :Struct _ => "struct"
                | :Alias _ => "alias"
            );
            format!("\(name^) (\(short_def))")
        )
        | :Fn _ => format!("a function")
        | :UnwindToken _ => String.from_str("unwind token")
        | :DelimitedContinuationToken _ => String.from_str("delimited continuation token")
        | :Array _ => String.from_str("array")
        | :ContextObject => String.from_str("@Context")
    ))
);

const type_check_impl = (expected :: &Ir.Type, actual :: &Ir.Type) => (
    let fail = () => (
        (@current TypeCheckContext).fail(() => (
            locked_output.write_str("Expected ");
            locked_output.write(&short_type_name(expected));
            locked_output.write_str(", got ");
            locked_output.write(&short_type_name(actual));
        ));
    );
    match ({ &expected^.shape^, &actual^.shape^ } :: { _, _ }) with (
        | { &(:Any), _ } => ()
        | { _, &(:Any) } => ()
        | { &(:Ref ref a), &(:Ref ref b) } => (
            let parent_ctx = @current TypeCheckContext;
            with TypeCheckContext = {
                .fail = msg -> Never => (
                    parent_ctx.fail(() => (
                        locked_output.write_str("referenced types are different\n");
                        msg()
                    ))
                ),
            };
            type_check_impl(a, b);
        )
        | { &(:Unit), &(:Unit) } => ()
        | { &(:Int), &(:Int) } => ()
        | { &(:UInt), &(:UInt) } => ()
        | { &(:IntSpecific _), &(:IntSpecific _) } => (
            if std.cmp.default_compare(expected, actual) is :Equal then () else (
                fail()
            );
        )
        | { &(:Float32), &(:Float32) } => ()
        | { &(:Float64), &(:Float64) } => ()
        | { &(:Bool), &(:Bool) } => ()
        | { &(:Char), &(:Char) } => ()
        | { &(:Named ref a), &(:Named ref b) } => (
            if a^ != b^ then (
                fail();
            );
        )
        | { &(:Array ref a), &(:Array ref b) } => (
            let parent_ctx = @current TypeCheckContext;
            with TypeCheckContext = {
                .fail = msg -> Never => (
                    parent_ctx.fail(() => (
                        locked_output.write_str("list element type is different\n");
                        msg()
                    ))
                ),
            };
            type_check_impl(
                &a^.element_ty,
                &b^.element_ty,
            );
        )
        | { &(:UnwindToken ref a), &(:UnwindToken ref b) } => (
            let parent_ctx = @current TypeCheckContext;
            with TypeCheckContext = {
                .fail = msg -> Never => (
                    parent_ctx.fail(() => (
                        locked_output.write_str("unwind result type is different\n");
                        msg()
                    ))
                ),
            };
            type_check_impl(
                &a^.result_ty,
                &b^.result_ty,
            );
        )
        | { &(:DelimitedContinuationToken ref a), &(:DelimitedContinuationToken ref b) } => (
            let parent_ctx = @current TypeCheckContext;
            with TypeCheckContext = {
                .fail = msg -> Never => (
                    parent_ctx.fail(() => (
                        locked_output.write_str("unwind result type is different\n");
                        msg()
                    ))
                ),
            };
            type_check_impl(
                &a^.result_ty,
                &b^.result_ty,
            );
        )
        | { &(:Fn ref a), &(:Fn ref b) } => (
            match ({ &a^.call_convention, &b^.call_convention } :: { _, _ }) with (
                | { &(:None), &(:None) } => ()
                | { &(:Some ref a), &(:Some ref b) } => (
                    if a^ != b^ then fail();
                )
                | _ => fail()
            );
            let a_args_len = &a^.args |> ArrayList.length;
            let b_args_len = &b^.args |> ArrayList.length;
            if a_args_len != b_args_len then (
                (@current TypeCheckContext).fail(() => (
                    locked_output.write_fmt!("Expected \(a_args_len) args, got \(b_args_len)");
                ));
            );
            for i in 0..a_args_len do (
                let parent_ctx = @current TypeCheckContext;
                with TypeCheckContext = {
                    .fail = msg -> Never => (
                        parent_ctx.fail(() => (
                            locked_output.write_fmt!("argument #\(i) is different\n");
                            msg()
                        ))
                    ),
                };
                type_check_impl(&a^.args.[i], &b^.args.[i]);
            );
            let parent_ctx = @current TypeCheckContext;
            with TypeCheckContext = {
                .fail = msg -> Never => (
                    parent_ctx.fail(() => (
                        locked_output.write_str("result type is different\n");
                        msg()
                    ))
                ),
            };
            type_check_impl(&a^.result, &b^.result);
        )
        | { &(:Native ref a), &(:Native ref b) } => (
            if a^ != b^ then (
                fail();
            );
        )
        | { &(:ContextObject), &(:ContextObject) } => ()
        | _ => fail()
    );
);

const type_check = (
    .expected :: &Ir.Type,
    .actual :: &Ir.Type,
    .span :: Span,
) => (
    with TypeCheckContext = {
        .fail = (inner_msg) -> Never => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    locked_output.write_str("Expected type ");
                    Ir.Print.type_name(expected);
                    locked_output.write_str(", got ");
                    Ir.Print.type_name(actual);
                    locked_output.write_str("\n");
                    inner_msg();
                ),
                .span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        )
    };
    type_check_impl(expected, actual);
);
