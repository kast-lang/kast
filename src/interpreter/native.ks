use (import "./_common.ks").*;

module:

const Native = (
    module:

    const t = newtype {
        .get :: Ty -> Value,
    };

    const Map = OrdMap.t[RcStr, t];

    const InitContext = @context type (&mut Map);

    const init_value = (name :: &str, value :: Value) => (
        # TODO value is moved? rc?
        (@current InitContext) |> OrdMap.add(RcStr.allocate(name), { .get = _ => value });
    );

    const init_ty = (name :: &str, ty :: Ty) => (
        init_value(&format!("type \(name)") |> as_str, Rc.new({ .shape = :Type ty, .ty = Ty.TYPE }));
    );

    const init_op = (name :: &str, f :: (Int, Int) -> Int) => (
        let @"impl" = (args :: ArrayList.t[ValueWithSpan], .caller :: Span) -> Value => (
            if &args |> ArrayList.length != 2 then (
                panic("Expected 2 args");
            );
            let a = args.[0] |> expect_value.with_span.into_int;
            let b = args.[1] |> expect_value.with_span.into_int;
            let result = f(a, b);
            Rc.new({ .shape = :Int result, .ty = Ty.INT })
        );
        let ty = Rc.new({
            .shape = :Fn {
                .args = (
                    let mut args = ArrayList.new();
                    &mut args |> ArrayList.push_back(Ty.INT);
                    &mut args |> ArrayList.push_back(Ty.INT);
                    args
                ),
                .result = Ty.INT,
            }
        });
        init_value(name, Rc.new({
            .shape = :NativeFn {
                .name = RcStr.allocate(name),
                .@"impl",
            },
            .ty,
        }))
    );

    const init_print = () => (
        let name = "print";
        let @"impl" = (args :: ArrayList.t[ValueWithSpan], .caller :: Span) -> Value => (
            if &args |> ArrayList.length != 1 then (
                panic("Expected 1 arg");
            );
            let s = args.[0] |> expect_value.with_span.into_string;
            lock_output(() => (
                locked_output.write(&s);
            ));
            Value.UNIT
        );
        let ty = Rc.new({
            .shape = :Fn {
                .args = (
                    let mut args = ArrayList.new();
                    &mut args |> ArrayList.push_back(Ty.STRING);
                    args
                ),
                .result = Ty.UNIT,
            }
        });
        init_value(name, Rc.new({
            .shape = :NativeFn {
                .name = RcStr.allocate(name),
                .@"impl",
            },
            .ty,
        }))
    );

    const init = () -> Map => (
        let mut map = OrdMap.new();
        with InitContext = &mut map;
        init_ty("Int", Ty.INT);
        init_ty("Bool", Ty.BOOL);
        init_value("true", Value.TRUE);
        init_value("false", Value.FALSE);
        init_ty("String", Ty.STRING);
        init_ty("Type", Ty.TYPE);
        init_ty("Ast", Ty.AST);
        init_op("+", (a, b) => a + b);
        init_op("-", (a, b) => a - b);
        init_op("*", (a, b) => a * b);
        init_op("/", (a, b) => a / b);
        init_print();
        map
    );

    const get = (map :: &Map, name :: &RcStr, ty :: Ty, .span :: Span) -> Value => (
        match map |> OrdMap.get(name) with (
            | :None => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Interpreter,
                    .span,
                    .message = () => (
                        locked_output.write_str("There is no native called ");
                        locked_output.write_escaped(name);
                    ),
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
            | :Some native => native^.get(ty)
        )
    );
);
