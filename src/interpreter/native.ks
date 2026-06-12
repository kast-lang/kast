use (import "./_common.ks").*;

module:

const Native = (
    module:

    const t = newtype {
        .get :: Ty -> Value,
    };

    const Map = OrdMap.t[String, t];

    const InitContext = @context type (&mut Map);

    const init_value = (name :: String, value :: Value) => (
        (@current InitContext) |> OrdMap.add(name, { .get = _ => value });
    );

    const init_ty = (name :: String, ty :: Ty) => (
        init_value("type " + name, { .shape = :Type ty, .ty = Ty.TYPE });
    );

    const init_op = (name :: String, f :: (Int, Int) -> Int) => (
        let @"impl" = (args :: ArrayList.t[Value], .caller :: Span) -> Value => (
            if &args |> ArrayList.length != 2 then (
                panic("Expected 2 args");
            );
            let a = (&args |> ArrayList.at(0))^ |> expect_value.expect_int(.span = caller);
            let b = (&args |> ArrayList.at(1))^ |> expect_value.expect_int(.span = caller);
            let result = f(a, b);
            { .shape = :Int result, .ty = Ty.INT }
        );
        let ty = {
            .shape = :Fn {
                .args = (
                    let mut args = ArrayList.new();
                    &mut args |> ArrayList.push_back(Ty.INT);
                    &mut args |> ArrayList.push_back(Ty.INT);
                    args
                ),
                .result = Ty.INT,
            }
        };
        init_value(name, { .shape = :NativeFn { .name, .@"impl" }, .ty })
    );

    const init_print = () => (
        let name = "print";
        let @"impl" = (args :: ArrayList.t[Value], .caller :: Span) -> Value => (
            if &args |> ArrayList.length != 1 then (
                panic("Expected 1 arg");
            );
            let s = (&args |> ArrayList.at(0))^ |> expect_value.expect_string(.span = caller);
            (@current Output).write(s);
            Value.UNIT
        );
        let ty = {
            .shape = :Fn {
                .args = (
                    let mut args = ArrayList.new();
                    &mut args |> ArrayList.push_back(Ty.STRING);
                    args
                ),
                .result = Ty.UNIT,
            }
        };
        init_value(name, { .shape = :NativeFn { .name, .@"impl" }, .ty })
    );

    const init = () -> Map => (
        let mut map = OrdMap.new();
        with InitContext = &mut map;
        init_ty("Int", Ty.INT);
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

    const get = (map :: &Map, name :: String, ty :: Ty, .span :: Span) -> Value => (
        match map |> OrdMap.get(name) with (
            | :None => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Interpreter,
                    .span,
                    .message = () => (
                        let output = @current Output;
                        output.write("There is no native called ");
                        output.write(String.escape(name));
                    ),
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
            | :Some native => native^.get(ty)
        )
    );
);
