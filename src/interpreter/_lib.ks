use (import "./_common.ks").*;

module:

const Interpreter = (
    module:

    const read_place = (place :: &Place, .span :: Span) -> Value => (
        let error = (s :: String) => (
            let diagnostic = {
                .severity = :Error,
                .source = :Interpreter,
                .span,
                .message = () => (
                    let output = @current Output;
                    output.write(s);
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        );
        match place^.state with (
            | :Uninitialized => error("Place is uninitialized")
            | :Occupied value => value
            | :MovedOut => error("Place was moved out of")
        )
    );

    const eval = (expr :: &Expr) -> Value => (
        let span = expr^.span;
        let result = match expr^.shape with (
            | :Unit => Value.UNIT
            | :Const ref place => (
                read_place(place, .span)
            )
        );
        result
    );

    const eval_type = (expr :: &TyExpr) -> Ty => (
        let span = expr^.span;
        let result = match expr^.shape with (
            | :Expr ref expr => (
                eval(expr) |> expect_value_type(.span)
            )
        );
        result
    );
);
