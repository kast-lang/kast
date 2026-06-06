use (import "./_common.ks").*;
use (import "./scope.ks").*;
use (import "./native.ks").*;

const super = @current_scope;

module:

const Interpreter = (
    module:

    const Scope = super.Scope;
    const expect_value = super.expect_value;

    const State = newtype {
        .natives :: Native.Map,
    };

    const StateContext = @context State;

    const init = () -> {
        .state :: State,
        .scope :: Scope.t,
    } => {
        .state = {
            .natives = Native.init(),
        },
        .scope = Scope.new(.parent = :None),
    };

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

    const claim = (place :: Place, .span :: Span) -> Value => (
        # TODO
        read_place(&place, .span)
    );

    const claim_move = (.from :: Place, .into :: &mut Place, .span :: Span) => (
        assign_place(into, claim(from, .span));
    );

    const assign_place = (place :: &mut Place, value :: Value) => (
        # TODO drop existing value
        place^.state = :Occupied value;
    );

    const eval_place = (expr :: &PlaceExpr) -> Place => (
        let span = expr^.span;
        let result = match expr^.shape with (
            | :Temp ref expr => (
                Place.init(eval(expr))
            )
            | :Binding ref binding => (
                Scope.get(binding, .span)
            )
        );
        result
    );

    const PatternMatchContext = @context newtype {
        .matched_binding :: (&Binding, Place) -> (),
    };

    const pattern_match = (pattern :: &Pattern, value :: Place) => (
        match pattern^.shape with (
            | :Binding ref binding => (
                (@current PatternMatchContext).matched_binding(binding, value);
            )
        );
    );

    const pattern_match_and_inject_bindings = (pattern :: &Pattern, value :: Place) => (
        let span = pattern^.span;
        with PatternMatchContext = {
            .matched_binding = (binding, mut value) => (
                let mut new_value = Place.uninitialized(.ty = value.ty);
                claim_move(.from = value, .into = &mut new_value, .span);
                Scope.inject_binding(binding, new_value);
            )
        };
        pattern_match(pattern, value);
    );

    const assign = (assignee :: &Assignee, mut value :: Place) => (
        let span = assignee^.span;
        match assignee^.shape with (
            | :Binding ref binding => (
                let mut existing_place = Scope.get(binding, .span);
                claim_move(.from = value, .into = &mut existing_place, .span);
            )
            | :Let ref pattern => pattern_match_and_inject_bindings(pattern, value)
        );
    );

    const apply = (
        f :: Value,
        args :: ArrayList.t[Value],
        .caller :: Span,
    ) -> Value => (
        match f.shape with (
            | :NativeFn native => (
                native.@"impl"(args, .caller)
            )
            | _ => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Interpreter,
                    .span = caller,
                    .message = () => (
                        let output = @current Output;
                        output.write("Expected a function, got ");
                        Value.print(&f);
                    ),
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        )
    );

    const eval = (expr :: &Expr) -> Value => (
        let span = expr^.span;
        let result = match expr^.shape with (
            | :Unit => Value.UNIT
            | :Read ref place_expr => (
                let place = eval_place(place_expr);
                read_place(&place, .span)
            )
            | :Const ref place => (
                read_place(place, .span)
            )
            | :Assign { .assignee = ref assignee, .value = ref value } => (
                let value = eval_place(value);
                assign(assignee, value);
                Value.UNIT
            )
            | :Type ref type_expr => {
                .shape = :Type eval_type(type_expr),
                .ty = Ty.TYPE,
            }
            | :Native s => (
                Native.get(&(@current StateContext).natives, s, expr^.ty, .span)
            )
            | :Stmt ref expr => (
                let ignored = eval(expr);
                Value.UNIT
            )
            | :Then ref list => (
                let mut result = Value.UNIT;
                for expr in list |> ArrayList.iter do (
                    result = eval(expr);
                );
                result
            )
            | :Apply { .f = ref f_expr, .args = ref args_exprs } => (
                let f = eval(f_expr);
                let mut args = ArrayList.new();
                for arg in args_exprs |> ArrayList.iter do (
                    &mut args |> ArrayList.push_back(eval(arg));
                );
                apply(f, args, .caller = span)
            )
            | :Scope ref body => (
                with Scope.Context = Scope.new(.parent = :Some &(@current Scope.Context));
                eval(body)
            )
        );
        result
    );

    const eval_type = (expr :: &TyExpr) -> Ty => (
        let span = expr^.span;
        let result = match expr^.shape with (
            | :Const ty => ty
            | :Expr ref expr => (
                eval(expr) |> expect_value.expect_type(.span)
            )
            | :Fn { .args = ref args_exprs, .result = ref result_expr } => (
                let mut args = ArrayList.new();
                for arg_expr in args_exprs |> ArrayList.iter do (
                    &mut args |> ArrayList.push_back(eval_type(arg_expr));
                );
                let result = eval_type(result_expr);
                { .shape = :Fn { .args, .result } }
            )
        );
        result
    );
);
