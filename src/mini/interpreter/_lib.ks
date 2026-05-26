use (import "../../ast.ks").*;
use (import "../../span.ks").*;
use (import "../../output.ks").*;
use (import "../../diagnostic.ks").*;
use (import "../ir/_lib.ks").*;

module:

const Interpreter = (
    module:

    const State = newtype {  };

    const Context = @context State;

    const init = () -> State => {  };

    const Place = newtype {
        .state :: PlaceState,
    };

    const PlaceState = newtype (
        | :Uninitialized
        | :Occupied Ir.Value
        | :MovedOut
    );

    const claim = (place :: Place, .span :: Span) -> Ir.Value => (
        match place.state with (
            | :Occupied value => value
            | :Uninitialized => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Interpreter,
                    .message = () => (
                        let output = @current Output;
                        output.write("Place was never initialized");
                    ),
                    .span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
            | :MovedOut => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Interpreter,
                    .message = () => (
                        let output = @current Output;
                        output.write("Place was already moved out of");
                    ),
                    .span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        )
    );

    const eval_place = (expr :: &Ir.PlaceExpr) -> Place => (
        let span = expr^.span;
        let result = match expr^.shape with (
            | :Ident name => (
                panic("TODO interpreter lookup")
            )
        );
        result
    );

    const eval = (expr :: &Ir.Expr) -> Ir.Value => (
        let span = expr^.span;
        let result = match expr^.shape with (
            | :Claim ref place => (
                let place = eval_place(place);
                claim(place, .span)
            )
        );
        result
    );
);
