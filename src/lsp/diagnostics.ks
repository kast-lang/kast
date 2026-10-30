use (import "./common.ks").*;
use (import "./state.ks").*;

module:

const diagnostics = (
    module:

    const document = (state :: &State, request :: Json.t) -> Json.t => with_return (
        let :Object ref fields = request;
        let (&(:Object ref params)) = fields
            |> OrdMap.get(&RcStr.allocate("params"))
            |> Option.unwrap;
        let text_document = (
            let value = params
                |> OrdMap.get(&RcStr.allocate("textDocument"))
                |> Option.unwrap;
            let :Object ref fields = value^;
            let (&(:String ref uri)) = fields
                |> OrdMap.get(&RcStr.allocate("uri"))
                |> Option.unwrap;
            { .uri = parse(uri |> RcStr.as_str) }
        );
        let file_state = get_latest_file_state(state, &text_document.uri)
            |> Option.unwrap_or_else(() => (
                # Technically null is not allowed response
                return :Null
            ));

        let mut diagnostics = ArrayList.new();
        for diagnostic in &file_state^.diagnostics |> ArrayList.iter do (
            let diagnostic = :Object (
                let mut fields = OrdMap.new();
                let range = span_to_lsp_range(diagnostic^.span);
                &mut fields |> OrdMap.add(RcStr.allocate("range"), range);
                let severity = match diagnostic^.severity with (
                    | :Error => 1
                    | :Warning => 2
                    | :Info => 3
                    | :Hint => 4
                );
                &mut fields |> OrdMap.add(RcStr.allocate("severity"), :Number severity);
                unwindable source (
                    let source = match diagnostic^.source with (
                        | :Lexer => "lexer"
                        | :Parser => "parser"
                        | :Internal => "internal"
                        | :Compiler => "compiler"
                        | :Interpreter => "interpreter"
                        | :Other => unwind source ()
                    );
                    &mut fields
                        |> OrdMap.add(
                            RcStr.allocate("source"),
                            :String RcStr.allocate(source),
                        );
                );
                let message = RcStr.from_String(output_to_string(diagnostic^.message));
                &mut fields |> OrdMap.add(RcStr.allocate("message"), :String message);
                # Can add tags : unnecessary/deprecated
                let mut related = ArrayList.new();
                for info in &diagnostic^.related |> ArrayList.iter do (
                    let mut fields = OrdMap.new();
                    &mut fields |> OrdMap.add(RcStr.allocate("location"), span_to_lsp_location(info^.span));
                    let message = RcStr.from_String(output_to_string(info^.message));
                    &mut fields |> OrdMap.add(RcStr.allocate("message"), :String message);
                    &mut related |> ArrayList.push_back(:Object fields);
                );
                &mut fields |> OrdMap.add(RcStr.allocate("relatedInformation"), :Array related);
                fields
            );
            &mut diagnostics |> ArrayList.push_back(diagnostic);
        );
        :Object (
            let mut fields = OrdMap.new();
            &mut fields |> OrdMap.add(RcStr.allocate("kind"), :String RcStr.allocate("full"));
            &mut fields |> OrdMap.add(RcStr.allocate("items"), :Array diagnostics);
            fields
        )
    );

    const workspace = (state :: &State, request :: Json.t) -> Json.t => (
        :Object (
            let mut fields = OrdMap.new();
            &mut fields |> OrdMap.add(RcStr.allocate("items"), :Array ArrayList.new());
            fields
        )
    );
);
