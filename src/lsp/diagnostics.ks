use (import "./common.ks").*;
use (import "./state.ks").*;

module:

const diagnostics = (
    module:

    const document = (state :: &mut State, request :: Json.t) -> Json.t => with_return (
        let :Object fields = request;
        let &(:Object params) = &fields |> OrdMap.get("params") |> Option.unwrap;
        let text_document = (
            let &value = &params |> OrdMap.get("textDocument") |> Option.unwrap;
            let :Object fields = value;
            let &(:String uri) = &fields |> OrdMap.get("uri") |> Option.unwrap;
            { .uri = parse(uri) }
        );
        let file_state = &state^.files
            |> OrdMap.get(Uri.to_string(text_document.uri))
            |> Option.unwrap_or_else(
                () => (
                    # Technically null is not allowed response
                    return :Null
                )
            );

        let mut diagnostics = ArrayList.new();
        for diagnostic in &file_state^.diagnostics |> ArrayList.iter do (
            let diagnostic = :Object (
                let mut fields = OrdMap.new();
                let range = span_to_lsp_range(diagnostic^.span);
                &mut fields |> OrdMap.add("range", range);
                let severity = match diagnostic^.severity with (
                    | :Error => 1
                    | :Warning => 2
                    | :Info => 3
                    | :Hint => 4
                );
                &mut fields |> OrdMap.add("severity", :Number severity);
                unwindable source (
                    let source = match diagnostic^.source with (
                        | :Lexer => "lexer"
                        | :Parser => "parser"
                        | :Internal => "internal"
                        | :Compiler => "compiler"
                        | :Interpreter => "interpreter"
                        | :Other => unwind source ()
                    );
                    &mut fields |> OrdMap.add("source", :String source);
                );
                let message = output_to_string(diagnostic^.message);
                &mut fields |> OrdMap.add("message", :String message);
                # Can add tags : unnecessary/deprecated
                let mut related = ArrayList.new();
                for info in &diagnostic^.related |> ArrayList.iter do (
                    let mut fields = OrdMap.new();
                    &mut fields |> OrdMap.add("location", span_to_lsp_location(info^.span));
                    let message = output_to_string(info^.message);
                    &mut fields |> OrdMap.add("message", :String message);
                    &mut related |> ArrayList.push_back(:Object fields);
                );
                &mut fields |> OrdMap.add("relatedInformation", :Array related);
                fields
            );
            &mut diagnostics |> ArrayList.push_back(diagnostic);
        );
        :Object (
            let mut fields = OrdMap.new();
            &mut fields |> OrdMap.add("kind", :String "full");
            &mut fields |> OrdMap.add("items", :Array diagnostics);
            fields
        )
    );

    const workspace = (state :: &mut State, request :: Json.t) -> Json.t => (
        :Object (
            let mut fields = OrdMap.new();
            &mut fields |> OrdMap.add("items", :Array ArrayList.new());
            fields
        )
    );
);
