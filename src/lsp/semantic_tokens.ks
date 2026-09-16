use (import "./common.ks").*;
use (import "./state.ks").*;

module:

const semantic_tokens = (
    module:

    const TokenType = newtype (
        | :Type
        | :Class
        | :Enum
        | :Interface
        | :Struct
        | :TypeParameter
        | :Parameter
        | :Variable
        | :Property
        | :EnumMember
        | :Event
        | :Function
        | :Method
        | :Macro
        | :Keyword
        | :Modifier
        | :Comment
        | :String
        | :Number
        | :Regexp
        | :Operator
        | :Decorator
        | :Namespace
    );

    impl TokenType as module = (
        module:

        const index = (self :: TokenType) -> Int32 => match self with (
            | :Type => 0
            | :Class => 1
            | :Enum => 2
            | :Interface => 3
            | :Struct => 4
            | :TypeParameter => 5
            | :Parameter => 6
            | :Variable => 7
            | :Property => 8
            | :EnumMember => 9
            | :Event => 10
            | :Function => 11
            | :Method => 12
            | :Macro => 13
            | :Keyword => 14
            | :Modifier => 15
            | :Comment => 16
            | :String => 17
            | :Number => 18
            | :Regexp => 19
            | :Operator => 20
            | :Decorator => 21
            | :Namespace => 22
        );

        const to_string = (self :: TokenType) -> String => match self with (
            | :Type => "type"
            | :Class => "class"
            | :Enum => "enum"
            | :Interface => "interface"
            | :Struct => "struct"
            | :TypeParameter => "typeParameter"
            | :Parameter => "parameter"
            | :Variable => "variable"
            | :Property => "property"
            | :EnumMember => "enumMember"
            | :Event => "event"
            | :Function => "function"
            | :Method => "method"
            | :Macro => "macro"
            | :Keyword => "keyword"
            | :Modifier => "modifier"
            | :Comment => "comment"
            | :String => "string"
            | :Number => "number"
            | :Regexp => "regexp"
            | :Operator => "operator"
            | :Decorator => "decorator"
            | :Namespace => "namespace"
        );

        const variants = () -> std.iter.Iterable[TokenType] => {
            .iter = f => (
                f(:Type);
                f(:Class);
                f(:Enum);
                f(:Interface);
                f(:Struct);
                f(:TypeParameter);
                f(:Parameter);
                f(:Variable);
                f(:Property);
                f(:EnumMember);
                f(:Event);
                f(:Function);
                f(:Method);
                f(:Macro);
                f(:Keyword);
                f(:Modifier);
                f(:Comment);
                f(:String);
                f(:Number);
                f(:Regexp);
                f(:Operator);
                f(:Decorator);
                f(:Namespace);
            ),
        };
    );

    const TokenModifier = newtype (
        | :Declaration
        | :Definition
        | :Readonly
        | :Static
        | :Deprecated
        | :Abstract
        | :Async
        | :Modification
        | :Documentation
        | :DefaultLibrary
    );

    impl TokenModifier as module = (
        module:

        const to_string = (self :: TokenModifier) -> String => match self with (
            | :Declaration => "declaration"
            | :Definition => "definition"
            | :Readonly => "readonly"
            | :Static => "static"
            | :Deprecated => "deprecated"
            | :Abstract => "abstract"
            | :Async => "async"
            | :Modification => "modification"
            | :Documentation => "documentation"
            | :DefaultLibrary => "defaultLibrary"
        );

        const index = (self :: TokenModifier) -> Int32 => match self with (
            | :Declaration => 0
            | :Definition => 1
            | :Readonly => 2
            | :Static => 3
            | :Deprecated => 4
            | :Abstract => 5
            | :Async => 6
            | :Modification => 7
            | :Documentation => 8
            | :DefaultLibrary => 9
        );

        const variants = () -> std.iter.Iterable[TokenModifier] => {
            .iter = f => (
                f(:Declaration);
                f(:Definition);
                f(:Readonly);
                f(:Static);
                f(:Deprecated);
                f(:Abstract);
                f(:Async);
                f(:Modification);
                f(:Documentation);
                f(:DefaultLibrary);
            ),
        };
    );

    const legend = () -> Json.t => (
        let mut fields = OrdMap.new();
        let mut token_types = ArrayList.new();
        for token_type in TokenType.variants() do (
            &mut token_types
                |> ArrayList.push_back(
                    :String (token_type |> TokenType.to_string)
                );
        );
        &mut fields |> OrdMap.add("tokenTypes", :Array token_types);
        let mut token_modifiers = ArrayList.new();
        for token_modifier in TokenModifier.variants() do (
            &mut token_modifiers
                |> ArrayList.push_back(
                    :String (token_modifier |> TokenModifier.to_string)
                );
        );
        &mut fields |> OrdMap.add("tokenModifiers", :Array token_modifiers);
        :Object fields
    );

    const full = (state :: &mut State, request :: Json.t) -> Json.t => with_return (
        let :Object fields = request;
        let (&(:Object params)) = &fields |> OrdMap.get("params") |> Option.unwrap;
        let text_document = (
            let (&value) = &params |> OrdMap.get("textDocument") |> Option.unwrap;
            let :Object fields = value;
            let (&(:String uri)) = &fields |> OrdMap.get("uri") |> Option.unwrap;
            { .uri = parse(uri) }
        );

        let file_state = &state^.files
            |> OrdMap.get(Uri.to_string(text_document.uri))
            |> Option.unwrap_or_else(() => return :Null);
        let parsed = &file_state^.parsed
            |> Option.as_ref
            |> Option.unwrap_or_else(() => return :Null);

        let mut data :: ArrayList.t[Int32] = ArrayList.new();
        let mut prev_start = { .line = 0, .column = 0 };
        let add_token = (
            span :: Span,
            token_type :: TokenType,
            token_modifiers :: ArrayList.t[TokenModifier],
        ) => (
            let token_type = token_type |> TokenType.index;
            let token_modifiers = (
                let mut flags = 0;
                for mod in token_modifiers |> ArrayList.into_iter do (
                    let index = TokenModifier.index(mod);
                    flags = flags
                        |> std.op.bit_or(
                            std.op.bit_shift_left(1, index)
                        );
                );
                flags
            );
            for line in span.start.line..span.end.line + 1 do (
                let start_column = if line == span.start.line then (
                    span.start.column.string_encoding
                ) else (
                    0
                );
                let end_column = if line == span.end.line then (
                    span.end.column.string_encoding
                ) else (
                    let line_contents = &file_state^.lines |> ArrayList.at(line);
                    String.length(line_contents^)
                );
                let delta_line = line - prev_start.line;
                let delta_start = if delta_line == 0 then (
                    start_column - prev_start.column
                ) else (
                    start_column
                );
                let length = end_column - start_column;
                &mut data |> ArrayList.push_back(delta_line);
                &mut data |> ArrayList.push_back(delta_start);
                &mut data |> ArrayList.push_back(length);
                &mut data |> ArrayList.push_back(token_type);
                &mut data |> ArrayList.push_back(token_modifiers);
                prev_start = { .line, .column = start_column };
            );
        );
        Highlight.highlight(
            parsed,
            {
                .print = (span, token_type, _) => with_return (
                    let token_type = match token_type with (
                        | :Regular => return
                        | :StringContent => :String
                        | :StringDelimeter => :EnumMember
                        | :Keyword => :Keyword
                        | :Number => :Number
                        | :RawIdent => :Variable
                        | :Ident => return
                        | :Escape => :EnumMember
                        | :Comment => :Comment
                        | :Error => return
                        | :SyntaxCommand => :Macro
                    );
                    let token_modifiers = ArrayList.new();
                    add_token(span, token_type, token_modifiers);
                ),
                .move_to = (...) => (),
                .replace_ast = :None,
            },
        );

        :Object (
            let mut fields = OrdMap.new();
            let mut json_data = ArrayList.new();
            for x in data |> ArrayList.into_iter do (
                &mut json_data
                    |> ArrayList.push_back(
                        :Number (std.convert.int32_to_float64(x))
                    );
            );
            &mut fields |> OrdMap.add("data", :Array json_data);
            fields
        )
    );
);
