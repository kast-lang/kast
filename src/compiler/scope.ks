use (import "./_common.ks").*;

module:

const Scope = (
    module:

    const Local = newtype (
        | :Binding Binding
        | :Const Place
    );

    const t = newtype {
        .parent :: Option.t[type (&Scope.t)],
        .locals :: OrdMap.t[String, Local],
    };

    const new = (.parent :: Option.t[type (&Scope.t)]) -> Scope.t => {
        .parent,
        .locals = OrdMap.new(),
    };

    const Context = @context t;

    const lookup_in = (
        scope :: &Scope.t,
        name :: String,
        .span :: Span,
    ) -> &Local => (
        if &scope^.locals |> OrdMap.get(name) is :Some local then (
            local
        ) else if scope^.parent is :Some parent then (
            lookup_in(parent, name, .span)
        ) else (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span,
                .message = () => (
                    let output = @current Output;
                    output.write("Could not find ");
                    output.write(String.escape(name));
                    output.write(" in scope");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        )
    );

    const lookup = (name :: String, .span :: Span) -> &Local => (
        lookup_in(&(@current Context), name, .span)
    );

    const inject_assignee_bindings = (assignee :: &Assignee) => (
        match assignee^.shape with (
            | :Binding _ => ()
            | :Let ref pattern => inject_pattern_bindings(pattern)
        );
    );

    const inject_pattern_bindings = (pattern :: &Pattern) => (
        match pattern^.shape with (
            | :Binding ref binding => inject_binding(binding)
        );
    );

    const inject_binding = (binding :: &Binding) => (
        &mut (@current Context).locals
            |> OrdMap.add(binding^.name, :Binding binding^);
    );

    const inject_const = (name :: String, value :: Value) => (
        &mut (@current Context).locals
            |> OrdMap.add(name, :Const Place.init(value));
    );
);
