use (import "./_common.ks").*;

module:

const Scope = (
    module:

    const t = newtype {
        .parent :: Option.t[type (&Scope.t)],
        .locals :: OrdMap.t[Id, Place],
    };

    const new = (.parent :: Option.t[type (&Scope.t)]) -> Scope.t => {
        .parent,
        .locals = OrdMap.new_with_compare(Id.compare),
    };

    const Context = @context t;

    const get_in = (
        scope :: &Scope.t,
        binding :: &Binding,
        .span :: Span,
    ) -> Place => (
        if &scope^.locals |> OrdMap.get(binding^.id) is :Some &place then (
            place
        ) else if scope^.parent is :Some parent then (
            get_in(parent, binding, .span)
        ) else (
            let diagnostic = {
                .severity = :Error,
                .source = :Interpreter,
                .span,
                .message = () => (
                    let output = @current Output;
                    output.write("Could not find variable in the current scope");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        )
    );

    const get = (binding :: &Binding, .span :: Span) -> Place => (
        get_in(&(@current Context), binding, .span)
    );

    const inject_binding = (binding :: &Binding, place :: Place) => (
        &mut (@current Context).locals
            |> OrdMap.add(binding^.id, place);
    )
);
