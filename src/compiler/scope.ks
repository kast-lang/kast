use (import "./_common.ks").*;

module:

const Scope = (
    module:

    const Local = newtype (
        | :Binding Binding
        | :Const Place
    );

    const t = Rc.t[newtype {
        .parent :: Option.t[Scope.t],
        .locals :: OrdMap.t[RcStr, Local],
    }];

    const new = (.parent :: Option.t[Scope.t]) -> Scope.t => (
        Rc.new({
            .parent,
            .locals = OrdMap.new(),
        })
    );

    const Context = @context t;

    const lookup_in = (
        scope :: &Scope.t,
        name :: &RcStr,
        .span :: Span,
    ) -> &Local => (
        if &scope^^.locals |> OrdMap.get(name) is :Some local then (
            local
        ) else if scope^^.parent is :Some ref parent then (
            lookup_in(parent, name, .span)
        ) else (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span,
                .message = () => (
                    locked_output.write_str("Could not find ");
                    locked_output.write_escaped(name);
                    locked_output.write_str(" in scope");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        )
    );

    const lookup = (name :: &RcStr, .span :: Span) -> &Local => (
        lookup_in(&(@current Context), name, .span)
    );

    const inject_assignee_bindings = (assignee :: &Assignee) => (
        match assignee^^.shape with (
            | :Binding _ => ()
            | :Let ref pattern => inject_pattern_bindings(pattern)
        );
    );

    const inject_pattern_bindings = (pattern :: &Pattern) => (
        match pattern^^.shape with (
            | :Binding ref binding => inject_binding(binding)
        );
    );

    const inject_binding = (binding :: &Binding) => (
        &mut (@current Context)^.locals
            |> OrdMap.add(binding^^.name, :Binding binding^);
    );

    const inject_const = (name :: &RcStr, value :: Value) => (
        &mut (@current Context)^.locals
            |> OrdMap.add(name^, :Const Place.init(value));
    );
);
