use (import "./_common.ks").*;

module:

const Scope = (
    module:

    const LocalShape = newtype (
        # TODO maybe remove this since its duplicate of Local.binding
        | :Binding Binding
        | :Const Place
    );

    const Local = newtype {
        .binding :: Binding,
        .shape :: LocalShape,
    };

    const ForbiddenLocal = newtype {
        .cause_span :: Span,
    };

    const t = Rc.t[newtype {
        .parent :: Option.t[Scope.t],
        .locals :: OrdMap.t[RcStr, Local],
        .forbidden_locals :: OrdMap.t[RcStr, ForbiddenLocal],
        .recursive :: Bool,
        .closed :: Bool,
    }];

    const new = (
        .parent :: Option.t[Scope.t],
        .recursive :: Bool,
    ) -> Scope.t => (
        Rc.new({
            .parent,
            .locals = OrdMap.new(),
            .forbidden_locals = OrdMap.new(),
            .recursive,
            .closed = false,
        })
    );

    const close = (self :: Scope.t) => (
        self^.closed = true;
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
            if scope^^.recursive and not scope^^.closed then (
                &mut scope^^.forbidden_locals
                    |> OrdMap.add(name^, {
                        .cause_span = span,
                    });
            );
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
        add_local(binding^^.name, {
            .shape = :Binding binding^,
            .binding = binding^,
        });
    );

    const inject_const = (binding :: &Binding, value :: Value) => (
        add_local(binding^^.name, {
            .shape = :Const Place.init(value),
            .binding = binding^,
        });
    );

    const add_local = (name :: RcStr, local :: Local) => (
        let ref mut scope = @current Context;
        if &scope^^.locals |> OrdMap.get(&name) is :Some existing then (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = local.binding^.span,
                .message = () => (
                    locked_output.write_str("Duplicate local ");
                    locked_output.write_escaped(&name);
                ),
                .related = (
                    let mut list = ArrayList.new();
                    &mut list |> ArrayList.push_back({
                        .message = () => (
                            locked_output.write_str("Previously defined here");
                        ),
                        .span = existing^.binding^.span,
                    });
                    list
                ),
            };
            Diagnostic.report_and_unwind(diagnostic)
        );
        if &scope^^.forbidden_locals |> OrdMap.get(&name) is :Some forbidden then (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = local.binding^.span,
                .message = () => (
                    locked_output.write_str("Compiler assumed previously that ");
                    locked_output.write_escaped(&name);
                    locked_output.write_str(" is not present in this scope");
                ),
                .related = (
                    let mut list = ArrayList.new();
                    &mut list |> ArrayList.push_back({
                        .message = () => (
                            locked_output.write_str("When looking up the symbol here");
                        ),
                        .span = forbidden^.cause_span,
                    });
                    list
                ),
            };
            Diagnostic.report_and_unwind(diagnostic)
        );
        &mut scope^^.locals
            |> OrdMap.add(name, local);
    );
);
