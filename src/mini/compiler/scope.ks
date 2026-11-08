use (import "./common.ks").*;

module:

const Scope = newtype {
    .parent :: Option.t[Rc.t[Scope]],
    .vars :: OrdMap.t[RcStr, Ir.Type],
    .found_in_parent :: (&RcStr, &Ir.Type) -> (),
};
const ScopeContext = @context type (Rc.t[Scope]);

const find_in_scope = (scope :: &Scope, name :: &RcStr) -> Option.t[Ir.Type] => (
    match &scope^.vars |> OrdMap.get(name) with (
        | :Some result => :Some result^
        | :None => match scope^.parent with (
            | :Some ref parent => (
                let result = find_in_scope(&parent^^, name);
                if result is :Some ref ty then (
                    scope^.found_in_parent(name, ty);
                );
                result
            )
            | :None => :None
        )
    )
);

const find_ident_ty = (name :: &RcStr, .span :: Span) -> Ir.Type => with_return (
    if find_in_scope(&(@current ScopeContext)^, name) is :Some result then (
        return result;
    );
    if (@current Compiler).get_toplevel_decl(name) is :Some decl then (
        match decl^ with (
            | :Fn fn_type => return { .shape = Rc.new(:Fn fn_type), .alias_name = :None }
            | :Const ref decl => return decl^.ty
            | _ => ()
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                locked_output.write(name);
                locked_output.write_str(" is a ");
                print_toplevel_kind(decl);
                locked_output.write_str(", can't be used as expr");
            ),
            .span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic);
    );
    let diagnostic = {
        .severity = :Error,
        .source = :Compiler,
        .message = () => (
            locked_output.write(name);
            locked_output.write_str(" not found in current scope");
        ),
        .span,
        .related = ArrayList.new(),
    };
    Diagnostic.report_and_unwind(diagnostic)
);
