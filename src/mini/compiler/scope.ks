use (import "./common.ks").*;

module:

const ScopeContext = @context CompilerScope.t;

const find_in_scope = (
    scope :: &CompilerScope.t,
    name :: String,
) -> Option.t[type (&CompilerScope.Local)] => (
    match &scope^.locals |> OrdMap.get(name) with (
        | :Some local => :Some local
        | :None => match scope^.parent with (
            | :Some ref parent => (
                let result = find_in_scope(parent, name);
                if result is :Some result then (
                    if result^ is :Binding ref binding then (
                        scope^.found_in_parent(name, binding^.ty);
                    );
                );
                result
            )
            | :None => :None
        )
    )
);

const find_ident_ty = (name :: String, .span :: Span) -> Ir.Type => with_return (
    if find_in_scope(&(@current ScopeContext), name) is :Some local then (
        return CompilerScope.Local.ty(local);
    );
    let diagnostic = {
        .severity = :Error,
        .source = :Compiler,
        .message = () => (
            let output = @current Output;
            output.write(name);
            output.write(" not found in current scope");
        ),
        .span,
        .related = ArrayList.new(),
    };
    Diagnostic.report_and_unwind(diagnostic)
);
