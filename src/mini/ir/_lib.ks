use (import "../../diagnostic.ks").*;
use (import "../../output.ks").*;
use (import "../../span.ks").*;

module:

const Ir = (
    module:

    const Types = import "./types.ks";
    use Types.*;
    use (import "./print.ks").*;

    impl Value as module = (
        module:

        const ty = (self :: &Value) -> Type => (
            self^.ty
        );

        const new_type = (ty :: Type) -> Value => {
            .shape = :Type ty,
            .ty = {
                .shape = :Type,
                .alias_name = :None,
            },
        };

        const expect_template = (self :: Value, .span :: Span) -> Template => (
            match self.shape with (
                | :Template t => t
                | _ => (
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .message = () => (
                            let output = @current Output;
                            output.write("Expected a template, got ");
                            Print.type_name(&self.ty);
                        ),
                        .span,
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                )
            )
        );

        const expect_type = (self :: Value, .span :: Span) -> Type => (
            match self.shape with (
                | :Type t => t
                | _ => (
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .message = () => (
                            let output = @current Output;
                            output.write("Expected a type, got ");
                            Print.type_name(&self.ty);
                        ),
                        .span,
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                )
            )
        );

        const expect_context_type = (self :: Value, .span :: Span) -> ContextType => (
            match self.shape with (
                | :ContextType t => t
                | _ => (
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .message = () => (
                            let output = @current Output;
                            output.write("Expected a context type, got ");
                            Print.type_name(&self.ty);
                        ),
                        .span,
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                )
            )
        );
    );

    impl CompilerScope.Local as module = (
        module:

        const ty = (self :: &CompilerScope.Local) -> Type => match self^ with (
            | :Binding ref binding => binding^.ty
            | :Const ref value => Value.ty(value)
        );
    );
);
