use (import "./output.ks").*;
use (import "./span.ks").*;
use (import "./token.ks").*;
use (import "./syntax_rule.ks").*;
use (import "./tuple.ks").*;

module:

const Ast = (
    module:
    
    const t = newtype {
        .shape :: Shape.t,
        .span :: Span,
    };
    
    const Shape = (
        module:
        
        const t = newtype (
            | :Empty
            | :Token Token.t
            | :Rule {
                .rule :: SyntaxRule.t,
                .root :: Group,
            }
        );
    );
    
    const Group = newtype {
        .children :: Tuple.t[Child],
    };
    
    const Child = newtype (
        | :Value Ast.t
        | :Group Group
    );

    const print_child = (self :: &Child) => (
        match self^ with (
            | :Value ref ast => print(ast)
            | :Group ref group => print_group(group)
        );
    );

    const print_group = (self :: &Group) => (
        Tuple.print(
            &self^.children,
            .open = "{",
            .delimeter = ",",
            .before_field_name = ".",
            .after_field_name = " = ",
            .print_value = print_child,
            .close = "}",
        );
    );
    
    const print = (self :: &Ast.t) => (
        let output = @current Output;
        match self^.shape with (
            | :Empty => ansi.with_mode(
                :Dim,
                () => output.write("<empty>"),
            )
            | :Token token => Token.Shape.print(token.shape)
            | :Rule { .rule, .root = ref root } => (
                ansi.with_mode(
                    :Magenta,
                    () => output.write(rule.name),
                );
                output.write(" ");
                print_group(root);
            )
        );
    );
);
