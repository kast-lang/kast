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
    
    const unwrap_child_value = (self :: Child) => match self with (
        | :Value value => value
        | :Group _ => panic("expected value ast child, got group")
    );
    
    const print_child = (self :: &Child) => (
        match self^ with (
            | :Value ref ast => print(ast)
            | :Group ref group => print_group(group)
        );
    );
    
    const print_group = (self :: &Group) => (
        print_children(&self^.children);
    );
    
    const print_children = (children :: &Tuple.t[Child]) => (
        Tuple.print(
            children,
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
    
    const iter_list = (
        ast :: Ast.t,
        .binary_rule_name :: String,
        .trailing_or_leading_rule_name :: Option.t[String],
    ) -> std.iter.Iterable[Ast.t] => {
        .iter = consumer => (
            let recurse = ast => (
                Ast.iter_list(
                    ast,
                    .binary_rule_name,
                    .trailing_or_leading_rule_name,
                ).iter(consumer);
            );
            match ast.shape with (
                | :Empty => consumer(ast)
                | :Token token => consumer(ast)
                | :Rule { .rule, .root = { .children, ... } } => with_return (
                    if rule.name == binary_rule_name then (
                        let { lhs, rhs } = Tuple.unwrap_unnamed_2(children);
                        recurse(lhs |> Ast.unwrap_child_value);
                        recurse(rhs |> Ast.unwrap_child_value);
                        return;
                    );
                    if trailing_or_leading_rule_name is :Some name then (
                        if rule.name == name then (
                            let inner = Tuple.unwrap_unnamed_1(children);
                            recurse(inner |> Ast.unwrap_child_value);
                            return;
                        );
                    );
                    consumer(ast);
                )
            );
        ),
    };
);
