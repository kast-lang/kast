use (import "./common.ks").*;

use (import "./const.ks").*;
use (import "./scope.ks").*;
use (import "./fn.ks").*;
use (import "./template.ks").*;
use (import "./expr/parse.ks").*;
use (import "./type/check.ks").*;
use (import "./type/def.ks").*;
use (import "./type/inference.ks").*;
use (import "./type/info.ks").*;
use (import "./type/parse.ks").*;

const root_scope = @current_scope;

module:

const Compiler = (
    module:

    const Target = root_scope.CompilationTarget;

    const Processable = [T] newtype (
        | :Unprocessed
        | :Processing
        | :Processed T
    );

    const TopLevelItem = newtype {
        .def :: TopLevelItemDef,
        .declaration :: Processable[TopLevelDecl],
        .implementation :: Processable[TopLevelImpl],
    };

    const State = newtype {
        .ruleset :: SyntaxRuleset.t,
        .toplevel_items :: OrdMap.t[RcStr, Box.t[TopLevelItem]],
        .toplevel_items_unprocessed :: Queue.t[RcStr],
        .program :: Ir.Program,
    };

    const Context = @context type (&mut State);

    const init = () -> State => {
        .ruleset = ruleset(),
        .toplevel_items = OrdMap.new(),
        .toplevel_items_unprocessed = Queue.new(),
        .program = {
            .types = OrdMap.new(),
            .contexts = OrdMap.new(),
            .consts = OrdMap.new(),
            .consts_order = ArrayList.new(),
            .fns = OrdMap.new(),
        },
    };

    const get_toplevel_decl = (
        name :: &RcStr,
    ) -> Option.t[type (&TopLevelDecl)] => with_return (
        let ctx = @current Context;
        let item = &ctx^.toplevel_items
            |> OrdMap.get(name)
            |> Option.unwrap_or_else(() => return :None);
        match item^^.declaration with (
            | :Processed ref value => :Some value
            | _ => (
                process_toplevel_item(name);
                get_toplevel_decl(name)
            )
        )
    );

    const get_toplevel_impl = (
        name :: &RcStr,
    ) -> Option.t[type (&TopLevelImpl)] => with_return (
        let ctx = @current Context;
        let item = &ctx^.toplevel_items
            |> OrdMap.get(name)
            |> Option.unwrap_or_else(() => return :None);
        match item^^.implementation with (
            | :Processed ref value => :Some value
            | _ => (
                process_toplevel_item(name);
                get_toplevel_impl(name)
            )
        )
    );

    const parse_toplevel_item = (ast :: &Ast.t) => with_return (
        let ctx = @current Context;
        if ast^.shape^ is :Rule { .rule = ref rule, .root = ref root } then (
            let rule_name = &rule^^.name |> RcStr.as_str;
            if rule_name == "native const" then (
                let { name_ast, ty } = root
                    |> AstHelpers.expect_two_children(:Some { "name", "type" });
                let name = name_ast |> AstHelpers.expect_ident;
                add_toplevel_item({
                    .name,
                    .span = name_ast^.span,
                    .ast = ast^,
                    .native = true,
                    .setup_contexts = :None,
                });
                return;
            );
            if rule_name == "native const value" then (
                let { name_ast, value } = root
                    |> AstHelpers.expect_two_children(:Some { "name", "value" });
                let name = name_ast |> AstHelpers.expect_ident;
                add_toplevel_item({
                    .name,
                    .span = name_ast^.span,
                    .ast = value^,
                    .native = true,
                    .setup_contexts = :None,
                });
                return;
            );
        );
        let root = ast |> AstHelpers.expect_rule("const");
        let name_ast = &root^.children
            |> Tuple.get_named("name")
            |> Ast.unwrap_child_value;
        let value = &root^.children
            |> Tuple.get_named("value")
            |> Ast.unwrap_child_value;
        if name_ast^.shape^ is :Rule { .rule = ref rule, .root = ref root } then (
            if &rule^^.name |> RcStr.as_str == "type ascribe" then (
                let { name_ast, ty } = root
                    |> AstHelpers.expect_two_children(:Some { "expr", "type" });
                let name = name_ast |> AstHelpers.expect_ident;
                let toplevel_item = {
                    .name,
                    .span = name_ast^.span,
                    .ast = ast^,
                    .native = false,
                    .setup_contexts = :None,
                };
                add_toplevel_item(toplevel_item);
                return;
            );
        );
        let name = name_ast |> AstHelpers.expect_ident;
        let toplevel_item = {
            .name,
            .span = name_ast^.span,
            .ast = value^,
            .native = false,
            .setup_contexts = :None,
        };
        add_toplevel_item(toplevel_item);
    );

    const add_toplevel_item = (def :: TopLevelItemDef) => with_return (
        let { .name, .span, ... } = def;
        let ctx = @current Context;
        if &ctx^.toplevel_items |> OrdMap.get(&name) is :Some existing then (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    locked_output.write_str("Top level item ");
                    locked_output.write_escaped(&name);
                    locked_output.write_str(" has already been defined");
                ),
                .span,
                .related = (
                    let mut related = ArrayList.new();
                    let info = {
                        .span = existing^^.def.span,
                        .message = () => (
                            locked_output.write_str("Previous definition is here");
                        ),
                    };
                    &mut related |> ArrayList.push_back(info);
                    related
                ),
            };
            Diagnostic.report_and_unwind(diagnostic)
        );
        let item = {
            .def,
            .declaration = :Unprocessed,
            .implementation = :Unprocessed,
        };
        &mut ctx^.toplevel_items |> OrdMap.add(name, Box.new(item));
        &mut ctx^.toplevel_items_unprocessed |> Queue.push(name);
    );

    const process_toplevel_item_impl = (name :: &RcStr) => with_return (
        let ctx = @current Context;
        let item = &mut ctx^.toplevel_items
            |> OrdMap.get_mut(name)
            |> Option.unwrap;
        let item = &mut item^^;
        let native = item^.def.native;
        item^.declaration = :Processing;
        item^.implementation = :Processing;
        if item^.def.ast.shape^ is :Rule { .rule = ref rule, .root = ref root } then (
            let rule_name = &rule^^.name |> RcStr.as_str;
            if rule_name == "const" then (
                let decl = process_const_declaration(name, &item^.def.ast);
                item^.declaration = :Processed :Const decl;
                let value = process_const(name, &decl);
                item^.implementation = :Processed :Const value;
                return;
            );
            if rule_name == "native const" then (
                let decl = process_const_declaration(name, &item^.def.ast);
                item^.declaration = :Processed :Const decl;
                item^.implementation = :Processed :NativeConst;
                return;
            );
            if rule_name == "template" then (
                item^.declaration = :Processed :Template;
                let template = parse_template(name, root);
                item^.implementation = :Processed :Template template;
                return;
            );
            if rule_name == "type alias" then (
                item^.declaration = :Processed :Type { .is_alias = true };
                let def = {
                    .shape = type_def(name, :Alias, root),
                    .native,
                };
                item^.implementation = :Processed :Type def;
                return;
            );
            if rule_name == "enum" then (
                item^.declaration = :Processed :Type { .is_alias = false };
                let def = {
                    .shape = type_def(name, :Enum, root),
                    .native,
                };
                item^.implementation = :Processed :Type def;
                return;
            );
            if rule_name == "struct" then (
                item^.declaration = :Processed :Type { .is_alias = false };
                let def = {
                    .shape = type_def(name, :Struct, root),
                    .native,
                };
                item^.implementation = :Processed :Type def;
                return;
            );
            if rule_name == "union" then (
                item^.declaration = :Processed :Type { .is_alias = false };
                let def = {
                    .shape = type_def(name, :Union, root),
                    .native,
                };
                item^.implementation = :Processed :Type def;
                return;
            );
            if rule_name == "opaque_type" then (
                item^.declaration = :Processed :Type { .is_alias = false };
                let def = {
                    .shape = type_def(name, :Opaque, root),
                    .native,
                };
                item^.implementation = :Processed :Type def;
                return;
            );
            if rule_name == "fn" then (
                let fn_type = process_toplevel_fn_declaration(name, &item^.def.ast);
                item^.declaration = :Processed :Fn fn_type;
                let fn_def = process_toplevel_fn(name, &item^.def.ast);
                item^.implementation = :Processed :Fn fn_def;
                return;
            );
            if rule_name == "create_context_type" then (
                item^.declaration = :Processed :Context;
                let ty = root
                    |> AstHelpers.expect_single_child(:Some "type")
                    |> parse_type;
                item^.implementation = :Processed :Context ty;
                return;
            );
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                locked_output.write_str("Unexpected top level item");
            ),
            .span = item^.def.ast.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const process_toplevel_item = (name :: &RcStr) => with_return (
        let ctx = @current Context;
        let item = &ctx^.toplevel_items
            |> OrdMap.get(name)
            |> Option.unwrap;
        let item = &item^^;
        let loop_detected = () => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    locked_output.write_str("Detected a loop processing ");
                    locked_output.write(name);
                ),
                .span = item^.def.span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        );
        match item^.implementation with (
            | :Processed _ => return
            | :Processing => loop_detected()
            | :Unprocessed => ()
        );
        match item^.declaration with (
            | :Processed _ => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Internal,
                    .message = () => (
                        locked_output.write_str("Declaration is processed but defintion is not for ");
                        locked_output.write(name);
                    ),
                    .span = item^.def.span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
            | :Processing => loop_detected()
            | :Unprocessed => ()
        );
        match item^.def.setup_contexts with (
            | :Some f => (
                f(() => process_toplevel_item_impl(name));
            )
            | :None => (
                process_toplevel_item_impl(name);
            )
        );
        let item = &ctx^.toplevel_items
            |> OrdMap.get(name)
            |> Option.unwrap;
        let item = &item^^;
        match item^.implementation with (
            | :Processed ref item => match item^ with (
                | :Const expr => (
                    &mut ctx^.program.consts |> OrdMap.add(name^, expr);
                    &mut ctx^.program.consts_order |> ArrayList.push_back(name^);
                )
                | :NativeConst => ()
                | :Type def => (
                    &mut ctx^.program.types |> OrdMap.add(name^, def);
                )
                | :Fn def => (
                    &mut ctx^.program.fns |> OrdMap.add(name^, def);
                )
                | :Context ty => (
                    &mut ctx^.program.contexts |> OrdMap.add(name^, ty);
                )
                | :Template _ => ()
            )
            | _ => panic("unreachable")
        )
    );

    const add_source_ast = (state :: &mut State, ast :: &Ast.t) => (
        with Context = state;
        for item in Ast.iter_list(
            ast,
            .binary_rule_name = "then",
            .trailing_or_leading_rule_name = :Some "stmt",
        ) do (
            parse_toplevel_item(item);
        );
    );

    const ruleset_path = () -> &str => (
        "kast:///mini/syntax.ks"
    );

    const ruleset = () -> SyntaxRuleset.t => (
        let mut lexer = Lexer.new(Source.read(&SourcePath.parse(ruleset_path())));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        SyntaxParser.parse_syntax_ruleset(&mut token_stream)
    );

    const add_source = (state :: &mut State, source :: Source) => (
        let mut lexer = Lexer.new(source);
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let parsed = Parser.parse(
            .ruleset = &state^.ruleset,
            .entire_source_span = Source.entire_span(&source),
            .path = source.path,
            .token_stream = &mut token_stream,
        );
        add_source_ast(state, &parsed.ast)
    );

    const compile = (mut state :: State) -> Ir.Program => (
        with Context = &mut state;
        with root_scope.Compiler = {
            .program = &mut state.program,
            .parse_expr,
            .parse_type,
            .find_ident_ty,
            .lookup_type,
            .add_toplevel_item,
            .get_toplevel_decl,
            .get_toplevel_impl,
        };
        # Empty scope is need for initializing consts (they do name lookups)
        let mut scope = Rc.new({
            .parent = :None,
            .vars = OrdMap.new(),
            .found_in_parent = (...) => (),
        });
        with ScopeContext = scope;
        # Outside of template instantiations
        let template_args_ctx = {
            .args = Rc.new(ArrayList.new()),
            .by_name = OrdMap.new(),
        };
        with TemplateArgsContext = &template_args_ctx;
        while &mut state.toplevel_items_unprocessed |> Queue.pop is :Some name do (
            process_toplevel_item(&name);
        );
        state.program
    );
);
