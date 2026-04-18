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
        .target :: CompilationTarget,
        .toplevel_items :: OrdMap.t[String, TopLevelItem],
        .toplevel_items_unprocessed :: Queue.t[String],
        .program :: Ir.Program,
    };

    const Context = @context State;

    const init = (target :: CompilationTarget) -> State => {
        .target,
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
        name :: String,
    ) -> Option.t[TopLevelDecl] => with_return (
        let ctx = @current Context;
        let item = &ctx.toplevel_items
            |> OrdMap.get(name)
            |> Option.unwrap_or_else(() => return :None);
        match item^.declaration with (
            | :Processed value => :Some value
            | _ => (
                process_toplevel_item(name);
                get_toplevel_decl(name)
            )
        )
    );

    const get_toplevel_impl = (
        name :: String,
    ) -> Option.t[TopLevelImpl] => with_return (
        let ctx = @current Context;
        let item = &ctx.toplevel_items
            |> OrdMap.get(name)
            |> Option.unwrap_or_else(() => return :None);
        match item^.implementation with (
            | :Processed value => :Some value
            | _ => (
                process_toplevel_item(name);
                get_toplevel_impl(name)
            )
        )
    );

    const parse_toplevel_item = (ast :: Ast.t) => with_return (
        let mut ctx = @current Context;
        if ast.shape is :Rule { .rule, .root } then (
            if rule.name == "native const" then (
                let { name_ast, ty } = root
                    |> AstHelpers.expect_two_children(:Some { "name", "type" });
                let name = name_ast |> AstHelpers.expect_ident;
                let toplevel_item = {
                    .name,
                    .span = name_ast.span,
                    .ast,
                    .native = true,
                    .setup_contexts = :None,
                };
                add_toplevel_item(toplevel_item);
                return;
            );
            if rule.name == "native const value" then (
                let { name_ast, value } = root
                    |> AstHelpers.expect_two_children(:Some { "name", "value" });
                let name = name_ast |> AstHelpers.expect_ident;
                let toplevel_item = {
                    .name,
                    .span = name_ast.span,
                    .ast = value,
                    .native = true,
                    .setup_contexts = :None,
                };
                add_toplevel_item(toplevel_item);
                return;
            );
        );
        let root = ast |> AstHelpers.expect_rule("const");
        let name_ast = (
            &root.children
                |> Tuple.get_named("name")
        )^
            |> Ast.unwrap_child_value;
        let value = (
            &root.children
                |> Tuple.get_named("value")
        )^
            |> Ast.unwrap_child_value;
        if name_ast.shape is :Rule { .rule, .root } then (
            if rule.name == "type ascribe" then (
                let { name_ast, ty } = root
                    |> AstHelpers.expect_two_children(:Some { "expr", "type" });
                let name = name_ast |> AstHelpers.expect_ident;
                let toplevel_item = {
                    .name,
                    .span = name_ast.span,
                    .ast,
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
            .span = name_ast.span,
            .ast = value,
            .native = false,
            .setup_contexts = :None,
        };
        add_toplevel_item(toplevel_item);
    );

    const add_toplevel_item = (def :: TopLevelItemDef) => with_return (
        let { .name, .span, ... } = def;
        let mut ctx = @current Context;
        if &ctx.toplevel_items |> OrdMap.get(name) is :Some existing then (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    let output = @current Output;
                    output.write("Top level item ");
                    output.write(String.escape(name));
                    output.write(" has already been defined");
                ),
                .span,
                .related = (
                    let mut related = ArrayList.new();
                    let info = {
                        .span = existing^.def.span,
                        .message = () => (
                            let output = @current Output;
                            output.write("Previous definition is here");
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
        &mut ctx.toplevel_items |> OrdMap.add(name, item);
        &mut ctx.toplevel_items_unprocessed |> Queue.push(name);
    );

    const process_toplevel_item_impl = (name :: String) => with_return (
        let mut ctx = @current Context;
        let item = () => &mut ctx.toplevel_items
            |> OrdMap.get_mut(name)
            |> Option.unwrap;
        let native = item()^.def.native;
        item()^.declaration = :Processing;
        item()^.implementation = :Processing;
        if item()^.def.ast.shape is :Rule { .rule, .root } then (
            if rule.name == "const" then (
                let decl = process_const_declaration(name, item()^.def.ast);
                item()^.declaration = :Processed :Const decl;
                let value = process_const(name, decl);
                item()^.implementation = :Processed :Const value;
                return;
            );
            if rule.name == "native const" then (
                let decl = process_const_declaration(name, item()^.def.ast);
                item()^.declaration = :Processed :Const decl;
                item()^.implementation = :Processed :NativeConst;
                return;
            );
            if rule.name == "template" then (
                item()^.declaration = :Processed :Template;
                let template = parse_template(name, root);
                item()^.implementation = :Processed :Template template;
                return;
            );
            if rule.name == "type alias" then (
                item()^.declaration = :Processed :Type;
                let def = {
                    .shape = type_def(name, :Alias, root),
                    .native,
                };
                item()^.implementation = :Processed :Type def;
                return;
            );
            if rule.name == "enum" then (
                item()^.declaration = :Processed :Type;
                let def = {
                    .shape = type_def(name, :Enum, root),
                    .native,
                };
                item()^.implementation = :Processed :Type def;
                return;
            );
            if rule.name == "struct" then (
                item()^.declaration = :Processed :Type;
                let def = {
                    .shape = type_def(name, :Struct, root),
                    .native,
                };
                item()^.implementation = :Processed :Type def;
                return;
            );
            if rule.name == "union" then (
                item()^.declaration = :Processed :Type;
                let def = {
                    .shape = type_def(name, :Union, root),
                    .native,
                };
                item()^.implementation = :Processed :Type def;
                return;
            );
            if rule.name == "opaque_type" then (
                item()^.declaration = :Processed :Type;
                let def = {
                    .shape = type_def(name, :Opaque, root),
                    .native,
                };
                item()^.implementation = :Processed :Type def;
                return;
            );
            if rule.name == "fn" then (
                let fn_type = process_toplevel_fn_declaration(name, item()^.def.ast);
                item()^.declaration = :Processed :Fn fn_type;
                let fn_def = process_toplevel_fn(name, item()^.def.ast);
                item()^.implementation = :Processed :Fn fn_def;
                return;
            );
            if rule.name == "create_context_type" then (
                item()^.declaration = :Processed :Context;
                let ty = root
                    |> AstHelpers.expect_single_child(:Some "type")
                    |> parse_type;
                item()^.implementation = :Processed :Context ty;
                return;
            );
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                (@current Output).write("Unexpected top level item");
            ),
            .span = item()^.def.ast.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const process_toplevel_item = (name :: String) => with_return (
        let mut ctx = @current Context;
        let item = &ctx.toplevel_items
            |> OrdMap.get(name)
            |> Option.unwrap;
        let loop_detected = () => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    let output = @current Output;
                    output.write("Detected a loop processing ");
                    output.write(name);
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
                        let output = @current Output;
                        output.write("Declaration is processed but defintion is not for ");
                        output.write(name);
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
                f(() => process_toplevel_item_impl(name))
            )
            | :None => (
                process_toplevel_item_impl(name);
            )
        );
        let item = &ctx.toplevel_items
            |> OrdMap.get(name)
            |> Option.unwrap;
        match item^.implementation with (
            | :Processed item => match item with (
                | :Const expr => (
                    &mut ctx.program.consts |> OrdMap.add(name, expr);
                    &mut ctx.program.consts_order |> ArrayList.push_back(name);
                )
                | :NativeConst => ()
                | :Type def => (
                    &mut ctx.program.types |> OrdMap.add(name, def);
                )
                | :Fn def => (
                    &mut ctx.program.fns |> OrdMap.add(name, def);
                )
                | :Context ty => (
                    &mut ctx.program.contexts |> OrdMap.add(name, ty);
                )
                | :Template _ => ()
            )
            | _ => panic("unreachable")
        )
    );

    const add_source_ast = (state :: &mut State, ast :: Ast.t) => (
        # TODO this will copy state, need context to be ref?
        with Context = state^;
        for item in Ast.iter_list(
            ast,
            .binary_rule_name = "then",
            .trailing_or_leading_rule_name = :Some "stmt",
        ) do (
            parse_toplevel_item(item);
        );
    );

    const ruleset_path = () -> String => (
        "kast:///mini/syntax.ks"
    );

    const ruleset = () -> SyntaxRuleset.t => (
        let mut lexer = Lexer.new(Source.read(SourcePath.parse(ruleset_path())));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        SyntaxParser.parse_syntax_ruleset(&mut token_stream)
    );

    const add_source = (state :: &mut State, source :: Source) => (
        let ruleset = ruleset();
        let mut lexer = Lexer.new(source);
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let parsed = Parser.parse(
            .ruleset,
            .entire_source_span = Source.entire_span(&source),
            .path = source.path,
            .token_stream = &mut token_stream,
        );
        add_source_ast(state, parsed.ast)
    );

    const compile = (mut state :: State) -> Ir.Program => (
        with Context = state;
        with root_scope.Compiler = {
            .target = state.target,
            # TODO we have to copies now, only works because we do js
            .program = state.program,
            .parse_expr,
            .parse_type,
            .find_ident_ty,
            .lookup_type,
            .add_toplevel_item,
            .resolve_type_aliases,
            .get_toplevel_decl,
            .get_toplevel_impl,
        };
        # Empty scope is need for initializing consts (they do name lookups)
        let mut scope = {
            .parent = :None,
            .vars = OrdMap.new(),
            .found_in_parent = (...) => (),
        };
        with ScopeContext = scope;
        # Outside of template instantiations
        with TemplateArgsContext = {
            .args = ArrayList.new(),
            .by_name = OrdMap.new(),
        };
        while &mut state.toplevel_items_unprocessed |> Queue.pop is :Some name do (
            process_toplevel_item(name);
        );
        state.program
    );
    # TODO should take &
    const resolve_type_aliases = (ty :: Ir.Type) -> Ir.Type => with_return (
        match ty with (
            | :Any => :Any
            | :Unit => :Unit
            | :Int => ty
            | :UInt => ty
            | :IntSpecific _ => ty
            | :Float32 => ty
            | :Float64 => ty
            | :Char => :Char
            | :Bool => :Bool
            | :Native s => :Native s
            | :Named name => (
                let def = &(@current Context).program.types
                    |> OrdMap.get(name)
                    |> Option.unwrap;
                match def^.shape with (
                    | :Alias ty => (
                        Log.trace(
                            () => (
                                let output = @current Output;
                                output.write(name);
                                output.write(" is alias to ");
                                Ir.Print.type_name(&ty);
                            )
                        );
                        resolve_type_aliases(ty)
                    )
                    | _ => :Named name
                )
            )
            | :Ref referenced => :Ref resolve_type_aliases(referenced)
            | :Fn { .call_convention, .args, .result } => (
                let mut resolved_args = ArrayList.new();
                for &arg in &args |> ArrayList.iter do (
                    &mut resolved_args |> ArrayList.push_back(resolve_type_aliases(arg));
                );
                let result = resolve_type_aliases(result);
                :Fn {
                    .call_convention,
                    .args = resolved_args,
                    .result = resolve_type_aliases(result),
                }
            )
            | :UnwindToken {
                .repr,
                .result_ty,
            } => :UnwindToken {
                .repr = resolve_type_aliases(repr),
                .result_ty = resolve_type_aliases(result_ty),
            }
            | :List {
                .repr,
                .element_ty,
            } => :List {
                .repr = resolve_type_aliases(repr),
                .element_ty = resolve_type_aliases(element_ty),
            }
        )
    );
);
