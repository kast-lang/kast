use (import "../../ir/_lib.ks").*;
use std.collections.OrdMap;

module:

const JavaScript = (
    module:

    use (import "./ast.ks").*;

    const State = newtype {
        .next_var_id :: Int32,
        .toplevel :: Ast.Block,
    };
    const Context = @context State;

    const ScopeT = newtype {
        .block :: &mut Ast.Block,
        .ctx_var :: Ast.Var,
    };
    const Scope = @context ScopeT;

    const new_block = () -> Ast.Block => {
        .stmts = ArrayList.new(),
    };

    const insert_stmt = (stmt :: Ast.Stmt) => (
        &mut (@current Scope).block^.stmts |> ArrayList.push_back(stmt);
    );

    const var = (name :: String) -> Ast.Var => (
        { .name }
    );

    const new_var = (prefix :: String) -> Ast.Var => (
        let mut ctx = @current Context;
        let name = prefix + "_" + to_string(ctx.next_var_id);
        ctx.next_var_id += 1;
        { .name }
    );

    const Place = newtype {
        .get :: () -> Ast.Expr,
        .set :: Ast.Expr -> (),
    };

    const place_of_var = (var :: Ast.Var) -> Place => {
        .get = () => :Var var,
        .set = value => assign(:Var var, value),
    };

    const place_of_field = (obj :: Place, field :: String) -> Place => {
        .get = () => :Field { .obj = obj.get(), .field },
        .set = value => assign(:Field { .obj = obj.get(), .field }, value),
    };

    const place_to_js = (place :: Place) -> Ast.Expr => (
        let mut fields = ArrayList.new();
        let get = :Fn {
            .args = ArrayList.new(),
            .body = (
                let mut block = new_block();
                with Scope = {
                    .block = &mut block,
                    .ctx_var = (@current Scope).ctx_var,
                };
                let result = place.get();
                &mut block.stmts |> ArrayList.push_back(:Return result);
                block
            ),
        };
        let set_arg = new_var("new_value");
        let set = :Fn {
            .args = (
                let mut args = ArrayList.new();
                &mut args |> ArrayList.push_back(set_arg);
                args
            ),
            .body = (
                let mut block = new_block();
                with Scope = {
                    .block = &mut block,
                    .ctx_var = (@current Scope).ctx_var,
                };
                place.set(:Var set_arg);
                block
            ),
        };
        &mut fields |> ArrayList.push_back(:Field { .name = "get", .value = get });
        &mut fields |> ArrayList.push_back(:Field { .name = "set", .value = set });
        :Obj fields
    );

    const calculate_place = (expr :: Ir.PlaceExpr) -> Place => (
        match expr.shape with (
            | :Ident name => place_of_var(var(name))
            | :Field { .obj, .field } => (
                place_of_field(
                    calculate_place(obj),
                    field,
                )
            )
            | :CurrentContext name => (
                place_of_field(
                    place_of_var((@current Scope).ctx_var),
                    name,
                )
            )
            | :Deref reference => (
                let var = new_var("ref");
                let_var(var, :Var calculate(reference));
                {
                    .get = () => :Apply {
                        .f = :Field { .obj = :Var var, .field = "get" },
                        .args = ArrayList.new(),
                    },
                    .set = new_value => (
                        insert_stmt(
                            :Expr :Apply {
                                .f = :Field { .obj = :Var var, .field = "set" },
                                .args = (
                                    let mut args = ArrayList.new();
                                    &mut args |> ArrayList.push_back(new_value);
                                    args
                                ),
                            }
                        );
                    ),
                }
            )
            | :Index { .list, .index } => (
                let obj = calculate_place(list).get();
                let index = calculate(index);
                {
                    .get = () => :Index { .obj, .index = :Var index },
                    .set = new_value => assign(
                        :Index { .obj, .index = :Var index },
                        new_value
                    ),
                }
            )
            | :Temp expr => (
                let temp_var = new_var("temp");
                let_var(temp_var, :Var calculate(expr));
                place_of_var(temp_var)
            )
        )
    );
    ## calculates expr and stores it in a var
    const calculate = (expr :: Ir.Expr) -> Ast.Var => with_return (
        let value :: Ast.Expr = match expr.shape with (
            | :Unit => :Null
            | :Uninitialized => :Obj ArrayList.new()
            | :Literal literal => match literal with (
                | :Int x => :NumberLiteral parse(x)
                | :Float64 x => :NumberLiteral x
                | :Char c => :StringLiteral to_string(c)
                | :String s => :StringLiteral s
            )
            | :Ref place => place_to_js(calculate_place(place))
            | :Claim place => calculate_place(place).get()
            | :Let { .name, .value } => (
                let value = calculate(value);
                let_var(var(name), :Var value);
                :Undefined
            )
            | :Record fields => (
                let mut js_fields = ArrayList.new();
                for field in fields |> ArrayList.into_iter do (
                    let field = {
                        .name = field.name,
                        .value = :Var calculate(field.value),
                    };
                    &mut js_fields |> ArrayList.push_back(:Field field)
                );
                :Obj js_fields
            )
            | :Assign { .assignee, .value } => (
                let assignee = calculate_place(assignee);
                assignee.set(:Var calculate(value));
                :Undefined
            )
            | :Fn def => compile_fn(def)
            | :Native { .parts = ir_parts } => (
                let mut parts = ArrayList.new();
                let mut first = true;
                let mut stmt = false;
                for part in ir_parts |> ArrayList.into_iter do (
                    let part = match part with (
                        | :Raw s => with_return (
                            if first then (
                                if s |> String.strip_prefix(.prefix = "stmt:") is :Some s then (
                                    stmt = true;
                                    let mut start = 0;
                                    while (
                                        start < String.length(s)
                                        and String.at(s, start) |> Char.is_whitespace
                                    ) do (
                                        start += Char.string_encoding_len(String.at(s, start));
                                    );
                                    return :Raw String.substring_from(s, start);
                                )
                            );
                            :Raw s
                        )
                        | :Interpolated expr => :Var calculate(expr)
                    );
                    first = false;
                    &mut parts |> ArrayList.push_back(part);
                );
                if stmt then (
                    insert_stmt(:RawConcat { .parts });
                    :Undefined
                ) else (
                    :RawConcat { .parts }
                )
            )
            | :InjectContext { .name, .value } => (
                let value = :Var calculate(value);
                let ctx_var = new_var("ctx");
                let_var(
                    ctx_var,
                    :Obj (
                        let mut parts = ArrayList.new();
                        &mut parts |> ArrayList.push_back(:Unpack :Var (@current Scope).ctx_var);
                        &mut parts |> ArrayList.push_back(:Field { .name, .value });
                        parts
                    )
                );
                (@current Scope).ctx_var = ctx_var;
                :Null
            )
            | :Stmt expr => (
                calculate(expr);
                :Null
            )
            | :If { .cond, .then_case, .else_case } => (
                let result_var = new_var("if_result");
                let_var(result_var, :Undefined);
                let cond = calculate(cond);
                let then_case = (
                    let mut block = new_block();
                    with Scope = {
                        .block = &mut block,
                        .ctx_var = (@current Scope).ctx_var,
                    };
                    assign(:Var result_var, :Var calculate(then_case));
                    block
                );
                let else_case = match else_case with (
                    | :Some else_case => :Some (
                        let mut block = new_block();
                        with Scope = {
                            .block = &mut block,
                            .ctx_var = (@current Scope).ctx_var,
                        };
                        assign(:Var result_var, :Var calculate(else_case));
                        block
                    )
                    | :None => :None
                );
                insert_stmt(
                    :If {
                        .cond = :Var cond,
                        .then_case,
                        .else_case,
                    }
                );
                return result_var
            )
            | :Then exprs => (
                let mut result = :Null;
                for expr in exprs |> ArrayList.into_iter do (
                    result = :Var calculate(expr);
                );
                result
            )
            | :Scope expr => (
                with Scope = { ...(@current Scope) };
                return calculate(expr)
            )
            | :Apply { .f, .args = ir_args } => (
                let f :: Ast.Var = calculate(f);
                let mut args :: ArrayList.t[Ast.Expr] = ArrayList.new();
                &mut args |> ArrayList.push_back(:Var (@current Scope).ctx_var);
                for arg in ir_args |> ArrayList.into_iter do (
                    let arg :: Ast.Expr = :Var calculate(arg);
                    &mut args |> ArrayList.push_back(arg);
                );
                :Apply {
                    .f = :Var f,
                    .args,
                }
            )
            | :EnumIs { .enum, .variant } => :Equal {
                .lhs = :Var calculate(enum),
                .rhs = :StringLiteral variant,
            }
            | :Variant name => :StringLiteral name
        );
        let result_var = new_var("temp");
        let_var(result_var, value);
        result_var
    );

    const compile_fn = (fn :: Ir.FnDef) -> Ast.Expr => (
        let mut args = ArrayList.new();
        let ctx_var = new_var("ctx");
        &mut args |> ArrayList.push_back(ctx_var);
        for arg in fn.args |> ArrayList.into_iter do (
            let arg = var(arg.name);
            &mut args |> ArrayList.push_back(arg);
        );
        let mut body = new_block();
        with Scope = {
            .block = &mut body,
            .ctx_var,
        };
        let result_var = calculate(fn.body);
        insert_stmt(:Return :Var result_var);
        :Fn {
            .args,
            .body,
        }
    );

    const let_var = (var :: Ast.Var, value :: Ast.Expr) => (
        let stmt = :Let { .var, .value };
        insert_stmt(stmt);
    );

    const assign = (assignee :: Ast.Expr, value :: Ast.Expr) => (
        let stmt = :Assign { .assignee, .value };
        insert_stmt(stmt);
    );

    const Compiled = newtype {
        .toplevel :: Ast.Block,
    };

    const print = (compiled :: Compiled) => (
        Ast.Print.stmts(compiled.toplevel.stmts);
    );

    const compile = (mut program :: Ir.Program) -> Compiled => (
        let mut state :: State = {
            .next_var_id = 0,
            .toplevel = new_block(),
        };
        with Context = state;
        let ctx_var = new_var("ctx");
        with Scope = {
            .block = &mut state.toplevel,
            .ctx_var,
        };
        let_var(ctx_var, :Obj ArrayList.new());
        for { .key = name, .value = def } in program.fns |> OrdMap.into_iter do (
            let_var(var(name), compile_fn(def));
        );
        for name in program.consts_order |> ArrayList.into_iter do (
            let value = &mut program.consts |> OrdMap.remove(name) |> Option.unwrap;
            let_var(var(name), :Var calculate(value));
        );
        if &program.fns |> OrdMap.get("main") is :Some _ then (
            insert_stmt(
                :Expr :Apply {
                    .f = :Var var("main"),
                    .args = (
                        let mut args = ArrayList.new();
                        &mut args |> ArrayList.push_back(:Var ctx_var);
                        args
                    ),
                }
            );
        );
        { .toplevel = state.toplevel }
    );
);
