use (import "../../../util.ks").*;
use (import "../../ir/_lib.ks").*;
use (import "../../../diagnostic.ks").*;
use (import "../../../output.ks").*;
use (import "../../../position.ks").*;
use (import "../../../span.ks").*;
use std.collections.OrdMap;
use std.collections.OrdSet;

module:

const C = (
    module:

    use (import "./ast.ks").*;
    use (import "./print.ks").*;

    const Compiled = Ast.Program;

    const print = (compiled :: Compiled) => (
        Print.program(&compiled);
    );

    const ContextT = newtype {
        .program :: Ir.Program,
        .declared_types :: OrdSet.t[String],
        .defined_types :: OrdSet.t[String],
        .fn_types :: OrdMap.t[Ir.FnType, Ast.Ident],
        .fn_capture_types :: OrdMap.t[String, Ast.Ident],
        .result :: Ast.Program,
        .next_id :: Int32,
    };
    const Context = @context ContextT;

    const FunctionContext = @context newtype {
        .capture_mode :: Ir.CaptureMode,
        .captures :: OrdMap.t[String, Ir.Type],
        .result_ty :: Ir.Type,
    };

    const UnwindContext = @context newtype {
        .insert_unwind :: () -> (),
        .cleanup_scope_without_unwind :: () -> (),
    };

    const ScopeT = newtype {
        .block :: &mut Ast.Block,
    };
    const Scope = @context ScopeT;

    const ScopeContextVar = @context newtype {
        .ident :: Ast.Ident,
    };

    const ident = (name :: String) -> Ast.Ident => (
        # TODO make sure its valid C identifier
        {
            .name
        }
    );

    const new_ident = (name_prefix :: String) -> Ast.Ident => (
        let mut ctx = @current Context;
        let ident = ident(name_prefix + to_string(ctx.next_id));
        ctx.next_id += 1;
        ident
    );

    const convert_ty = (ty :: &Ir.Type) -> Ast.Ty => with_return (
        let mut ctx = @current Context;
        make_sure_type_is_declared(ty);
        if ty^.alias_name is :Some name then (
            return :Named ident(name)
        );
        match ty^.shape with (
            | :Any => :Void
            | :Ref ref t => :Pointer convert_ty(t)
            | :Unit => :Void
            | :Int => :Int
            | :UInt => :SizeT
            | :IntSpecific {
                .signed,
                .bits,
            } => (
                :Raw output_to_string(
                    () => (
                        let output = @current Output;
                        if not signed then (
                            output.write("u_");
                        );
                        output.write("int");
                        output.write(to_string(bits));
                        output.write("_t");
                    )
                )
            )
            | :Float32 => :Float32
            | :Float64 => :Float64
            | :Bool => :Bool
            | :Char => :Char
            | :Named name => :Named ident(name)
            | :Native s => :Raw s
            | :UnwindToken {
                .repr = ref repr,
                .result_ty = _,
            } => convert_ty(repr)
            | :DelimitedContinuationToken {
                .repr = ref repr,
                .result_ty = _,
            } => convert_ty(repr)
            | :Array {
                .repr = ref repr,
                .element_ty = _,
            } => convert_ty(repr)
            | :Fn ref f_ty => (
                let ident = fn_type_ident(f_ty);
                :Named ident
            )
            | :ContextObject => :Struct ident("Context")
        )
    );

    const fn_type_ident = (f_ty :: &Ir.FnType) -> Ast.Ident => with_return (
        let mut ctx = @current Context;
        if &ctx.fn_types |> OrdMap.get(f_ty^) is :Some ident then (
            return ident^;
        );
        let ident = init_fn_type_ident(f_ty);
        &mut ctx.fn_types |> OrdMap.add(f_ty^, ident);
        ident
    );

    const init_fn_type_ident = (f_ty :: &Ir.FnType) -> Ast.Ident => with_return (
        let mut ctx = @current Context;
        let {
            .is_closure,
            .call_convention,
            .args = ref args,
            .result = ref result,
        } = f_ty^;
        let mut c_args = ArrayList.new();
        if is_closure then (
            &mut c_args |> ArrayList.push_back(:Pointer :Void);
        );
        if call_convention is :None then (
            &mut c_args
                |> ArrayList.push_back(
                    :Pointer :Struct ident("Context")
                );
        );
        for arg in args |> ArrayList.iter do (
            &mut c_args
                |> ArrayList.push_back(convert_ty(arg));
        );
        let raw_fn_type_name = new_ident(
            output_to_string(
                () => (
                    let output = @current Output;
                    output.write("raw_");
                    Ir.Print.fn_type_as_ident(f_ty);
                )
            )
        );
        let raw_ty_def = {
            .name = raw_fn_type_name,
            .def = :FnPointer {
                .args = c_args,
                .result_ty = convert_ty(result),
            },
        };
        &mut ctx.result.types |> ArrayList.push_back(raw_ty_def);
        if not is_closure then (
            return raw_fn_type_name;
        );
        let fn_type_name = new_ident(
            output_to_string(
                () => (
                    Ir.Print.fn_type_as_ident(f_ty);
                )
            )
        );
        let ty_decl = {
            .name = fn_type_name,
            .def = :Alias :Struct fn_type_name,
        };
        &mut ctx.result.types |> ArrayList.push_back(ty_decl);
        let ty_def = {
            .name = fn_type_name,
            .def = :Struct {
                .fields = (
                    let mut fields = ArrayList.new();
                    &mut fields
                        |> ArrayList.push_back(
                            {
                                .name = ident("captured"),
                                .ty = :Pointer :Void,
                            }
                        );
                    &mut fields
                        |> ArrayList.push_back(
                            {
                                .name = ident("f"),
                                .ty = :Named raw_fn_type_name,
                            }
                        );
                    fields
                )
            },
        };
        &mut ctx.result.types |> ArrayList.push_back(ty_def);
        fn_type_name
    );

    const Pure = newtype {
        .span :: Span,
        .expr :: Option.t[Ast.Expr],
    };

    const void = (span :: Span) -> Pure => {
        .expr = :None,
        .span,
    };

    const ExprContext = @context newtype {
        .span :: Span,
    };

    const pure = (pure :: Pure) -> Ast.Expr => (
        match pure.expr with (
            | :Some expr => expr
            | :None => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Compiler,
                    .message = () => (
                        let output = @current Output;
                        output.write("This expression is void, and can't be used as a value");
                    ),
                    .span = pure.span,
                    .related = single_element_list(
                        {
                            .message = () => (
                                let output = @current Output;
                                output.write("While compiling this expr");
                            ),
                            .span = (@current ExprContext).span,
                        }
                    ),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        )
    );

    const Place = newtype {
        .get :: () -> Pure,
        .set :: Ast.Expr -> (),
    };

    const current_context_place = (
        name :: String,
        .span :: Span,
    ) -> Place => (
        let place_expr = :Field {
            .obj = :Deref :Ident (@current ScopeContextVar).ident,
            .field = ident(name),
        };
        {
            .get = () => {
                .expr = :Some place_expr,
                .span,
            },
            .set = value => (
                insert_stmt(:Assign { .assignee = place_expr, .value });
            ),
        }
    );

    const calculate_place = (ir_expr :: &Ir.PlaceExpr) -> Place => with_return (
        let span = ir_expr^.span;
        with ExprContext = { .span };
        match ir_expr^.shape with (
            | :Ident name => find_ident(&ir_expr^.ty, name, .span)
            | :Field { .obj = ref obj, .field } => (
                let obj = calculate_place(obj);
                let field_expr = :Field {
                    .obj = pure(obj.get()),
                    .field = ident(field),
                };
                {
                    .get = () => {
                        .expr = :Some field_expr,
                        .span,
                    },
                    .set = value => (
                        insert_stmt(:Assign { .assignee = field_expr, .value });
                    ),
                }
            )
            | :CurrentContext name => (
                current_context_place(name, .span)
            )
            | :Deref ref reference => (
                let reference = pure(calculate(reference));
                let deref_expr = :Deref reference;
                {
                    .get = () => {
                        .expr = :Some deref_expr,
                        .span,
                    },
                    .set = value => (
                        insert_stmt(:Assign { .assignee = deref_expr, .value });
                    ),
                }
            )
            | :Temp ref expr => (
                let value = calculate(expr);
                {
                    .get = () => value,
                    .set = _ => (
                        let diagnostic = {
                            .severity = :Error,
                            .source = :Compiler,
                            .message = () => (
                                let output = @current Output;
                                output.write("Can't assign to temporary expression");
                            ),
                            .span = expr^.span,
                            .related = ArrayList.new(),
                        };
                        Diagnostic.report_and_unwind(diagnostic)
                    ),
                }
            )
            | :ContextObject => (
                let ctx_expr = :Deref :Ident (@current ScopeContextVar).ident;
                {
                    .get = () => {
                        .expr = :Some ctx_expr,
                        .span,
                    },
                    .set = value => (
                        insert_stmt(:Assign { .assignee = ctx_expr, .value });
                    ),
                }
            )
        )
    );

    const calculate_literal = (
        literal :: &Ir.Literal,
        span :: Span,
    ) -> Pure => with_return (
        let literal :: Ast.Literal = match literal^ with (
            | :Bool x => :Bool x
            | :Int x => :Int x
            | :Float x => :Float x
            | :Char x => :Char x
            | :String s => return {
                .expr = :Some :CompoundLiteral {
                    .ty = :Named ident("StringView"),
                    .fields = (
                        let mut fields = ArrayList.new();
                        let contents = {
                            .name = ident("contents"),
                            .value = :Literal :String s,
                        };
                        &mut fields |> ArrayList.push_back(contents);
                        let length = {
                            .name = ident("length"),
                            .value = :Literal :Int to_string(String.utf8_length(s)),
                        };
                        &mut fields |> ArrayList.push_back(length);
                        fields
                    )
                },
                .span,
            }
        );
        {
            .expr = :Some :Literal literal,
            .span,
        }
    );

    const check_unwind = () => (
        insert_stmt(
            :If {
                .cond = :Raw "current_unwinding_raw_token.id",
                .then_case = (
                    let mut block = new_block();
                    with Scope = {
                        .block = &mut block,
                    };
                    (@current UnwindContext).insert_unwind();
                    block
                ),
                .else_case = :None,
            }
        );
    );

    const defer = (f :: () -> ()) => (
        let mut unwind_ctx = @current UnwindContext;
        let handle_unwind_label = new_ident("handle_unwind");
        let after_defer_label = new_ident("after_defer");
        insert_stmt(:Goto after_defer_label);
        insert_stmt(:GotoLabel handle_unwind_label);
        f();
        unwind_ctx.insert_unwind();
        insert_stmt(:GotoLabel after_defer_label);
        unwind_ctx.insert_unwind = () => (
            insert_stmt(:Goto handle_unwind_label);
        );
        let prev_cleanup_scope_without_unwind = unwind_ctx.cleanup_scope_without_unwind;
        unwind_ctx.cleanup_scope_without_unwind = () => (
            f();
            prev_cleanup_scope_without_unwind();
        );
    );

    const construct_type_info = (info :: &Ir.ConstructTypeInfo) -> Ast.Expr => (
        let ctx = @current Context;
        let ty = output_to_string(
            () => (
                Print.ty(&convert_ty(&info^.ty))
            )
        );
        let { size, stride, alignment } = if info^.ty.shape is :Unit then {
            :Raw "0",
            :Raw "0",
            :Raw "1",
        } else {
            :Raw ("sizeof(" + ty + ")"),
            :Raw ("sizeof(" + ty + ")"),
            :Raw ("alignof(" + ty + ")"),
        };
        let members_var = new_ident("members");
        # TODO instead of allocating at runtime we should have const array
        insert_stmt(
            :LetVar {
                .ty = :Named ident("Array_MemberInfo"),
                .ident = members_var,
                .value = :Some :CompoundLiteral {
                    .ty = :Named ident("Array_MemberInfo"),
                    .fields = (
                        let mut fields = ArrayList.new();
                        let length = &info^.members |> ArrayList.length;
                        let length_field = {
                            .name = ident("length"),
                            .value = :Literal :Int to_string(length),
                        };
                        &mut fields |> ArrayList.push_back(length_field);
                        let data_field = {
                            .name = ident("data"),
                            .value = :Raw ("malloc_or_panic_fn(sizeof(MemberInfo) * " + to_string(length) + ")"),
                        };
                        &mut fields |> ArrayList.push_back(data_field);
                        fields
                    ),
                },
            }
        );
        for { i, member } in &info^.members |> ArrayList.iter |> std.iter.enumerate do (
            let mut fields = ArrayList.new();
            let offset_or_name_field = {
                .name = ident("offset_or_name"),
                .value = if member^.ty.shape is :Unit then (
                    :Literal :Int "0"
                ) else (
                    :Raw ("offsetof(" + ty + ", " + ident(member^.name).name + ")")
                )
            };
            &mut fields |> ArrayList.push_back(offset_or_name_field);
            let ty_field = {
                .name = ident("ty"),
                .value = :Ref :Ident ident(member^.type_info_const_name)
            };
            &mut fields |> ArrayList.push_back(ty_field);
            insert_stmt(
                :Assign {
                    .assignee = :RawParts (
                        let mut parts = ArrayList.new();
                        &mut parts
                            |> ArrayList.push_back(
                                :Field {
                                    .obj = :Ident members_var,
                                    .field = ident("data"),
                                }
                            );
                        &mut parts
                            |> ArrayList.push_back(
                                :Raw ("[" + to_string(i) + "]")
                            );
                        parts
                    ),
                    .value = :CompoundLiteral {
                        .ty = :Named ident("MemberInfo"),
                        .fields,
                    },
                }
            );
        );
        let mut fields = ArrayList.new();
        let members_field = {
            .name = ident("members"),
            .value = :Ident members_var,
        };
        &mut fields |> ArrayList.push_back(members_field);
        let stride_field = {
            .name = ident("stride"),
            .value = stride,
        };
        &mut fields |> ArrayList.push_back(stride_field);
        let size_field = {
            .name = ident("size"),
            .value = size,
        };
        &mut fields |> ArrayList.push_back(size_field);
        let alignment_field = {
            .name = ident("alignment"),
            .value = alignment,
        };
        &mut fields |> ArrayList.push_back(alignment_field);
        :CompoundLiteral {
            .ty = :Named ident("TypeInfo"),
            .fields,
        }
    );

    const find_ident = (ty :: &Ir.Type, name :: String, .span :: Span) -> Place => (
        let ident_expr = match (
            &(@current FunctionContext).captures |> OrdMap.get(name)
        ) with (
            | :Some _ => (
                let captured_field = :Field {
                    .obj = :Deref :Ident ident("captured"),
                    .field = ident(name),
                };
                match (@current FunctionContext).capture_mode with (
                    | :Move => captured_field
                    | :ByRef => :Deref captured_field
                )
            )
            | :None => :Ident ident(name)
        );
        {
            .get = () => {
                .expr = if ty^.shape is :Unit then (
                    :None
                ) else (
                    :Some ident_expr
                ),
                .span,
            },
            .set = value => (
                if ty^.shape is :Unit then (
                    insert_stmt(:Expr value);
                ) else (
                    insert_stmt(:Assign { .assignee = ident_expr, .value });
                );
            ),
        }
    );

    const with_scope = [T] (scope :: ScopeT, f :: () -> T) -> T => (
        with Scope = scope;
        with UnwindContext = {
            .insert_unwind = (@current UnwindContext).insert_unwind,
            .cleanup_scope_without_unwind = () => (),
        };
        let result = f();
        (@current UnwindContext).cleanup_scope_without_unwind();
        result
    );

    const uninitialized = (ty :: &Ir.Type, .span :: Span) -> Pure => with_return (
        if ty^.shape is :Unit then (
            return void(span);
        );
        let mut block = new_block();
        with Scope = {
            .block = &mut block,
        };
        let ident = new_ident("uninitialized");
        insert_stmt(
            :LetVar {
                .ty = convert_ty(ty),
                .ident,
                .value = :None,
            }
        );
        insert_stmt(
            :RawParts (
                let mut parts = ArrayList.new();
                &mut parts |> ArrayList.push_back(:Raw "memset(");
                &mut parts |> ArrayList.push_back(:Ref :Ident ident);
                &mut parts |> ArrayList.push_back(:Raw ", 0, sizeof(");
                &mut parts |> ArrayList.push_back(:Ident ident);
                &mut parts |> ArrayList.push_back(:Raw "))");
                parts
            )
        );
        insert_stmt(:Expr :Ident ident);
        { .expr = :Some :Stmt block, .span }
    );

    const calculate = (ir_expr :: &Ir.Expr) -> Pure => with_return (
        let mut ctx = @current Context;
        let span = ir_expr^.span;
        with ExprContext = { .span };
        let expr :: Ast.Expr = match ir_expr^.shape with (
            | :Unit => return void(span)
            | :Array ref ir_elements => (
                let element_ty = match ir_expr^.ty.shape with (
                    | :Array { .element_ty = ref element_ty, ... } => element_ty
                    | _ => panic("Array expr is not array type???")
                );
                let data = (
                    let mut elements = ArrayList.new();
                    for element in ir_elements |> ArrayList.iter do (
                        let element = pure(calculate(element));
                        &mut elements |> ArrayList.push_back(element);
                    );
                    :ArrayLiteral {
                        .ty = convert_ty(element_ty),
                        .elements,
                    }
                );
                let mut fields = ArrayList.new();
                let data_field = {
                    .name = ident("data"),
                    .value = data,
                };
                &mut fields |> ArrayList.push_back(data_field);
                let length_field = {
                    .name = ident("length"),
                    .value = :Literal :Int to_string(ir_elements |> ArrayList.length),
                };
                &mut fields |> ArrayList.push_back(length_field);
                :CompoundLiteral {
                    .ty = convert_ty(&ir_expr^.ty),
                    .fields,
                }
            )
            | :Uninitialized => (
                return uninitialized(&ir_expr^.ty, .span)
            )
            | :Claim ref place => return calculate_place(place).get()
            | :Ref ref place_expr => (
                let place = calculate_place(place_expr);
                if place_expr^.ty.shape is :Unit then (
                    return { .expr = :Some :Raw "NULL", .span }
                ) else (
                    return {
                        .expr = :Some :Ref pure(place.get()),
                        .span,
                    }
                )
            )
            | :Native { .parts = ref parts } => (
                let mut raw_parts = ArrayList.new();
                let mut stmt = false;
                for part in parts |> ArrayList.iter do (
                    match part^ with (
                        | :Raw s => (
                            if s |> String.strip_prefix(.prefix = "stmt:") is :Some s then (
                                stmt = true;
                                let mut start = 0;
                                while (
                                    start < String.length(s)
                                    and String.at(s, start) |> Char.is_whitespace
                                ) do (
                                    start += Char.string_encoding_len(String.at(s, start));
                                );
                                let s = String.substring_from(s, start);
                                &mut raw_parts |> ArrayList.push_back(:Raw s);
                                continue;
                            );
                            if String.strip_prefix(s, .prefix = "#include ") is :Some @"include" then (
                                &mut ctx.result.includes |> OrdSet.add(@"include");
                                return void(span);
                            );
                            &mut raw_parts |> ArrayList.push_back(:Raw s);
                        )
                        | :Interpolated ref expr => (
                            let mut block = new_block();
                            with Scope = {
                                .block = &mut block,
                            };
                            let result = calculate(expr);
                            let part = if &block.stmts |> ArrayList.length == 0 then (
                                pure(result)
                            ) else (
                                if result.expr is :Some expr then (
                                    insert_stmt(:Expr expr);
                                );
                                :Stmt block
                            );
                            &mut raw_parts |> ArrayList.push_back(part);
                        )
                    )
                );
                if stmt then (
                    insert_stmt(:RawParts raw_parts);
                    return void(span);
                );
                :RawParts raw_parts
            )
            | :Literal ref literal => (
                return calculate_literal(literal, span)
            )
            | :Variant name => (
                return {
                    .expr = :Some :Ident enum_variant_name(&ir_expr^.ty, name),
                    .span,
                }
            )
            | :Stmt ref expr => (
                calculate(expr);
                return void(span)
            )
            | :Then ref exprs => (
                let mut result = void(span);
                for expr in exprs |> ArrayList.iter do (
                    result = calculate(expr);
                );
                return result
            )
            | :Loop ref body_expr => (
                let mut body = new_block();
                with_scope(
                    { .block = &mut body },
                    () => calculate(body_expr),
                );
                insert_stmt(
                    :For {
                        .init = :None,
                        .test = :None,
                        .incr = :None,
                        .body,
                    }
                );
                return void(span)
            )
            | :If { .cond = ref cond, .then_case = ref then_case, .else_case = ref else_case } => (
                let ident = new_ident("if_result");
                let is_void = if ir_expr^.ty.shape is :Unit then true else false;
                if not is_void then (
                    insert_stmt(
                        :LetVar {
                            .ty = convert_ty(&ir_expr^.ty),
                            .ident,
                            .value = :None,
                        }
                    );
                );
                let cond = pure(calculate(cond));
                let then_case = (
                    let mut block = new_block();
                    with Scope = {
                        .block = &mut block,
                    };
                    if calculate(then_case).expr is :Some result then (
                        insert_stmt(:Assign { .assignee = :Ident ident, .value = result });
                    );
                    block
                );
                let else_case = match else_case^ with (
                    | :None => :None
                    | :Some ref else_case => (
                        let mut block = new_block();
                        with Scope = {
                            .block = &mut block,
                        };
                        if calculate(else_case).expr is :Some result then (
                            insert_stmt(:Assign { .assignee = :Ident ident, .value = result });
                        );
                        :Some block
                    )
                );
                insert_stmt(:If { .cond, .then_case, .else_case });
                return {
                    .expr = if is_void then :None else :Some :Ident ident,
                    .span = span,
                }
            )
            | :Let { .name, .value = ref value } => (
                if value^.ty.shape is :Unit then (
                    calculate(value);
                    return void(span);
                );
                if value^.shape is :Uninitialized then (
                    insert_stmt(
                        :LetVar {
                            .ty = convert_ty(&value^.ty),
                            .ident = ident(name),
                            .value = :None,
                        }
                    );
                ) else (
                    let_var(&value^.ty, ident(name), pure(calculate(value)));
                );
                return void(span)
            )
            | :LetContextRef ref new_ctx_ptr => (
                let ctx_var = new_ident("ctx");
                let_var(
                    &{
                        .shape = :Ref {
                            .shape = :ContextObject,
                            .alias_name = :None,
                        },
                        .alias_name = :None,
                    },
                    ctx_var,
                    pure(calculate(new_ctx_ptr)),
                );
                (@current ScopeContextVar).ident = ctx_var;
                return void(span)
            )
            | :Assign { .assignee = ref assignee, .value = ref value } => (
                let assignee = calculate_place(assignee);
                let value = pure(calculate(value));
                assignee.set(value);
                return void(span)
            )
            | :Scope ref expr => (
                with_scope(
                    { .block = (@current Scope).block },
                    () => return calculate(expr)
                )
            )
            | :Apply { .f = ref f, .args = ref ir_args } => (
                let f_ty = match f^.ty.shape with (
                    | :Fn ty => ty
                    | _ => (
                        let diagnostic = {
                            .severity = :Error,
                            .source = :Internal,
                            .message = () => (
                                let output = @current Output;
                                output.write("Expected a fn type");
                            ),
                            .span = f^.span,
                            .related = ArrayList.new(),
                        };
                        Diagnostic.report_and_unwind(diagnostic)
                    )
                );
                let f = pure(calculate(f));
                let mut args = ArrayList.new();
                if f_ty.is_closure then (
                    &mut args
                        |> ArrayList.push_back(
                            :Field {
                                .obj = f,
                                .field = ident("captured"),
                            }
                        );
                );
                let f = if f_ty.is_closure then (
                    :Field {
                        .obj = f,
                        .field = ident("f"),
                    }
                ) else f;
                if f_ty.call_convention is :None then (
                    &mut args |> ArrayList.push_back(:Ident (@current ScopeContextVar).ident);
                );
                for arg in ir_args |> ArrayList.iter do (
                    let arg = pure(calculate(arg));
                    &mut args |> ArrayList.push_back(arg);
                );
                if ir_expr^.ty.shape is :Unit then (
                    insert_stmt(:Expr :Apply { .f, .args });
                    if f_ty.call_convention is :None then (
                        check_unwind();
                    );
                    return void(span)
                ) else (
                    let result_var = new_ident("apply_result");
                    let_var(&ir_expr^.ty, result_var, :Apply { .f, .args });
                    if f_ty.call_convention is :None then (
                        check_unwind();
                    );
                    return { .expr = :Some :Ident result_var, .span }
                )
            )
            | :InjectContext { .name, .value = ref value_expr } => (
                let value = calculate(value_expr);
                if value.expr is :Some value then (
                    let ctx = current_context_place(name, .span);
                    let old_value_var = new_ident("old_ctx");
                    let_var(&value_expr^.ty, old_value_var, pure(ctx.get()));
                    ctx.set(value);
                    defer(() => ctx.set(:Ident old_value_var));
                );
                return void(span)
            )
            | :EnumIs { .enum = ref enum_expr, .variant } => (
                return {
                    .expr = :Some :Equal {
                        pure(calculate(enum_expr)),
                        :Ident enum_variant_name(&enum_expr^.ty, variant)
                    },
                    .span,
                }
            )
            | :Record ref fields => (
                let mut field_initializers = ArrayList.new();
                for field in fields |> ArrayList.iter do (
                    let value = calculate(&field^.value);
                    if value.expr is :Some value then (
                        let field = {
                            .name = ident(field^.name),
                            .value,
                        };
                        &mut field_initializers |> ArrayList.push_back(field);
                    );
                );
                :CompoundLiteral {
                    .ty = convert_ty(&ir_expr^.ty),
                    .fields = field_initializers,
                }
            )
            | :Defer ref expr => (
                defer(
                    () => (
                        calculate(expr);
                    )
                );
                return void(span)
            )
            | :Unwind {
                .token = ref token,
                .value = ref value,
            } => (
                let token = pure(calculate(token));
                let value = calculate(value);
                if value.expr is :Some value then (
                    insert_stmt(
                        :Assign {
                            .assignee = :Field {
                                .obj = :Deref token,
                                .field = ident("value"),
                            },
                            .value,
                        }
                    );
                );
                insert_stmt(
                    :Assign {
                        .assignee = :Raw "current_unwinding_raw_token",
                        .value = :Field {
                            .obj = :Deref token,
                            .field = ident("raw"),
                        },
                    }
                );
                # TODO cleanup before return
                (
                    @current UnwindContext
                ).insert_unwind();
                # TODO what if its not void?
                return void(span)
            )
            | :Unwindable {
                .token_ty = ref token_ty,
                .token,
                .body = ref body,
            } => (
                let token_own_var = new_ident("token_own");
                let_var(
                    token_ty,
                    token_own_var,
                    :CompoundLiteral {
                        .ty = convert_ty(token_ty),
                        .fields = single_element_list(
                            { .name = ident("raw"), .value = :Raw "RawUnwindToken_new_fn()" }
                        ),
                    },
                );
                let token_var = ident(token);
                let_var(
                    &{
                        .shape = :Ref token_ty^,
                        .alias_name = :None,
                    },
                    token_var,
                    :Ref :Ident token_own_var,
                );
                let have_result_value = match ir_expr^.ty.shape with (
                    | :Unit => false
                    | _ => true
                );
                let result_var = new_ident("unwindable_result");
                if have_result_value then (
                    insert_stmt(
                        :LetVar {
                            .ty = convert_ty(&ir_expr^.ty),
                            .ident = result_var,
                            .value = :None,
                        }
                    );
                );
                let handle_unwind_label = new_ident("handle_unwind");
                let unwindable_body_label = new_ident("unwindable_body");
                let after_unwindable_label = new_ident("after_unwindable");
                insert_stmt(:Goto unwindable_body_label);
                insert_stmt(:GotoLabel handle_unwind_label);
                insert_stmt(
                    :If {
                        .cond = :Apply {
                            .f = :Ident ident("is_unwinding_with_fn"),
                            .args = single_element_list(
                                :Field {
                                    .obj = :Ident token_own_var,
                                    .field = ident("raw"),
                                }
                            ),
                        },
                        .then_case = (
                            let mut block = new_block();
                            with Scope = {
                                .block = &mut block,
                            };
                            insert_stmt(
                                :Assign {
                                    .assignee = :Ident ident("current_unwinding_raw_token"),
                                    .value = :Ident ident("NO_UNWIND"),
                                }
                            );
                            if have_result_value then (
                                insert_stmt(
                                    :Assign {
                                        .assignee = :Ident result_var,
                                        .value = :Field {
                                            .obj = :Ident token_own_var,
                                            .field = ident("value"),
                                        },
                                    }
                                );
                            );
                            insert_stmt(:Goto after_unwindable_label);
                            block
                        ),
                        .else_case = :Some (
                            let mut block = new_block();
                            with Scope = {
                                .block = &mut block,
                            };
                            (@current UnwindContext).insert_unwind();
                            block
                        ),
                    }
                );
                insert_stmt(:GotoLabel unwindable_body_label);
                with UnwindContext = {
                    .insert_unwind = () => (
                        insert_stmt(:Goto handle_unwind_label);
                    ),
                    .cleanup_scope_without_unwind = () => (),
                };
                let result = calculate(body);
                (@current UnwindContext).cleanup_scope_without_unwind();
                if have_result_value then (
                    insert_stmt(
                        :Assign {
                            .assignee = :Ident result_var,
                            .value = pure(result),
                        }
                    );
                );
                insert_stmt(:GotoLabel after_unwindable_label);
                return {
                    .expr = if have_result_value then (
                        :Some :Ident result_var
                    ) else (
                        :None
                    ),
                    .span,
                }
            )
            | :DelimitedContinuation {
                .capture_mode,
                .captures = ref captures,
                .token_ty = ref token_ty,
                .resume_fn,
                .token = token_name,
                .body = ref body,
            } => (
                let coro = new_ident("coro");
                let ctx_var = new_ident("ctx");
                let token_name = ident(token_name);
                let captured_ty_name = new_ident("delimited_captures");
                let mut captured_ty_fields = (
                    let mut fields = captured_to_field_defs(capture_mode, captures);
                    make_sure_type_is_complete(token_ty);
                    let token_field = {
                        .name = token_name,
                        .ty = convert_ty(token_ty),
                    };
                    &mut fields |> ArrayList.push_back(token_field);
                    let ctx_field = {
                        .name = ctx_var,
                        .ty = :Pointer :Struct ident("Context"), # TODO maybe should own?
                    };
                    &mut fields |> ArrayList.push_back(ctx_field);
                    fields
                );
                let captured_ty = {
                    .name = captured_ty_name,
                    .def = :Struct { .fields = captured_ty_fields },
                };
                let captured_decl = {
                    .name = captured_ty_name,
                    .def = :Alias :Struct captured_ty_name,
                };
                &mut ctx.result.types |> ArrayList.push_back(captured_decl);
                &mut ctx.result.types |> ArrayList.push_back(captured_ty);
                let captured_var = new_ident("captured");
                let captured_ty = :Named captured_ty_name;
                construct_captured(captured_var, captured_ty, capture_mode, captures, .span);
                let body_fn_ident = new_ident("delimited_body");
                let coro_fn = new_ident("delimited_coro");
                add_fn_impl(
                    .ctx_var,
                    .capture_mode,
                    .captures,
                    .signature = {
                        .name = body_fn_ident,
                        .args = single_element_list({ .name = coro, .ty = :Raw "mco_coro*" }),
                        .result_ty = convert_ty(&ir_expr^.ty),
                    },
                    .before_body = () => (
                        insert_stmt(
                            :LetVar {
                                .ty = :Pointer captured_ty,
                                .ident = ident("captured"),
                                .value = :Some :Apply {
                                    .f = :Ident ident("mco_get_user_data"),
                                    .args = single_element_list(:Ident coro),
                                },
                            }
                        );
                        let_var(
                            &{
                                .shape = :Ref {
                                    .shape = :ContextObject,
                                    .alias_name = :None,
                                },
                                .alias_name = :None,
                            },
                            ctx_var,
                            :Field { .obj = :Deref :Ident ident("captured"), .field = ctx_var },
                        );
                        let_var(
                            token_ty,
                            token_name,
                            :Field { .obj = :Deref :Ident ident("captured"), .field = token_name },
                        );
                    ),
                    .body,
                );
                &mut ctx.result.fns
                    |> ArrayList.push_back(
                        {
                            .signature = {
                                .name = coro_fn,
                                .args = single_element_list({ .name = ident("co"), .ty = :Raw "mco_coro*" }),
                                .result_ty = :Void,
                            },
                            .body = (
                                let mut block = new_block();
                                with Scope = { .block = &mut block };
                                let call :: Ast.Expr = :Apply {
                                    .f = :Ident body_fn_ident,
                                    .args = single_element_list(:Ident ident("co")),
                                };
                                if ir_expr^.ty.shape is :Unit then (
                                    insert_stmt(:Expr call);
                                ) else (
                                    insert_stmt(
                                        :LetVar {
                                            .ty = convert_ty(&ir_expr^.ty),
                                            .ident = ident("result"),
                                            .value = :Some call,
                                        }
                                    );
                                    insert_mco_stmt(
                                        :Apply {
                                            .f = :Ident ident("mco_push"),
                                            .args = (
                                                let mut args = ArrayList.new();
                                                &mut args |> ArrayList.push_back(:Ident ident("co"));
                                                &mut args |> ArrayList.push_back(:Ref :Ident ident("result"));
                                                &mut args
                                                    |> ArrayList.push_back(
                                                        :Apply {
                                                            .f = :Ident ident("sizeof"),
                                                            .args = single_element_list(:Ident ident("result")),
                                                        }
                                                    );
                                                args
                                            ),
                                        }
                                    );
                                );
                                block
                            ),
                        }
                    );
                let desc = new_ident("desc");
                insert_stmt(
                    :LetVar {
                        .ty = :Raw "mco_desc",
                        .ident = desc,
                        .value = :Some :Apply {
                            .f = :Raw "mco_desc_init",
                            .args = (
                                let mut args = ArrayList.new();
                                &mut args |> ArrayList.push_back(:Ident coro_fn);
                                &mut args |> ArrayList.push_back(:Literal :Int "0");
                                args
                            ),
                        },
                    }
                );
                insert_stmt(
                    :Assign {
                        .assignee = :Field { .obj = :Ident desc, .field = ident("user_data") },
                        .value = :Ident captured_var,
                    }
                );
                insert_stmt(
                    :Assign {
                        .assignee = :Field { .obj = :Deref :Ident captured_var, .field = ctx_var },
                        .value = :Ident (@current ScopeContextVar).ident,
                    }
                );
                insert_stmt(
                    :LetVar {
                        .ty = :Raw "mco_coro*",
                        .ident = coro,
                        .value = :None,
                    }
                );
                insert_mco_stmt(
                    :Apply {
                        .f = :Raw "mco_create",
                        .args = (
                            let mut args = ArrayList.new();
                            &mut args |> ArrayList.push_back(:Ref :Ident coro);
                            &mut args |> ArrayList.push_back(:Ref :Ident desc);
                            args
                        ),
                    },
                );
                insert_stmt(
                    :Assign {
                        .assignee = :Field {
                            .obj = :Deref :Ident captured_var,
                            .field = token_name,
                        },
                        .value = :CompoundLiteral {
                            .ty = convert_ty(token_ty),
                            .fields = (
                                let mut fields = ArrayList.new();
                                let coro_field = {
                                    .name = ident("coro"),
                                    .value = :Ident coro,
                                };
                                &mut fields |> ArrayList.push_back(coro_field);
                                fields
                            ),
                        },
                    }
                );
                :Apply {
                    .f = :Ident ident(resume_fn + "_fn"),
                    .args = single_element_list(
                        :Field {
                            .obj = :Deref :Ident captured_var,
                            .field = token_name,
                        }
                    ),
                }
            )
            | :CaptureContinuation {
                .token = ref token,
                .continuation_ty_repr = ref continuation_ty_repr,
                .continuation = continuation_name,
                .resume_fn,
                .body = ref body,
            } => (
                make_sure_type_is_complete(continuation_ty_repr);
                let continuation_name = ident(continuation_name);
                let token = pure(calculate(token));
                let coro :: Ast.Expr = :Field {
                    .obj = token,
                    .field = ident("coro"),
                };
                let_var(
                    continuation_ty_repr,
                    continuation_name,
                    :CompoundLiteral {
                        .ty = convert_ty(continuation_ty_repr),
                        .fields = (
                            let mut fields = ArrayList.new();
                            let token_field = {
                                .name = ident("token"),
                                .value = token,
                            };
                            &mut fields |> ArrayList.push_back(token_field);
                            let f_field = {
                                .name = ident("f"),
                                .value = :Ident ident(resume_fn + "_fn"),
                            };
                            &mut fields |> ArrayList.push_back(f_field);
                            fields
                        ),
                    },
                );
                if body^.ty.shape is :Unit then (
                    calculate(body);
                ) else (
                    let result_var = new_ident("result");
                    let_var(&body^.ty, result_var, pure(calculate(body)));
                    insert_mco_stmt(
                        :Apply {
                            .f = :Ident ident("mco_push"),
                            .args = (
                                let mut args = ArrayList.new();
                                &mut args |> ArrayList.push_back(coro);
                                &mut args |> ArrayList.push_back(:Ref :Ident result_var);
                                &mut args
                                    |> ArrayList.push_back(
                                        :Apply {
                                            .f = :Ident ident("sizeof"),
                                            .args = single_element_list(:Ident result_var),
                                        }
                                    );
                                args
                            ),
                        }
                    );
                );
                insert_mco_stmt(
                    :Apply {
                        .f = :Ident ident("mco_yield"),
                        .args = single_element_list(coro),
                    }
                );
                return {
                    .span,
                    .expr = :Some :Ident ident("TODO")
                }
            )
            | :ConstructTypeInfo ref ty => construct_type_info(ty)
            | :Fn ref fn_def => (
                make_sure_type_is_complete(&ir_expr^.ty);
                let name = new_ident("closure");
                add_fn(name.name, fn_def);
                let { f :: Ast.Expr, captured :: Ast.Expr } = if (
                    &fn_def^.captures |> OrdMap.length != 0
                ) then (
                    let captured_var = new_ident("captured");
                    let captured_ty = :Named (
                        &ctx.fn_capture_types
                            |> OrdMap.get(name.name)
                            |> Option.unwrap_or_else(() => panic("captured ty not initialized for fn"))
                    )^;
                    construct_captured(
                        captured_var,
                        captured_ty,
                        fn_def^.capture_mode,
                        &fn_def^.captures,
                        .span,
                    );
                    { :Ident ident(name.name + "_fn"), :Ident captured_var }
                ) else (
                    { :Ident ident(name.name + "_as_closure"), :Ident ident("NULL") }
                );
                return {
                    .expr = :Some :CompoundLiteral {
                        .ty = convert_ty(&ir_expr^.ty),
                        .fields = (
                            let mut fields = ArrayList.new();
                            let captured_field = {
                                .name = ident("captured"),
                                .value = captured,
                            };
                            &mut fields |> ArrayList.push_back(captured_field);
                            let f_field = {
                                .name = ident("f"),
                                .value = f,
                            };
                            &mut fields |> ArrayList.push_back(f_field);
                            fields
                        )
                    },
                    .span,
                }
            )
        );
        if ir_expr^.ty.shape is :Unit then (
            insert_stmt(:Expr expr);
            void(span)
        ) else (
            let ident = new_ident("value");
            let_var(&ir_expr^.ty, ident, expr);
            {
                .expr = :Some :Ident ident,
                .span = span,
            }
        )
    );

    const let_var = (ty :: &Ir.Type, ident :: Ast.Ident, value :: Ast.Expr) => (
        insert_stmt(
            :LetVar {
                .ty = convert_ty(ty),
                .ident,
                .value = :Some value,
            }
        );
    );

    const insert_stmt = (stmt :: Ast.Stmt) => (
        &mut (@current Scope).block^.stmts |> ArrayList.push_back(stmt);
    );

    const insert_mco_stmt = (expr :: Ast.Expr) => (
        let result_var = new_ident("mco_result");
        insert_stmt(
            :LetVar {
                .ty = :Raw "mco_result",
                .ident = result_var,
                .value = :Some expr,
            }
        );
        insert_stmt(
            :Expr :Apply {
                .f = :Ident ident("assert"),
                .args = (
                    let mut args = ArrayList.new();
                    let equals = :Equal {
                        :Ident result_var,
                        :Ident ident("MCO_SUCCESS"),
                    };
                    &mut args |> ArrayList.push_back(equals);
                    args
                ),
            }
        );
    );

    const new_block = () -> Ast.Block => {
        .stmts = ArrayList.new(),
    };

    const construct_captured = (
        captured_var :: Ast.Ident,
        captured_ty :: Ast.Ty,
        capture_mode :: Ir.CaptureMode,
        captures :: &OrdMap.t[String, Ir.Type],
        .span :: Span,
    ) => (
        insert_stmt(
            :LetVar {
                .ty = :Pointer captured_ty,
                .ident = captured_var,
                .value = :Some :Raw output_to_string(
                    () => (
                        let output = @current Output;
                        output.write("malloc_or_panic_fn(sizeof(");
                        Print.ty(&captured_ty);
                        output.write("))");
                    )
                ),
            }
        );
        insert_stmt(
            :Assign {
                .assignee = :Deref :Ident captured_var,
                .value = :CompoundLiteral {
                    .ty = captured_ty,
                    .fields = (
                        let mut fields = ArrayList.new();
                        for &{ .key = name, .value = ref ty } in captures |> OrdMap.iter do (
                            let value = pure(find_ident(ty, name, .span).get());
                            let value = match capture_mode with (
                                | :Move => value
                                | :ByRef => :Ref value
                            );
                            let field = {
                                .name = ident(name),
                                .value = value,
                            };
                            &mut fields |> ArrayList.push_back(field);
                        );
                        fields
                    )
                }
            }
        );
    );

    const captured_to_field_defs = (
        capture_mode :: Ir.CaptureMode,
        captures :: &OrdMap.t[String, Ir.Type],
    ) -> ArrayList.t[Ast.FieldDef] => (
        let mut fields = ArrayList.new();
        for &{ .key = name, .value = ref ty } in captures |> OrdMap.iter do (
            make_sure_type_is_complete(ty);
            let ty = convert_ty(ty);
            let ty = match capture_mode with (
                | :Move => ty
                | :ByRef => :Pointer ty
            );
            let field = {
                .name = ident(name),
                .ty,
            };
            &mut fields |> ArrayList.push_back(field);
        );
        fields
    );

    const add_fn_impl = (
        .ctx_var :: Ast.Ident,
        .capture_mode :: Ir.CaptureMode,
        .captures :: &OrdMap.t[String, Ir.Type],
        .signature :: Ast.FnSignature,
        .before_body :: () -> (),
        .body :: &Ir.Expr,
    ) => (
        let mut ctx = @current Context;
        with FunctionContext = {
            .capture_mode,
            .captures = captures^, # TODO
            .result_ty = body^.ty,
        };
        with UnwindContext = {
            .insert_unwind = () => (
                match uninitialized(
                    &body^.ty,
                    .span = {
                        .start = Position.beginning(),
                        .end = Position.beginning(),
                        .path = :Special __FILE__
                    },
                ).expr with (
                    | :Some expr => insert_stmt(:Return expr)
                    | :None => insert_stmt(:ReturnVoid)
                );
            ),
            .cleanup_scope_without_unwind = () => (),
        };
        let fn :: Ast.Fn = {
            .signature,
            .body = (
                let mut block = new_block();
                with Scope = { .block = &mut block };
                with ScopeContextVar = { .ident = ctx_var };
                before_body();
                let result = calculate(body);
                (@current UnwindContext).cleanup_scope_without_unwind();
                if result.expr is :Some expr then (
                    insert_stmt(:Return expr);
                );
                block
            ),
        };
        &mut ctx.result.fns |> ArrayList.push_back(fn);
    );

    const add_fn = (
        name :: String,
        def :: &Ir.FnDef,
    ) => (
        let fn_name = name + "_fn";
        let mut ctx = @current Context;
        let mut args = ArrayList.new();
        if &def^.captures |> OrdMap.length != 0 then (
            let ty_def = {
                .name = ident(fn_name + "_captured"),
                .def = :Struct {
                    .fields = captured_to_field_defs(def^.capture_mode, &def^.captures),
                },
            };
            let ty_decl = {
                .name = ty_def.name,
                .def = :Alias :Struct ty_def.name,
            };
            &mut ctx.result.types |> ArrayList.push_back(ty_decl);
            &mut ctx.fn_capture_types |> OrdMap.add(name, ty_def.name);
            &mut ctx.result.types |> ArrayList.push_back(ty_def);
            &mut args
                |> ArrayList.push_back(
                    {
                        .name = ident("captured_void"),
                        .ty = :Pointer :Void,
                    }
                );
        );
        let ctx_var = new_ident("ctx");
        if def^.call_convention is :None then (
            &mut args
                |> ArrayList.push_back(
                    {
                        .name = ctx_var,
                        .ty = :Pointer :Struct ident("Context"),
                    }
                );
        );
        for arg in &def^.args |> ArrayList.iter do (
            make_sure_type_is_complete(&arg^.ty);
            let arg :: Ast.FnArg = {
                .name = ident(arg^.name),
                .ty = convert_ty(&arg^.ty),
            };
            &mut args |> ArrayList.push_back(arg);
        );
        make_sure_type_is_complete(&def^.result_ty);
        add_fn_impl(
            .ctx_var,
            .capture_mode = def^.capture_mode,
            .captures = &def^.captures,
            .signature = {
                .name = ident(fn_name),
                .args,
                .result_ty = convert_ty(&def^.result_ty),
            },
            .before_body = () => (
                if &ctx.fn_capture_types |> OrdMap.get(name) is :Some &captured_ty then (
                    insert_stmt(
                        :LetVar {
                            .ty = :Pointer :Named captured_ty,
                            .ident = ident("captured"),
                            .value = :Some :Ident ident("captured_void"),
                        }
                    );
                );
            ),
            .body = &def^.body,
        );
        if &def^.captures |> OrdMap.length == 0 then (
            let fn_as_closure :: Ast.Fn = {
                .signature = {
                    .name = ident(name + "_as_closure"),
                    .args = (
                        let mut args_as_closure = ArrayList.new();
                        &mut args_as_closure
                            |> ArrayList.push_back(
                                {
                                    .name = ident("captured_nothing"),
                                    .ty = :Pointer :Void,
                                }
                            );
                        for &arg in &args |> ArrayList.iter do (
                            &mut args_as_closure |> ArrayList.push_back(arg);
                        );
                        args_as_closure
                    ),
                    .result_ty = convert_ty(&def^.result_ty),
                },
                .body = (
                    let mut block = new_block();
                    with Scope = { .block = &mut block };
                    with ScopeContextVar = { .ident = ctx_var };
                    insert_stmt(
                        :Return :Apply {
                            .f = :Ident ident(fn_name),
                            .args = (
                                let mut call_args = ArrayList.new();
                                for arg in &args |> ArrayList.iter do (
                                    &mut call_args |> ArrayList.push_back(:Ident arg^.name);
                                );
                                call_args
                            )
                        }
                    );
                    block
                ),
            };
            &mut ctx.result.fns |> ArrayList.push_back(fn_as_closure);
            let fn_ty :: Ir.FnType = {
                .is_closure = true, # TODO
                .call_convention = def^.call_convention,
                .args = (
                    let mut args = ArrayList.new();
                    for arg in &def^.args |> ArrayList.iter do (
                        &mut args |> ArrayList.push_back(arg^.ty);
                    );
                    args
                ),
                .result = def^.result_ty,
            };
            let fn_ty :: Ir.Type = { .shape = :Fn fn_ty, .alias_name = :None };
            make_sure_type_is_complete(&fn_ty);
            let static = {
                .@"const" = true,
                .ty = convert_ty(&fn_ty),
                .name = ident(name),
                .value = :Some :CompoundLiteral {
                    .ty = convert_ty(&fn_ty),
                    .fields = (
                        let mut fields = ArrayList.new();
                        let captured_field = {
                            .name = ident("captured"),
                            .value = :Ident ident("NULL"),
                        };
                        &mut fields |> ArrayList.push_back(captured_field);
                        let f_field = {
                            .name = ident("f"),
                            .value = :Ident fn_as_closure.signature.name,
                        };
                        &mut fields |> ArrayList.push_back(f_field);
                        fields
                    )
                },
            };
            &mut ctx.result.statics |> ArrayList.push_back(static);
        );
    );

    const make_sure_type_is_declared = (ty :: &Ir.Type) => with_return (
        if ty^.alias_name is :Some name then (
            let def = &(@current Context).program.types
                |> OrdMap.get(name)
                |> Option.unwrap;
            type_def(name, def);
            return;
        );
        match Ir.type_repr(ty)^.shape with (
            | :Any => ()
            | :Ref _ => ()
            | :Unit => ()
            | :Int => ()
            | :UInt => ()
            | :IntSpecific _ => ()
            | :Float32 => ()
            | :Float64 => ()
            | :Bool => ()
            | :Char => ()
            | :Named name => (
                let def = &(@current Context).program.types
                    |> OrdMap.get(name)
                    |> Option.unwrap;
                type_decl(name, def);
            )
            | :Fn ref fn_ty => (
                fn_type_ident(fn_ty);
            )
            | :Native String => ()
            | :ContextObject => (
                let ctx_decl = {
                    .name = ident("Context"),
                    .def = :Alias :Struct ident("Context"),
                };
                &mut (@current Context).result.types
                    |> ArrayList.push_back(ctx_decl);
            )
        )
    );

    const make_sure_type_is_complete = (ty :: &Ir.Type) => (
        if ty^.alias_name is :Some name then (
            let def = &(@current Context).program.types
                |> OrdMap.get(name)
                |> Option.unwrap;
            type_def(name, def);
        );
        match Ir.type_repr(ty)^.shape with (
            | :Any => ()
            | :Ref _ => ()
            | :Unit => ()
            | :Int => ()
            | :UInt => ()
            | :IntSpecific _ => ()
            | :Float32 => ()
            | :Float64 => ()
            | :Bool => ()
            | :Char => ()
            | :Named name => (
                let def = &(@current Context).program.types
                    |> OrdMap.get(name)
                    |> Option.unwrap;
                type_def(name, def);
            )
            | :Fn ref fn_ty => (
                fn_type_ident(fn_ty);
            )
            | :Native String => ()
            | :ContextObject => (
                let mut ctx = @current Context;
                let mut ctx_fields = ArrayList.new();
                for &{ .key = name, .value = ref ty } in &ctx.program.contexts |> OrdMap.iter do (
                    make_sure_type_is_complete(ty);
                    let field = {
                        .name = ident(name),
                        .ty = convert_ty(ty),
                    };
                    &mut ctx_fields |> ArrayList.push_back(field);
                );
                let ctx_def = {
                    .name = ident("Context"),
                    .def = :Struct { .fields = ctx_fields },
                };
                &mut ctx.result.types |> ArrayList.push_back(ctx_def);
            )
        )
    );

    const enum_variant_name = (enum_ty :: &Ir.Type, variant :: String) -> Ast.Ident => (
        let s = output_to_string(
            () => (
                let output = @current Output;
                Ir.Print.type_name(enum_ty);
                output.write("_");
                output.write(variant);
            )
        );
        ident(s)
    );

    const type_decl = (name :: String, def :: &Ir.TypeDef) => with_return (
        let mut ctx = @current Context;
        if &ctx.declared_types |> OrdSet.contains(name) then (
            return;
        );
        &mut ctx.declared_types |> OrdSet.add(name);
        if def^.native then (
            return;
        );
        let def :: Ast.TyDefShape = match def^.shape with (
            | :Opaque => panic("opaque type can only be native?")
            | :Enum _ => :Alias :Enum ident(name)
            | :Union _ => :Alias :Union ident(name)
            | :Struct _ => :Alias :Struct ident(name)
            | :Alias _ => (
                type_def(name, def);
                return
            )
        );
        let def :: Ast.TyDef = {
            .name = ident(name),
            .def,
        };
        &mut ctx.result.types |> ArrayList.push_back(def);
    );

    const type_def = (name :: String, def :: &Ir.TypeDef) => with_return (
        type_decl(name, def);
        let mut ctx = @current Context;
        if &ctx.defined_types |> OrdSet.contains(name) then (
            return;
        );
        &mut ctx.defined_types |> OrdSet.add(name);
        if def^.native then (
            return;
        );
        match def^.shape with (
            | :Opaque => ()
            | :Enum _ => ()
            | :Union { .variants = ref variants } => (
                for &{ .key = _, .value = ref ty } in variants |> OrdMap.iter do (
                    make_sure_type_is_complete(ty);
                );
            )
            | :Struct { .fields = ref fields } => (
                for &{ .key = _, .value = ref ty } in fields |> OrdMap.iter do (
                    make_sure_type_is_complete(ty);
                );
            )
            | :Alias ref ty => (
                make_sure_type_is_complete(ty);
            )
        );
        let def :: Ast.TyDefShape = match def^.shape with (
            | :Opaque => :Alias :Pointer :Void
            | :Enum { .variants = ref variants } => :Enum (
                let mut idents = ArrayList.new();
                for &variant in variants |> OrdSet.iter do (
                    &mut idents
                        |> ArrayList.push_back(
                            enum_variant_name(
                                &{
                                    .shape = :Named name,
                                    .alias_name = :None,
                                },
                                variant,
                            )
                        );
                );
                { .variants = idents }
            )
            | :Union { .variants = ref variants } => :Union (
                let mut ast_fields = ArrayList.new();
                for &{ .key = name, .value = ref ty } in variants |> OrdMap.iter do (
                    if ty^.shape is :Unit then (
                        continue;
                    );
                    let field = {
                        .name = ident(name),
                        .ty = convert_ty(ty),
                    };
                    &mut ast_fields |> ArrayList.push_back(field);
                );
                { .fields = ast_fields }
            )
            | :Struct { .fields = ref fields } => :Struct (
                let mut ast_fields = ArrayList.new();
                for &{ .key = name, .value = ref ty } in fields |> OrdMap.iter do (
                    if ty^.shape is :Unit then (
                        continue;
                    );
                    let field = {
                        .name = ident(name),
                        .ty = convert_ty(ty),
                    };
                    &mut ast_fields |> ArrayList.push_back(field);
                );
                { .fields = ast_fields }
            )
            | :Alias ref ty => :Alias convert_ty(ty)
        );
        let def :: Ast.TyDef = {
            .name = ident(name),
            .def,
        };
        &mut ctx.result.types |> ArrayList.push_back(def);
    );

    const compile = (program :: Ir.Program) -> Compiled => (
        with Context = {
            .declared_types = OrdSet.new(),
            .defined_types = OrdSet.new(),
            .fn_types = OrdMap.new_with_compare(
                (a, b) => Ir.compare_fn_type(&a, &b),
            ),
            .fn_capture_types = OrdMap.new(),
            .program,
            .result = {
                .includes = OrdSet.new(),
                .types = ArrayList.new(),
                .statics = ArrayList.new(),
                .fns = ArrayList.new(),
            },
            .next_id = 0,
        };
        let mut ctx = @current Context;
        # for int32_t and similar
        &mut ctx.result.includes
            |> OrdSet.add("<stdint.h>");
        &mut ctx.result.includes |> OrdSet.add("<sys/types.h>");
        &mut ctx.result.includes |> OrdSet.add("<sys/types.h>");
        &mut ctx.result.includes |> OrdSet.add("<stdalign.h>");
        &mut ctx.result.includes |> OrdSet.add("<stddef.h>");
        &mut ctx.result.includes |> OrdSet.add("<stdbool.h>");
        &mut ctx.result.includes |> OrdSet.add("<assert.h>");
        &mut ctx.result.includes |> OrdSet.add("<stdlib.h>");
        make_sure_type_is_complete(
            &{ .shape = :ContextObject, .alias_name = :None }
        );
        for &{ .key = name, .value = ref ty_def } in &ctx.program.types |> OrdMap.iter do (
            type_def(name, ty_def);
        );
        for &{ .key = name, .value = ref def } in &ctx.program.fns |> OrdMap.iter do (
            add_fn(name, def);
        );

        (
            # consts
            let mut args = ArrayList.new();
            let ctx_var = new_ident("ctx");
            &mut args
                |> ArrayList.push_back(
                    {
                        .name = ctx_var,
                        .ty = :Pointer :Named ident("Context"),
                    }
                );
            with FunctionContext = {
                .capture_mode = :ByRef,
                .captures = OrdMap.new(),
                .result_ty = { .shape = :Unit, .alias_name = :None },
            };
            with UnwindContext = {
                .insert_unwind = () => (
                    # TODO this would mean we didnt finish initializing all consts
                    insert_stmt(:ReturnVoid);
                ),
                .cleanup_scope_without_unwind = () => (),
            };

            let mut body = new_block();
            with Scope = { .block = &mut body };
            with ScopeContextVar = { .ident = ctx_var };
            for &name in &ctx.program.consts_order |> ArrayList.iter do (
                let value = &ctx.program.consts |> OrdMap.get(name) |> Option.unwrap;
                let ty = &value^.ty;
                let value = calculate(value);
                if value.expr is :Some value then (
                    let static = {
                        .@"const" = false,
                        .ty = convert_ty(ty),
                        .name = ident(name),
                        .value = :None,
                    };
                    &mut ctx.result.statics |> ArrayList.push_back(static);
                    insert_stmt(:Assign { .assignee = :Ident ident(name), .value });
                );
            );
            let fn :: Ast.Fn = {
                .signature = {
                    .name = ident("init_consts"),
                    .args,
                    .result_ty = :Void,
                },
                .body,
            };
            (@current UnwindContext).cleanup_scope_without_unwind();
            &mut ctx.result.fns |> ArrayList.push_back(fn);
        );
        ctx.result
    );
);
