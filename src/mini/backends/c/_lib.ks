use (import "../../../util.ks").*;
use (import "../../ir/_lib.ks").*;
use (import "../../../diagnostic.ks").*;
use (import "../../../output.ks").*;
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
        .defined_types :: OrdSet.t[String],
        .fn_types :: OrdMap.t[Ir.Type, Ast.Ident],
        .result :: Ast.Program,
        .next_id :: Int32,
    };
    const Context = @context ContextT;

    const FunctionContext = @context newtype {
        .result_ty :: Ast.Ty,
    };

    const UnwindContext = @context newtype {
        .insert_unwind :: () -> (),
        .cleanup_scope_without_unwind :: () -> (),
    };

    const ScopeT = newtype {
        .block :: &mut Ast.Block,
    };
    const Scope = @context ScopeT;

    const ScopeContextVar = @context Ast.Ident;

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

    const convert_ty = (ty :: &Ir.Type) -> Ast.Ty => (
        let mut ctx = @current Context;
        match ty^ with (
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
            | :Float64 => :Float64
            | :Bool => :Bool
            | :Char => :Char
            | :Named name => (
                let def = &ctx.program.types
                    |> OrdMap.get(name)
                    |> Option.unwrap_or_else(
                        () => panic("Failed to find type " + String.escape(name))
                    );
                if def^.shape is :Struct _ then (
                    :Struct ident(name)
                ) else (
                    :Named ident(name)
                )
            )
            | :Native s => :Raw s
            | :UnwindToken {
                .repr = ref repr,
                .result_ty = _,
            } => convert_ty(repr)
            | :List {
                .repr = ref repr,
                .element_ty = _,
            } => convert_ty(repr)
            | :Fn _ => (
                let &ident = &ctx.fn_types
                    |> OrdMap.get(ty^)
                    |> Option.unwrap_or_else(
                        () => panic("fn type was not defined???")
                    );
                :Named ident
            )
        )
    );

    const init_fn_type = (f_ty :: Ir.FnType) -> Ast.Ident => (
        let mut ctx = @current Context;
        let {
            .call_convention,
            .args = ref args,
            .result = ref result,
        } = f_ty;
        let mut c_args = ArrayList.new();
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
        let ident = new_ident("fn_type");
        let ty_def = {
            .name = ident,
            .def = :FnPointer {
                .args = c_args,
                .result_ty = convert_ty(result),
            },
        };
        &mut ctx.result.types |> ArrayList.push_back(ty_def);
        ident
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
            .obj = :Deref :Ident (@current ScopeContextVar),
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
            | :Ident name => {
                .get = () => {
                    .expr = :Some :Ident ident(name),
                    .span,
                },
                .set = value => (
                    insert_stmt(:Assign { .assignee = :Ident ident(name), .value });
                ),
            }
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
        )
    );

    const calculate_literal = (
        literal :: &Ir.Literal,
        span :: Span,
    ) -> Pure => with_return (
        let literal :: Ast.Literal = match literal^ with (
            | :Bool x => :Bool x
            | :Int x => :Int x
            | :Float64 x => :Float64 x
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
                            .value = :Literal :Int to_string(String.length(s)),
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
        let size = :Raw ("sizeof(" + ty + ")");
        let stride = size;
        let alignment = :Raw ("alignof(" + ty + ")");
        let mut members = ArrayList.new();
        let members_var = new_ident("members");
        # TODO instead of allocating at runtime we should have const array
        insert_stmt(
            :LetVar {
                .ty = :Named ident("List_MemberInfo"),
                .ident = members_var,
                .value = :Some :CompoundLiteral {
                    .ty = :Named ident("List_MemberInfo"),
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
                            .value = :Raw ("malloc(sizeof(MemberInfo) * " + to_string(length) + ")"),
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
                .value = :Raw ("offsetof(" + ty + ", " + ident(member^.name).name + ")")
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

    const calculate = (ir_expr :: &Ir.Expr) -> Pure => with_return (
        let mut ctx = @current Context;
        let span = ir_expr^.span;
        with ExprContext = { .span };
        let expr :: Ast.Expr = match ir_expr^.shape with (
            | :Unit => return void(span)
            | :List ref ir_elements => (
                let element_ty = match ir_expr^.ty with (
                    | :List { .element_ty = ref element_ty, ... } => element_ty
                    | _ => panic("List expr is not list type???")
                );
                let data = (
                    let mut elements = ArrayList.new();
                    for element in ir_elements |> ArrayList.iter do (
                        let element = pure(calculate(element));
                        &mut elements |> ArrayList.push_back(element);
                    );
                    :Ref :ArrayLiteral {
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
                if ir_expr^.ty is :Unit then (
                    return void(span);
                );
                let mut block = new_block();
                with Scope = {
                    .block = &mut block,
                };
                let ident = new_ident("uninitialized");
                insert_stmt(
                    :LetVar {
                        .ty = convert_ty(&ir_expr^.ty),
                        .ident,
                        .value = :None,
                    }
                );
                insert_stmt(:Expr :Ident ident);
                :Stmt block
            )
            | :Claim ref place => return calculate_place(place).get()
            | :Ref ref place => return {
                .expr = :Some :Ref pure(calculate_place(place).get()),
                .span = span,
            }
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
                (
                    with Scope = {
                        .block = &mut body,
                    };
                    calculate(body_expr);
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
                let is_void = if ir_expr^.ty is :Unit then true else false;
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
            | :Assign { .assignee = ref assignee, .value = ref value } => (
                let assignee = calculate_place(assignee);
                let value = pure(calculate(value));
                assignee.set(value);
                return void(span)
            )
            # | :Fn FnDef
            | :Scope ref expr => (
                with Scope = {
                    .block = (@current Scope).block,
                };
                with UnwindContext = {
                    .insert_unwind = (@current UnwindContext).insert_unwind,
                    .cleanup_scope_without_unwind = () => (),
                };
                let result = calculate(expr);
                (@current UnwindContext).cleanup_scope_without_unwind();
                return result
            )
            | :Apply { .f = ref f, .args = ref ir_args } => (
                let f_ty = match f^.ty with (
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
                if f_ty.call_convention is :None then (
                    &mut args |> ArrayList.push_back(:Ident (@current ScopeContextVar));
                );
                for arg in ir_args |> ArrayList.iter do (
                    let arg = pure(calculate(arg));
                    &mut args |> ArrayList.push_back(arg);
                );
                if ir_expr^.ty is :Unit then (
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
                .token_ty_repr = ref token_ty_repr,
                .token,
                .body = ref body,
            } => (
                let token_own_var = new_ident("token_own");
                let_var(
                    token_ty_repr,
                    token_own_var,
                    :CompoundLiteral {
                        .ty = convert_ty(token_ty_repr),
                        .fields = single_element_list(
                            { .name = ident("raw"), .value = :Raw "newRawUnwindToken()" }
                        ),
                    },
                );
                let token_var = ident(token);
                let_var(
                    &(:Ref token_ty_repr^),
                    token_var,
                    :Ref :Ident token_own_var,
                );
                let have_result_value = match ir_expr^.ty with (
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
                            .f = :Ident ident("is_unwinding_with"),
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
            | :ConstructTypeInfo ref ty => construct_type_info(ty)
        );
        if ir_expr^.ty is :Unit then (
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

    const new_block = () -> Ast.Block => {
        .stmts = ArrayList.new(),
    };

    const add_fn = (
        name :: String,
        def :: &Ir.FnDef,
    ) => (
        let mut ctx = @current Context;
        let mut args = ArrayList.new();
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
            make_sure_all_are_defined(&arg^.ty);
            let arg :: Ast.FnArg = {
                .name = ident(arg^.name),
                .ty = convert_ty(&arg^.ty),
            };
            &mut args |> ArrayList.push_back(arg);
        );
        make_sure_all_are_defined(&def^.result_ty);
        with FunctionContext = {
            .result_ty = convert_ty(&def^.result_ty),
        };
        with UnwindContext = {
            .insert_unwind = () => (
                match (@current FunctionContext).result_ty with (
                    | :Void => (
                        insert_stmt(:ReturnVoid);
                    )
                    | result_ty => (
                        let result_var = new_ident("return_uninitialized");
                        insert_stmt(
                            :LetVar {
                                .ty = (@current FunctionContext).result_ty,
                                .ident = result_var,
                                .value = :None,
                            }
                        );
                        insert_stmt(:Expr :Ref :Ident result_var); # This prevents UB (i think)
                        insert_stmt(:Return :Ident result_var);
                    )
                );
            ),
            .cleanup_scope_without_unwind = () => (),
        };
        let fn :: Ast.Fn = {
            .signature = {
                .name = ident(name),
                .args,
                .result_ty = convert_ty(&def^.result_ty),
            },
            .body = (
                let mut block = new_block();
                with Scope = { .block = &mut block };
                with ScopeContextVar = ctx_var;
                let result = calculate(&def^.body);
                (@current UnwindContext).cleanup_scope_without_unwind();
                if result.expr is :Some expr then (
                    insert_stmt(:Return expr);
                );
                block
            ),
        };
        &mut ctx.result.fns |> ArrayList.push_back(fn);
    );

    const make_sure_all_are_defined = (ty :: &Ir.Type) => (
        match ty^ with (
            | :Any => ()
            | :Ref ref inner => (
                # type can be forward declared for pointers
                # make_sure_all_are_defined(inner)
            )
            | :Unit => ()
            | :Int => ()
            | :UInt => ()
            | :IntSpecific _ => ()
            | :Float64 => ()
            | :Bool => ()
            | :Char => ()
            | :Named name => (
                let def = &(@current Context).program.types
                    |> OrdMap.get(name)
                    |> Option.unwrap;
                type_def(name, def);
            )
            | :Fn fn_ty => (
                let {
                    .call_convention = _,
                    .args = ref args,
                    .result = ref result,
                } = fn_ty;
                for arg in args |> ArrayList.iter do (
                    make_sure_all_are_defined(arg);
                );
                make_sure_all_are_defined(result);
                &mut (@current Context).fn_types
                    |> OrdMap.get_or_init(ty^, () => init_fn_type(fn_ty));
            )
            | :Native String => ()
            | :UnwindToken {
                .repr = ref repr,
                .result_ty = _,
            } => (
                make_sure_all_are_defined(repr);
            )
            | :List {
                .repr = ref repr,
                .element_ty = _,
            } => (
                make_sure_all_are_defined(repr);
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

    const type_def = (name :: String, def :: &Ir.TypeDef) => with_return (
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
                    make_sure_all_are_defined(ty);
                );
            )
            | :Struct { .fields = ref fields } => (
                for &{ .key = _, .value = ref ty } in fields |> OrdMap.iter do (
                    make_sure_all_are_defined(ty);
                );
            )
            | :Alias ref ty => (
                make_sure_all_are_defined(ty);
            )
        );
        let def :: Ast.TyDefShape = match def^.shape with (
            | :Opaque => :Alias :Pointer :Void
            | :Enum { .variants = ref variants } => :Enum (
                let mut idents = ArrayList.new();
                for &variant in variants |> OrdSet.iter do (
                    &mut idents |> ArrayList.push_back(enum_variant_name(&(:Named name), variant));
                );
                { .variants = idents }
            )
            | :Union { .variants = ref variants } => :Union (
                let mut ast_fields = ArrayList.new();
                for &{ .key = name, .value = ref ty } in variants |> OrdMap.iter do (
                    if ty^ is :Unit then (
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
                    if ty^ is :Unit then (
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
            .defined_types = OrdSet.new(),
            .fn_types = OrdMap.new_with_compare(
                (a, b) => Ir.compare_type(&a, &b),
            ),
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
        for &{ .key = name, .value = ref ty_def } in &ctx.program.types |> OrdMap.iter do (
            type_def(name, ty_def);
        );
        let mut ctx_fields = ArrayList.new();
        for &{ .key = name, .value = ref ty } in &ctx.program.contexts |> OrdMap.iter do (
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
                        .ty = :Pointer :Struct ident("Context"),
                    }
                );
            with FunctionContext = {
                .result_ty = :Void,
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
            with ScopeContextVar = ctx_var;
            for &name in &ctx.program.consts_order |> ArrayList.iter do (
                let value = &ctx.program.consts |> OrdMap.get(name) |> Option.unwrap;
                # with FunctionContext = {
                #     .result_ty = convert_ty(:Unit),
                # };
                # with UnwindContext = {
                #     .insert_unwind = () => (),
                #     .cleanup_scope_without_unwind = () => (),
                # };
                # with Scope = { .block = &mut block };
                # with ScopeContextVar = ctx_var;
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
