use (import "../../../util.ks").*;
use (import "../../ir/_lib.ks").*;
use (import "../../../diagnostic.ks").*;
use (import "../../../output.ks").*;
use (import "../../../position.ks").*;
use (import "../../../span.ks").*;
use (import "../../../rc_str.ks").*;
use std.collections.OrdMap;
use std.collections.OrdSet;
use std.rc.Rc;

module:

const C = (
    module:

    use (import "./ast.ks").*;
    use (import "./print.ks").*;

    const Compiled = Ast.Program;

    const print = (compiled :: &Compiled) => (
        Print.program(compiled);
    );

    const ContextT = newtype {
        .program :: Ir.Program,
        .declared_types :: OrdSet.t[RcStr],
        .defined_types :: OrdSet.t[RcStr],
        .fn_types :: OrdMap.t[Ir.FnType, Ast.Ident],
        .fn_capture_types :: OrdMap.t[RcStr, Ast.Ident],
        .result :: Ast.Program,
        .next_id :: Int32,
    };
    const Context = @context type (&mut ContextT);

    const FunctionContext = @context newtype {
        .capture_mode :: Ir.CaptureMode,
        .captures :: &OrdMap.t[RcStr, Ir.Type],
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

    const ident = (name :: &RcStr) -> Ast.Ident => (
        # TODO make sure its valid C identifier
        {
            .name = name^
        }
    );

    const new_ident = (name_prefix :: &str) -> Ast.Ident => (
        let ctx = @current Context;
        let ident = ident(&RcStr.from_String(format!("\(name_prefix)\(ctx^.next_id)")));
        ctx^.next_id += 1;
        ident
    );

    const convert_ty = (ty :: &Ir.Type) -> Ast.Ty => with_return (
        let ctx = @current Context;
        make_sure_type_is_declared(ty);
        if ty^.alias_name is :Some ref name then (
            return :Named ident(name)
        );
        match ty^.shape^ with (
            | :Any => :Void
            | :Ref ref t => :Pointer Rc.new(convert_ty(t))
            | :Unit => :Void
            | :Int => :Int
            | :UInt => :SizeT
            | :IntSpecific {
                .signed,
                .bits,
            } => (
                :Raw RcStr.from_String(output_to_string(() => (
                    if not signed then (
                        locked_output.write_str("u_");
                    );
                    locked_output.write_str("int");
                    locked_output.write_str(&to_string(bits) |> as_str);
                    locked_output.write_str("_t");
                )))
            )
            | :Float32 => :Float32
            | :Float64 => :Float64
            | :Bool => :Bool
            | :Char => :Char
            | :Named ref name => :Named ident(name)
            | :Native ref s => :Raw s^
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
            | :ContextObject => :Struct ident(&RcStr.allocate("Context"))
        )
    );

    const fn_type_ident = (f_ty :: &Ir.FnType) -> Ast.Ident => with_return (
        let ctx = @current Context;
        if &ctx^.fn_types |> OrdMap.get(f_ty) is :Some ident then (
            return ident^;
        );
        let ident = init_fn_type_ident(f_ty);
        &mut ctx^.fn_types |> OrdMap.add(f_ty^, ident);
        ident
    );

    const init_fn_type_ident = (f_ty :: &Ir.FnType) -> Ast.Ident => with_return (
        let ctx = @current Context;
        let {
            .is_closure,
            .call_convention,
            .args = ref args,
            .result = ref result,
        } = f_ty^;
        let mut c_args = ArrayList.new();
        if is_closure then (
            &mut c_args |> ArrayList.push_back(:Pointer Rc.new(:Void));
        );
        if call_convention is :None then (
            &mut c_args
                |> ArrayList.push_back(:Pointer Rc.new(:Struct ident(&RcStr.allocate("Context"))));
        );
        for arg in args |> ArrayList.iter do (
            &mut c_args
                |> ArrayList.push_back(convert_ty(arg));
        );
        let raw_fn_type_name = new_ident(
            &output_to_string(() => (
                locked_output.write_str("raw_");
                Ir.Print.fn_type_as_ident(f_ty);
            )) |> as_str
        );
        let raw_ty_def = {
            .name = raw_fn_type_name,
            .def = :FnPointer {
                .args = c_args,
                .result_ty = convert_ty(result),
            },
        };
        &mut ctx^.result.types |> ArrayList.push_back(raw_ty_def);
        if not is_closure then (
            return raw_fn_type_name;
        );
        let fn_type_name = new_ident(
            &output_to_string(() => (
                Ir.Print.fn_type_as_ident(f_ty);
            )) |> as_str
        );
        let ty_decl = {
            .name = fn_type_name,
            .def = :Alias :Struct fn_type_name,
        };
        &mut ctx^.result.types |> ArrayList.push_back(ty_decl);
        let ty_def = {
            .name = fn_type_name,
            .def = :Struct {
                .fields = (
                    let mut fields = ArrayList.new();
                    &mut fields
                        |> ArrayList.push_back({
                            .name = ident(&RcStr.allocate("captured")),
                            .ty = :Pointer Rc.new(:Void),
                        });
                    &mut fields
                        |> ArrayList.push_back({
                            .name = ident(&RcStr.allocate("f")),
                            .ty = :Named raw_fn_type_name,
                        });
                    fields
                )
            },
        };
        &mut ctx^.result.types |> ArrayList.push_back(ty_def);
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
                        locked_output.write_str("This expression is void, and can't be used as a value");
                    ),
                    .span = pure.span,
                    .related = single_element_list({
                        .message = () => (
                            locked_output.write_str("While compiling this expr");
                        ),
                        .span = (@current ExprContext).span,
                    }),
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
        name :: &RcStr,
        .span :: Span,
    ) -> Place => (
        let place_expr = {
            .shape = Rc.new(:Field {
                .obj = {
                    .shape = Rc.new(:Deref {
                        .shape = Rc.new(:Ident (@current ScopeContextVar).ident),
                    }),
                },
                .field = ident(name),
            }),
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
        match ir_expr^.shape^ with (
            | :Ident ref name => find_ident(&ir_expr^.ty, name, .span)
            | :Field { .obj = ref obj, .field = ref field } => (
                let obj = calculate_place(obj);
                let field_expr = {
                    .shape = Rc.new(:Field {
                        .obj = pure(obj.get()),
                        .field = ident(field),
                    }),
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
            | :CurrentContext ref name => (
                current_context_place(name, .span)
            )
            | :Deref ref reference => (
                let reference = pure(calculate(reference));
                let deref_expr = {
                    .shape = Rc.new(:Deref reference),
                };
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
                                locked_output.write_str("Can't assign to temporary expression");
                            ),
                            .span = expr^.span,
                            .related = ArrayList.new(),
                        };
                        Diagnostic.report_and_unwind(diagnostic)
                    ),
                }
            )
            | :ContextObject => (
                let ctx_expr = {
                    .shape = Rc.new(:Deref {
                        .shape = Rc.new(:Ident (@current ScopeContextVar).ident),
                    }),
                };
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
            | :String ref s => return {
                .expr = :Some {
                    .shape = Rc.new(:CompoundLiteral {
                        .ty = :Named ident(&RcStr.allocate("StringView")),
                        .fields = (
                            let mut fields = ArrayList.new();
                            let contents = {
                                .name = ident(&RcStr.allocate("contents")),
                                .value = { .shape = Rc.new(:Literal :String s^) },
                            };
                            &mut fields |> ArrayList.push_back(contents);
                            let length = {
                                .name = ident(&RcStr.allocate("length")),
                                .value = { .shape = Rc.new(:Literal :Int RcStr.from_String(to_string(String.utf8_length(s |> RcStr.as_str)))) },
                            };
                            &mut fields |> ArrayList.push_back(length);
                            fields
                        )
                    }),
                },
                .span,
            }
        );
        {
            .expr = :Some {
                .shape = Rc.new(:Literal literal),
            },
            .span,
        }
    );

    const check_unwind = () => (
        insert_stmt(:If {
            .cond = { .shape = Rc.new(:Raw RcStr.allocate("current_unwinding_raw_token.id")) },
            .then_case = (
                let mut block = new_block();
                with Scope = {
                    .block = &mut block,
                };
                (@current UnwindContext).insert_unwind();
                block
            ),
            .else_case = :None,
        });
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
        let ty = output_to_string(() => (
            Print.ty(&convert_ty(&info^.ty))
        ));
        let { size, stride, alignment } = if info^.ty.shape^ is :Unit then {
            { .shape = Rc.new(:Raw RcStr.allocate("0")) },
            { .shape = Rc.new(:Raw RcStr.allocate("0")) },
            { .shape = Rc.new(:Raw RcStr.allocate("1")) },
        } else {
            { .shape = Rc.new(:Raw RcStr.from_String(format!("sizeof(\(ty))"))) },
            { .shape = Rc.new(:Raw RcStr.from_String(format!("sizeof(\(ty))"))) },
            { .shape = Rc.new(:Raw RcStr.from_String(format!("alignof(\(ty))"))) },
        };
        let members_var = new_ident("members");
        # TODO instead of allocating at runtime we should have const array
        insert_stmt(:LetVar {
            .ty = :Named ident(&RcStr.allocate("Array_MemberInfo")),
            .ident = members_var,
            .value = :Some {
                .shape = Rc.new(:CompoundLiteral {
                    .ty = :Named ident(&RcStr.allocate("Array_MemberInfo")),
                    .fields = (
                        let mut fields = ArrayList.new();
                        let length = &info^.members |> ArrayList.length;
                        let length_field = {
                            .name = ident(&RcStr.allocate("length")),
                            .value = { .shape = Rc.new(:Literal :Int RcStr.from_String(to_string(length))) },
                        };
                        &mut fields |> ArrayList.push_back(length_field);
                        let data_field = {
                            .name = ident(&RcStr.allocate("data")),
                            .value = { .shape = Rc.new(:Raw RcStr.from_String(format!("malloc_or_panic_fn(sizeof(MemberInfo) * \(length))"))) },
                        };
                        &mut fields |> ArrayList.push_back(data_field);
                        fields
                    ),
                }),
            },
        });
        for { i, member } in &info^.members |> ArrayList.iter |> std.iter.enumerate do (
            let mut fields = ArrayList.new();
            let offset_or_name_field = {
                .name = ident(&RcStr.allocate("offset_or_name")),
                .value = {
                    .shape = Rc.new(if member^.ty.shape^ is :Unit then (
                        :Literal :Int RcStr.allocate("0")
                    ) else (
                        :Raw RcStr.from_String(format!("offsetof(\(ty), \(ident(&member^.name).name))"))
                    )),
                },
            };
            &mut fields |> ArrayList.push_back(offset_or_name_field);
            let ty_field = {
                .name = ident(&RcStr.allocate("ty")),
                .value = {
                    .shape = Rc.new(:Ref {
                        .shape = Rc.new(:Ident ident(&member^.type_info_const_name)),
                    }),
                },
            };
            &mut fields |> ArrayList.push_back(ty_field);
            insert_stmt(:Assign {
                .assignee = {
                    .shape = Rc.new(:RawParts (
                        let mut parts = ArrayList.new();
                        &mut parts
                            |> ArrayList.push_back({
                                .shape = Rc.new(:Field {
                                    .obj = { .shape = Rc.new(:Ident members_var) },
                                    .field = ident(&RcStr.allocate("data")),
                                }),
                            });
                        &mut parts
                            |> ArrayList.push_back({
                                .shape = Rc.new(:Raw RcStr.from_String(format!("[\(i)]"))),
                            });
                        parts
                    )),
                },
                .value = {
                    .shape = Rc.new(:CompoundLiteral {
                        .ty = :Named ident(&RcStr.allocate("MemberInfo")),
                        .fields,
                    }),
                },
            });
        );
        let mut fields = ArrayList.new();
        let members_field = {
            .name = ident(&RcStr.allocate("members")),
            .value = { .shape = Rc.new(:Ident members_var) },
        };
        &mut fields |> ArrayList.push_back(members_field);
        let stride_field = {
            .name = ident(&RcStr.allocate("stride")),
            .value = stride,
        };
        &mut fields |> ArrayList.push_back(stride_field);
        let size_field = {
            .name = ident(&RcStr.allocate("size")),
            .value = size,
        };
        &mut fields |> ArrayList.push_back(size_field);
        let alignment_field = {
            .name = ident(&RcStr.allocate("alignment")),
            .value = alignment,
        };
        &mut fields |> ArrayList.push_back(alignment_field);
        {
            .shape = Rc.new(:CompoundLiteral {
                .ty = :Named ident(&RcStr.allocate("TypeInfo")),
                .fields,
            }),
        }
    );

    const find_ident = (ty :: &Ir.Type, name :: &RcStr, .span :: Span) -> Place => (
        let ident_expr = match (
            (@current FunctionContext).captures |> OrdMap.get(name)
        ) with (
            | :Some _ => (
                let captured_field = {
                    .shape = Rc.new(:Field {
                        .obj = {
                            .shape = Rc.new(:Deref {
                                .shape = Rc.new(:Ident ident(&RcStr.allocate("captured"))),
                            }),
                        },
                        .field = ident(name),
                    }),
                };
                match (@current FunctionContext).capture_mode with (
                    | :Move => captured_field
                    | :ByRef => {
                        .shape = Rc.new(:Deref captured_field),
                    }
                )
            )
            | :None => { .shape = Rc.new(:Ident ident(name)) }
        );
        {
            .get = () => {
                .expr = if ty^.shape^ is :Unit then (
                    :None
                ) else (
                    :Some ident_expr
                ),
                .span,
            },
            .set = value => (
                if ty^.shape^ is :Unit then (
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
        if ty^.shape^ is :Unit then (
            return void(span);
        );
        let mut block = new_block();
        with Scope = {
            .block = &mut block,
        };
        let ident = new_ident("uninitialized");
        insert_stmt(:LetVar {
            .ty = convert_ty(ty),
            .ident,
            .value = :None,
        });
        insert_stmt(:RawParts (
            let mut parts = ArrayList.new();
            &mut parts |> ArrayList.push_back({ .shape = Rc.new(:Raw RcStr.allocate("memset(")) });
            &mut parts |> ArrayList.push_back({ .shape = Rc.new(:Ref { .shape = Rc.new(:Ident ident) }) });
            &mut parts |> ArrayList.push_back({ .shape = Rc.new(:Raw RcStr.allocate(", 0, sizeof(")) });
            &mut parts |> ArrayList.push_back({ .shape = Rc.new(:Ident ident) });
            &mut parts |> ArrayList.push_back({ .shape = Rc.new(:Raw RcStr.allocate("))")) });
            parts
        ));
        insert_stmt(:Expr { .shape = Rc.new(:Ident ident) });
        { .expr = :Some { .shape = Rc.new(:Stmt block) }, .span }
    );

    const calculate = (ir_expr :: &Ir.Expr) -> Pure => with_return (
        let ctx = @current Context;
        let span = ir_expr^.span;
        with ExprContext = { .span };
        let expr :: Ast.Expr = match ir_expr^.shape^ with (
            | :Unit => return void(span)
            | :Array ref ir_elements => (
                let element_ty = match ir_expr^.ty.shape^ with (
                    | :Array { .element_ty = ref element_ty, ... } => element_ty
                    | _ => panic("Array expr is not array type???")
                );
                let data = (
                    let mut elements = ArrayList.new();
                    for element in ir_elements |> ArrayList.iter do (
                        let element = pure(calculate(element));
                        &mut elements |> ArrayList.push_back(element);
                    );
                    {
                        .shape = Rc.new(:ArrayLiteral {
                            .ty = convert_ty(element_ty),
                            .elements,
                        }),
                    }
                );
                let mut fields = ArrayList.new();
                let data_field = {
                    .name = ident(&RcStr.allocate("data")),
                    .value = data,
                };
                &mut fields |> ArrayList.push_back(data_field);
                let length_field = {
                    .name = ident(&RcStr.allocate("length")),
                    .value = {
                        .shape = Rc.new(:Literal :Int RcStr.from_String(to_string(ir_elements |> ArrayList.length))),
                    },
                };
                &mut fields |> ArrayList.push_back(length_field);
                {
                    .shape = Rc.new(:CompoundLiteral {
                        .ty = convert_ty(&ir_expr^.ty),
                        .fields,
                    }),
                }
            )
            | :Uninitialized => (
                return uninitialized(&ir_expr^.ty, .span)
            )
            | :Claim ref place => return calculate_place(place).get()
            | :Ref ref place_expr => (
                let place = calculate_place(place_expr);
                if place_expr^.ty.shape^ is :Unit then (
                    return { .expr = :Some { .shape = Rc.new(:Raw RcStr.allocate("NULL")) }, .span }
                ) else (
                    return {
                        .expr = :Some { .shape = Rc.new(:Ref pure(place.get())) },
                        .span,
                    }
                )
            )
            | :Native { .parts = ref parts } => (
                let mut raw_parts = ArrayList.new();
                let mut stmt = false;
                for part in parts |> ArrayList.iter do (
                    match part^ with (
                        | :Raw ref s => (
                            if s |> RcStr.as_str |> String.strip_prefix(.prefix = "stmt:") is :Some s then (
                                stmt = true;
                                let mut start = 0;
                                while (
                                    start < String.length(s)
                                    and String.at(s, start) |> Char.is_whitespace
                                ) do (
                                    start += Char.string_encoding_len(String.at(s, start));
                                );
                                let s = String.substring_from(s, start);
                                &mut raw_parts |> ArrayList.push_back({
                                    .shape = Rc.new(:Raw RcStr.allocate(s)),
                                });
                                continue;
                            );
                            if s |> RcStr.as_str |> String.strip_prefix(.prefix = "#include ") is :Some @"include" then (
                                &mut ctx^.result.includes |> OrdSet.add(RcStr.allocate(@"include"));
                                return void(span);
                            );
                            &mut raw_parts |> ArrayList.push_back({
                                .shape = Rc.new(:Raw s^),
                            });
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
                                { .shape = Rc.new(:Stmt block) }
                            );
                            &mut raw_parts |> ArrayList.push_back(part);
                        )
                    )
                );
                if stmt then (
                    insert_stmt(:RawParts raw_parts);
                    return void(span);
                );
                { .shape = Rc.new(:RawParts raw_parts) }
            )
            | :Literal ref literal => (
                return calculate_literal(literal, span)
            )
            | :Variant ref name => (
                return {
                    .expr = :Some {
                        .shape = Rc.new(:Ident enum_variant_name(&ir_expr^.ty, name)),
                    },
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
                insert_stmt(:For {
                    .init = :None,
                    .test = :None,
                    .incr = :None,
                    .body,
                });
                return void(span)
            )
            | :If {
                .cond = ref cond,
                .then_case = ref then_case,
                .else_case = ref else_case,
            } => (
                let ident = new_ident("if_result");
                let is_void = if ir_expr^.ty.shape^ is :Unit then true else false;
                if not is_void then (
                    insert_stmt(:LetVar {
                        .ty = convert_ty(&ir_expr^.ty),
                        .ident,
                        .value = :None,
                    });
                );
                let cond = pure(calculate(cond));
                let then_case = (
                    let mut block = new_block();
                    with Scope = {
                        .block = &mut block,
                    };
                    if calculate(then_case).expr is :Some result then (
                        insert_stmt(:Assign {
                            .assignee = { .shape = Rc.new(:Ident ident) },
                            .value = result,
                        });
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
                            insert_stmt(:Assign {
                                .assignee = { .shape = Rc.new(:Ident ident) },
                                .value = result,
                            });
                        );
                        :Some block
                    )
                );
                insert_stmt(:If { .cond, .then_case, .else_case });
                return {
                    .expr = if is_void then :None else :Some { .shape = Rc.new(:Ident ident) },
                    .span = span,
                }
            )
            | :Let { .name = ref name, .value = ref value } => (
                if value^.ty.shape^ is :Unit then (
                    calculate(value);
                    return void(span);
                );
                if value^.shape^ is :Uninitialized then (
                    insert_stmt(:LetVar {
                        .ty = convert_ty(&value^.ty),
                        .ident = ident(name),
                        .value = :None,
                    });
                ) else (
                    let_var(&value^.ty, ident(name), pure(calculate(value)));
                );
                return void(span)
            )
            | :LetContextRef ref new_ctx_ptr => (
                let ctx_var = new_ident("ctx");
                let_var(
                    &{
                        .shape = Rc.new(:Ref {
                            .shape = Rc.new(:ContextObject),
                            .alias_name = :None,
                        }),
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
                return with_scope(
                    { .block = (@current Scope).block },
                    () => calculate(expr)
                )
            )
            | :Apply { .f = ref f, .args = ref ir_args } => (
                let f_ty = match f^.ty.shape^ with (
                    | :Fn ty => ty
                    | _ => (
                        let diagnostic = {
                            .severity = :Error,
                            .source = :Internal,
                            .message = () => (
                                locked_output.write_str("Expected a fn type");
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
                    &mut args |> ArrayList.push_back({
                        .shape = Rc.new(:Field {
                            .obj = f,
                            .field = ident(&RcStr.allocate("captured")),
                        }),
                    });
                );
                let f = if f_ty.is_closure then {
                    .shape = Rc.new(:Field {
                        .obj = f,
                        .field = ident(&RcStr.allocate("f")),
                    })
                } else f;
                if f_ty.call_convention is :None then (
                    &mut args |> ArrayList.push_back({
                        .shape = Rc.new(:Ident (@current ScopeContextVar).ident),
                    });
                );
                for arg in ir_args |> ArrayList.iter do (
                    let arg = pure(calculate(arg));
                    &mut args |> ArrayList.push_back(arg);
                );
                if ir_expr^.ty.shape^ is :Unit then (
                    insert_stmt(:Expr {
                        .shape = Rc.new(:Apply { .f, .args }),
                    });
                    if f_ty.call_convention is :None then (
                        check_unwind();
                    );
                    return void(span)
                ) else (
                    let result_var = new_ident("apply_result");
                    let_var(&ir_expr^.ty, result_var, { .shape = Rc.new(:Apply { .f, .args }) });
                    if f_ty.call_convention is :None then (
                        check_unwind();
                    );
                    return {
                        .expr = :Some {
                            .shape = Rc.new(:Ident result_var),
                        },
                        .span,
                    }
                )
            )
            | :InjectContext { .name = ref name, .value = ref value_expr } => (
                let value = calculate(value_expr);
                if value.expr is :Some value then (
                    let ctx = current_context_place(name, .span);
                    let old_value_var = new_ident("old_ctx");
                    let_var(&value_expr^.ty, old_value_var, pure(ctx.get()));
                    ctx.set(value);
                    defer(() => ctx.set({
                        .shape = Rc.new(:Ident old_value_var),
                    }));
                );
                return void(span)
            )
            | :EnumIs { .enum = ref enum_expr, .variant = ref variant } => (
                return {
                    .expr = :Some {
                        .shape = Rc.new(:Equal {
                            pure(calculate(enum_expr)),
                            {
                                .shape = Rc.new(:Ident enum_variant_name(&enum_expr^.ty, variant)),
                            },
                        }),
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
                            .name = ident(&field^.name),
                            .value,
                        };
                        &mut field_initializers |> ArrayList.push_back(field);
                    );
                );
                {
                    .shape = Rc.new(:CompoundLiteral {
                        .ty = convert_ty(&ir_expr^.ty),
                        .fields = field_initializers,
                    }),
                }
            )
            | :Defer ref expr => (
                defer(() => (
                    calculate(expr);
                ));
                return void(span)
            )
            | :Unwind {
                .token = ref token,
                .value = ref value,
            } => (
                let token = pure(calculate(token));
                let value = calculate(value);
                if value.expr is :Some value then (
                    insert_stmt(:Assign {
                        .assignee = {
                            .shape = Rc.new(:Field {
                                .obj = {
                                    .shape = Rc.new(:Deref token),
                                },
                                .field = ident(&RcStr.allocate("value")),
                            }),
                        },
                        .value,
                    });
                );
                insert_stmt(:Assign {
                    .assignee = {
                        .shape = Rc.new(:Raw RcStr.allocate("current_unwinding_raw_token")),
                    },
                    .value = {
                        .shape = Rc.new(:Field {
                            .obj = {
                                .shape = Rc.new(:Deref token),
                            },
                            .field = ident(&RcStr.allocate("raw")),
                        }),
                    },
                });
                # TODO cleanup before return
                (
                    @current UnwindContext
                ).insert_unwind();
                # TODO what if its not void?
                return void(span)
            )
            | :Unwindable {
                .token_ty = ref token_ty,
                .token = ref token,
                .body = ref body,
            } => (
                let token_own_var = new_ident("token_own");
                let_var(
                    token_ty,
                    token_own_var,
                    {
                        .shape = Rc.new(:CompoundLiteral {
                            .ty = convert_ty(token_ty),
                            .fields = single_element_list({
                                .name = ident(&RcStr.allocate("raw")),
                                .value = {
                                    .shape = Rc.new(:Raw RcStr.allocate("RawUnwindToken_new_fn()")),
                                },
                            }),
                        }),
                    },
                );
                let token_var = ident(token);
                let_var(
                    &{
                        .shape = Rc.new(:Ref token_ty^),
                        .alias_name = :None,
                    },
                    token_var,
                    {
                        .shape = Rc.new(:Ref {
                            .shape = Rc.new(:Ident token_own_var),
                        }),
                    },
                );
                let have_result_value = match ir_expr^.ty.shape^ with (
                    | :Unit => false
                    | _ => true
                );
                let result_var = new_ident("unwindable_result");
                if have_result_value then (
                    insert_stmt(:LetVar {
                        .ty = convert_ty(&ir_expr^.ty),
                        .ident = result_var,
                        .value = :None,
                    });
                );
                let handle_unwind_label = new_ident("handle_unwind");
                let unwindable_body_label = new_ident("unwindable_body");
                let after_unwindable_label = new_ident("after_unwindable");
                insert_stmt(:Goto unwindable_body_label);
                insert_stmt(:GotoLabel handle_unwind_label);
                insert_stmt(:If {
                    .cond = {
                        .shape = Rc.new(:Apply {
                            .f = {
                                .shape = Rc.new(:Ident ident(&RcStr.allocate("is_unwinding_with_fn"))),
                            },
                            .args = single_element_list({
                                .shape = Rc.new(:Field {
                                    .obj = {
                                        .shape = Rc.new(:Ident token_own_var),
                                    },
                                    .field = ident(&RcStr.allocate("raw")),
                                }),
                            }),
                        }),
                    },
                    .then_case = (
                        let mut block = new_block();
                        with Scope = {
                            .block = &mut block,
                        };
                        insert_stmt(:Assign {
                            .assignee = {
                                .shape = Rc.new(:Ident ident(&RcStr.allocate("current_unwinding_raw_token"))),
                            },
                            .value = {
                                .shape = Rc.new(:Ident ident(&RcStr.allocate("NO_UNWIND"))),
                            },
                        });
                        if have_result_value then (
                            insert_stmt(:Assign {
                                .assignee = {
                                    .shape = Rc.new(:Ident result_var),
                                },
                                .value = {
                                    .shape = Rc.new(:Field {
                                        .obj = {
                                            .shape = Rc.new(:Ident token_own_var),
                                        },
                                        .field = ident(&RcStr.allocate("value")),
                                    }),
                                },
                            });
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
                });
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
                    insert_stmt(:Assign {
                        .assignee = {
                            .shape = Rc.new(:Ident result_var),
                        },
                        .value = pure(result),
                    });
                );
                insert_stmt(:GotoLabel after_unwindable_label);
                return {
                    .expr = if have_result_value then (
                        :Some {
                            .shape = Rc.new(:Ident result_var),
                        }
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
                .resume_fn = ref resume_fn,
                .token = ref token_name,
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
                        .ty = :Pointer Rc.new(:Struct ident(&RcStr.allocate("Context"))), # TODO maybe should own?
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
                &mut ctx^.result.types |> ArrayList.push_back(captured_decl);
                &mut ctx^.result.types |> ArrayList.push_back(captured_ty);
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
                        .args = single_element_list({ .name = coro, .ty = :Raw RcStr.allocate("mco_coro*") }),
                        .result_ty = convert_ty(&ir_expr^.ty),
                    },
                    .before_body = () => (
                        insert_stmt(:LetVar {
                            .ty = :Pointer Rc.new(captured_ty),
                            .ident = ident(&RcStr.allocate("captured")),
                            .value = :Some {
                                .shape = Rc.new(:Apply {
                                    .f = {
                                        .shape = Rc.new(:Ident ident(&RcStr.allocate("mco_get_user_data"))),
                                    },
                                    .args = single_element_list({
                                        .shape = Rc.new(:Ident coro),
                                    }),
                                }),
                            },
                        });
                        let_var(
                            &{
                                .shape = Rc.new(:Ref {
                                    .shape = Rc.new(:ContextObject),
                                    .alias_name = :None,
                                }),
                                .alias_name = :None,
                            },
                            ctx_var,
                            {
                                .shape = Rc.new(:Field {
                                    .obj = {
                                        .shape = Rc.new(:Deref {
                                            .shape = Rc.new(:Ident ident(&RcStr.allocate("captured"))),
                                        }),
                                    },
                                    .field = ctx_var,
                                }),
                            },
                        );
                        let_var(
                            token_ty,
                            token_name,
                            {
                                .shape = Rc.new(:Field {
                                    .obj = {
                                        .shape = Rc.new(:Deref {
                                            .shape = Rc.new(:Ident ident(&RcStr.allocate("captured"))),
                                        }),
                                    },
                                    .field = token_name,
                                }),
                            },
                        );
                    ),
                    .body,
                );
                &mut ctx^.result.fns
                    |> ArrayList.push_back({
                        .signature = {
                            .name = coro_fn,
                            .args = single_element_list({
                                .name = ident(&RcStr.allocate("co")),
                                .ty = :Raw RcStr.allocate("mco_coro*"),
                            }),
                            .result_ty = :Void,
                        },
                        .body = (
                            let mut block = new_block();
                            with Scope = { .block = &mut block };
                            let call :: Ast.Expr = {
                                .shape = Rc.new(:Apply {
                                    .f = {
                                        .shape = Rc.new(:Ident body_fn_ident),
                                    },
                                    .args = single_element_list({
                                        .shape = Rc.new(:Ident ident(&RcStr.allocate("co"))),
                                    }),
                                }),
                            };
                            if ir_expr^.ty.shape^ is :Unit then (
                                insert_stmt(:Expr call);
                            ) else (
                                insert_stmt(:LetVar {
                                    .ty = convert_ty(&ir_expr^.ty),
                                    .ident = ident(&RcStr.allocate("result")),
                                    .value = :Some call,
                                });
                                insert_mco_stmt({
                                    .shape = Rc.new(:Apply {
                                        .f = {
                                            .shape = Rc.new(:Ident ident(&RcStr.allocate("mco_push"))),
                                        },
                                        .args = (
                                            let mut args = ArrayList.new();
                                            &mut args |> ArrayList.push_back({
                                                .shape = Rc.new(:Ident ident(&RcStr.allocate("co"))),
                                            });
                                            &mut args |> ArrayList.push_back({
                                                .shape = Rc.new(:Ref {
                                                    .shape = Rc.new(:Ident ident(&RcStr.allocate("result"))),
                                                }),
                                            });
                                            &mut args |> ArrayList.push_back({
                                                .shape = Rc.new(:Apply {
                                                    .f = {
                                                        .shape = Rc.new(:Ident ident(&RcStr.allocate("sizeof"))),
                                                    },
                                                    .args = single_element_list({
                                                        .shape = Rc.new(:Ident ident(&RcStr.allocate("result"))),
                                                    }),
                                                }),
                                            });
                                            args
                                        ),
                                    }),
                                });
                            );
                            block
                        ),
                    });
                let desc = new_ident("desc");
                insert_stmt(:LetVar {
                    .ty = :Raw RcStr.allocate("mco_desc"),
                    .ident = desc,
                    .value = :Some {
                        .shape = Rc.new(:Apply {
                            .f = {
                                .shape = Rc.new(:Raw RcStr.allocate("mco_desc_init")),
                            },
                            .args = (
                                let mut args = ArrayList.new();
                                &mut args |> ArrayList.push_back({
                                    .shape = Rc.new(:Ident coro_fn),
                                });
                                &mut args |> ArrayList.push_back({
                                    .shape = Rc.new(:Literal :Int RcStr.allocate("0")),
                                });
                                args
                            ),
                        }),
                    },
                });
                insert_stmt(:Assign {
                    .assignee = {
                        .shape = Rc.new(:Field {
                            .obj = {
                                .shape = Rc.new(:Ident desc),
                            },
                            .field = ident(&RcStr.allocate("user_data")),
                        }),
                    },
                    .value = {
                        .shape = Rc.new(:Ident captured_var),
                    },
                });
                insert_stmt(:Assign {
                    .assignee = {
                        .shape = Rc.new(:Field {
                            .obj = {
                                .shape = Rc.new(:Deref {
                                    .shape = Rc.new(:Ident captured_var),
                                }),
                            },
                            .field = ctx_var,
                        }),
                    },
                    .value = {
                        .shape = Rc.new(:Ident (@current ScopeContextVar).ident),
                    },
                });
                insert_stmt(:LetVar {
                    .ty = :Raw RcStr.allocate("mco_coro*"),
                    .ident = coro,
                    .value = :None,
                });
                insert_mco_stmt({
                    .shape = Rc.new(:Apply {
                        .f = { .shape = Rc.new(:Raw RcStr.allocate("mco_create")) },
                        .args = (
                            let mut args = ArrayList.new();
                            &mut args |> ArrayList.push_back({
                                .shape = Rc.new(:Ref {
                                    .shape = Rc.new(:Ident coro),
                                }),
                            });
                            &mut args |> ArrayList.push_back({
                                .shape = Rc.new(:Ref {
                                    .shape = Rc.new(:Ident desc),
                                }),
                            });
                            args
                        ),
                    }),
                });
                insert_stmt(:Assign {
                    .assignee = {
                        .shape = Rc.new(:Field {
                            .obj = {
                                .shape = Rc.new(:Deref {
                                    .shape = Rc.new(:Ident captured_var),
                                }),
                            },
                            .field = token_name,
                        }),
                    },
                    .value = {
                        .shape = Rc.new(:CompoundLiteral {
                            .ty = convert_ty(token_ty),
                            .fields = (
                                let mut fields = ArrayList.new();
                                let coro_field = {
                                    .name = ident(&RcStr.allocate("coro")),
                                    .value = { .shape = Rc.new(:Ident coro) },
                                };
                                &mut fields |> ArrayList.push_back(coro_field);
                                fields
                            ),
                        }),
                    },
                });
                let shape = :Apply {
                    .f = {
                        .shape = Rc.new(:Ident ident(&RcStr.from_String(format!("\(resume_fn^)_fn")))),
                    },
                    .args = single_element_list({
                        .shape = Rc.new(:Field {
                            .obj = {
                                .shape = Rc.new(:Deref {
                                    .shape = Rc.new(:Ident captured_var),
                                }),
                            },
                            .field = token_name,
                        }),
                    }),
                };
                { .shape = Rc.new(shape) }
            )
            | :CaptureContinuation {
                .token = ref token,
                .continuation_ty_repr = ref continuation_ty_repr,
                .continuation = ref continuation_name,
                .resume_fn = ref resume_fn,
                .body = ref body,
            } => (
                make_sure_type_is_complete(continuation_ty_repr);
                let continuation_name = ident(continuation_name);
                let token = pure(calculate(token));
                let coro :: Ast.Expr = {
                    .shape = Rc.new(:Field {
                        .obj = token,
                        .field = ident(&RcStr.allocate("coro")),
                    }),
                };
                let_var(
                    continuation_ty_repr,
                    continuation_name,
                    {
                        .shape = Rc.new(:CompoundLiteral {
                            .ty = convert_ty(continuation_ty_repr),
                            .fields = (
                                let mut fields = ArrayList.new();
                                let token_field = {
                                    .name = ident(&RcStr.allocate("token")),
                                    .value = token,
                                };
                                &mut fields |> ArrayList.push_back(token_field);
                                let f_field = {
                                    .name = ident(&RcStr.allocate("f")),
                                    .value = {
                                        .shape = Rc.new(:Ident ident(&RcStr.from_String(format!("\(resume_fn^)_fn")))),
                                    },
                                };
                                &mut fields |> ArrayList.push_back(f_field);
                                fields
                            ),
                        }),
                    },
                );
                if body^.ty.shape^ is :Unit then (
                    calculate(body);
                ) else (
                    let result_var = new_ident("result");
                    let_var(&body^.ty, result_var, pure(calculate(body)));
                    insert_mco_stmt({
                        .shape = Rc.new(:Apply {
                            .f = {
                                .shape = Rc.new(:Ident ident(&RcStr.allocate("mco_push"))),
                            },
                            .args = (
                                let mut args = ArrayList.new();
                                &mut args |> ArrayList.push_back(coro);
                                &mut args |> ArrayList.push_back({
                                    .shape = Rc.new(:Ref {
                                        .shape = Rc.new(:Ident result_var),
                                    }),
                                });
                                &mut args |> ArrayList.push_back({
                                    .shape = Rc.new(:Apply {
                                        .f = {
                                            .shape = Rc.new(:Ident ident(&RcStr.allocate("sizeof"))),
                                        },
                                        .args = single_element_list({
                                            .shape = Rc.new(:Ident result_var),
                                        }),
                                    }),
                                });
                                args
                            ),
                        }),
                    });
                );
                insert_mco_stmt({
                    .shape = Rc.new(:Apply {
                        .f = {
                            .shape = Rc.new(:Ident ident(&RcStr.allocate("mco_yield"))),
                        },
                        .args = single_element_list(coro),
                    }),
                });
                return {
                    .expr = :Some {
                        .shape = Rc.new(:Ident ident(&RcStr.allocate("TODO"))),
                    },
                    .span,
                }
            )
            | :ConstructTypeInfo ref ty => construct_type_info(ty)
            | :Fn ref fn_def => (
                make_sure_type_is_complete(&ir_expr^.ty);
                let name = new_ident("closure");
                add_fn(&name.name, fn_def);
                let { f :: Ast.ExprShape, captured :: Ast.ExprShape } = if (
                    &fn_def^.captures |> OrdMap.length != 0
                ) then (
                    let captured_var = new_ident("captured");
                    let captured_ty = :Named (
                        &ctx^.fn_capture_types
                            |> OrdMap.get(&name.name)
                            |> Option.unwrap_or_else(() => panic("captured ty not initialized for fn"))
                    )^;
                    construct_captured(
                        captured_var,
                        captured_ty,
                        fn_def^.capture_mode,
                        &fn_def^.captures,
                        .span,
                    );
                    {
                        :Ident ident(&RcStr.from_String(format!("\(name.name)_fn"))),
                        :Ident captured_var,
                    }
                ) else (
                    {
                        :Ident ident(&RcStr.from_String(format!("\(name.name)_as_closure"))),
                        :Ident ident(&RcStr.allocate("NULL")),
                    }
                );
                let f = { .shape = Rc.new(f) };
                let captured = { .shape = Rc.new(captured) };
                return {
                    .expr = :Some {
                        .shape = Rc.new(:CompoundLiteral {
                            .ty = convert_ty(&ir_expr^.ty),
                            .fields = (
                                let mut fields = ArrayList.new();
                                let captured_field = {
                                    .name = ident(&RcStr.allocate("captured")),
                                    .value = captured,
                                };
                                &mut fields |> ArrayList.push_back(captured_field);
                                let f_field = {
                                    .name = ident(&RcStr.allocate("f")),
                                    .value = f,
                                };
                                &mut fields |> ArrayList.push_back(f_field);
                                fields
                            )
                        }),
                    },
                    .span,
                }
            )
        );
        if ir_expr^.ty.shape^ is :Unit then (
            insert_stmt(:Expr expr);
            void(span)
        ) else (
            let ident = new_ident("value");
            let_var(&ir_expr^.ty, ident, expr);
            {
                .expr = :Some {
                    .shape = Rc.new(:Ident ident),
                },
                .span = span,
            }
        )
    );

    const let_var = (ty :: &Ir.Type, ident :: Ast.Ident, value :: Ast.Expr) => (
        insert_stmt(:LetVar {
            .ty = convert_ty(ty),
            .ident,
            .value = :Some value,
        });
    );

    const insert_stmt = (stmt :: Ast.StmtShape) => (
        let stmt = { .shape = Rc.new(stmt) };
        &mut (@current Scope).block^.stmts |> ArrayList.push_back(stmt);
    );

    const insert_mco_stmt = (expr :: Ast.Expr) => (
        let result_var = new_ident("mco_result");
        insert_stmt(:LetVar {
            .ty = :Raw RcStr.allocate("mco_result"),
            .ident = result_var,
            .value = :Some expr,
        });
        insert_stmt(:Expr {
            .shape = Rc.new(:Apply {
                .f = {
                    .shape = Rc.new(:Ident ident(&RcStr.allocate("assert"))),
                },
                .args = (
                    let mut args = ArrayList.new();
                    let equals = {
                        .shape = Rc.new(:Equal {
                            { .shape = Rc.new(:Ident result_var) },
                            { .shape = Rc.new(:Ident ident(&RcStr.allocate("MCO_SUCCESS"))) },
                        }),
                    };
                    &mut args |> ArrayList.push_back(equals);
                    args
                ),
            }),
        });
    );

    const new_block = () -> Ast.Block => {
        .stmts = ArrayList.new(),
    };

    const construct_captured = (
        captured_var :: Ast.Ident,
        captured_ty :: Ast.Ty,
        capture_mode :: Ir.CaptureMode,
        captures :: &OrdMap.t[RcStr, Ir.Type],
        .span :: Span,
    ) => (
        insert_stmt(:LetVar {
            .ty = :Pointer Rc.new(captured_ty),
            .ident = captured_var,
            .value = :Some {
                .shape = Rc.new(:Raw RcStr.from_String(output_to_string(() => (
                    locked_output.write_str("malloc_or_panic_fn(sizeof(");
                    Print.ty(&captured_ty);
                    locked_output.write_str("))");
                )))),
            },
        });
        insert_stmt(:Assign {
            .assignee = {
                .shape = Rc.new(:Deref {
                    .shape = Rc.new(:Ident captured_var),
                }),
            },
            .value = {
                .shape = Rc.new(:CompoundLiteral {
                    .ty = captured_ty,
                    .fields = (
                        let mut fields = ArrayList.new();
                        for &{ .key = ref name, .value = ref ty } in captures |> OrdMap.iter do (
                            let value = pure(find_ident(ty, name, .span).get());
                            let value = match capture_mode with (
                                | :Move => value
                                | :ByRef => {
                                    .shape = Rc.new(:Ref value),
                                }
                            );
                            let field = {
                                .name = ident(name),
                                .value = value,
                            };
                            &mut fields |> ArrayList.push_back(field);
                        );
                        fields
                    ),
                }),
            },
        });
    );

    const captured_to_field_defs = (
        capture_mode :: Ir.CaptureMode,
        captures :: &OrdMap.t[RcStr, Ir.Type],
    ) -> ArrayList.t[Ast.FieldDef] => (
        let mut fields = ArrayList.new();
        for &{ .key = ref name, .value = ref ty } in captures |> OrdMap.iter do (
            make_sure_type_is_complete(ty);
            let ty = convert_ty(ty);
            let ty = match capture_mode with (
                | :Move => ty
                | :ByRef => :Pointer Rc.new(ty)
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
        .captures :: &OrdMap.t[RcStr, Ir.Type],
        .signature :: Ast.FnSignature,
        .before_body :: () -> (),
        .body :: &Ir.Expr,
    ) => (
        let ctx = @current Context;
        with FunctionContext = {
            .capture_mode,
            .captures,
            # TODO
            .result_ty = body^.ty,
        };
        with UnwindContext = {
            .insert_unwind = () => (
                match uninitialized(
                    &body^.ty,
                    .span = {
                        .start = Position.beginning(),
                        .end = Position.beginning(),
                        .path = :Special RcStr.allocate(__FILE__)
                    },
                ).expr with (
                    | :Some expr => insert_stmt(:Return expr)
                    | :None => insert_stmt(:ReturnVoid)
                );
            ),
            .cleanup_scope_without_unwind = () => (),
        };
        let f :: Ast.Fn = {
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
        &mut ctx^.result.fns |> ArrayList.push_back(f);
    );

    const add_fn = (
        name :: &RcStr,
        def :: &Ir.FnDef,
    ) => (
        let fn_name = RcStr.from_String(format!("\(name^)_fn"));
        let ctx = @current Context;
        let mut args = ArrayList.new();
        if &def^.captures |> OrdMap.length != 0 then (
            let ty_def = {
                .name = ident(&RcStr.from_String(format!("\(fn_name)_captured"))),
                .def = :Struct {
                    .fields = captured_to_field_defs(def^.capture_mode, &def^.captures),
                },
            };
            let ty_decl = {
                .name = ty_def.name,
                .def = :Alias :Struct ty_def.name,
            };
            &mut ctx^.result.types |> ArrayList.push_back(ty_decl);
            &mut ctx^.fn_capture_types |> OrdMap.add(name^, ty_def.name);
            &mut ctx^.result.types |> ArrayList.push_back(ty_def);
            &mut args
                |> ArrayList.push_back({
                    .name = ident(&RcStr.allocate("captured_void")),
                    .ty = :Pointer Rc.new(:Void),
                });
        );
        let ctx_var = new_ident("ctx");
        if def^.call_convention is :None then (
            &mut args
                |> ArrayList.push_back({
                    .name = ctx_var,
                    .ty = :Pointer Rc.new(:Struct ident(&RcStr.allocate("Context"))),
                });
        );
        for arg in &def^.args |> ArrayList.iter do (
            make_sure_type_is_complete(&arg^.ty);
            let arg :: Ast.FnArg = {
                .name = ident(&arg^.name),
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
                .name = ident(&fn_name),
                .args,
                .result_ty = convert_ty(&def^.result_ty),
            },
            .before_body = () => (
                if &ctx^.fn_capture_types |> OrdMap.get(name) is :Some &captured_ty then (
                    insert_stmt(:LetVar {
                        .ty = :Pointer Rc.new(:Named captured_ty),
                        .ident = ident(&RcStr.allocate("captured")),
                        .value = :Some {
                            .shape = Rc.new(:Ident ident(&RcStr.allocate("captured_void"))),
                        },
                    });
                );
            ),
            .body = &def^.body,
        );
        if &def^.captures |> OrdMap.length == 0 then (
            let fn_as_closure :: Ast.Fn = {
                .signature = {
                    .name = ident(&RcStr.from_String(format!("\(name^)_as_closure"))),
                    .args = (
                        let mut args_as_closure = ArrayList.new();
                        &mut args_as_closure
                            |> ArrayList.push_back({
                                .name = ident(&RcStr.allocate("captured_nothing")),
                                .ty = :Pointer Rc.new(:Void),
                            });
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
                    insert_stmt(:Return {
                        .shape = Rc.new(:Apply {
                            .f = { .shape = Rc.new(:Ident ident(&fn_name)) },
                            .args = (
                                let mut call_args = ArrayList.new();
                                for arg in &args |> ArrayList.iter do (
                                    &mut call_args |> ArrayList.push_back({
                                        .shape = Rc.new(:Ident arg^.name),
                                    });
                                );
                                call_args
                            )
                        }),
                    });
                    block
                ),
            };
            &mut ctx^.result.fns |> ArrayList.push_back(fn_as_closure);
            let fn_ty :: Ir.FnType = {
                .is_closure = true,
                # TODO
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
            let fn_ty :: Ir.Type = { .shape = Rc.new(:Fn fn_ty), .alias_name = :None };
            make_sure_type_is_complete(&fn_ty);
            let static = {
                .@"const" = true,
                .ty = convert_ty(&fn_ty),
                .name = ident(name),
                .value = :Some {
                    .shape = Rc.new(:CompoundLiteral {
                        .ty = convert_ty(&fn_ty),
                        .fields = (
                            let mut fields = ArrayList.new();
                            let captured_field = {
                                .name = ident(&RcStr.allocate("captured")),
                                .value = {
                                    .shape = Rc.new(:Ident ident(&RcStr.allocate("NULL"))),
                                },
                            };
                            &mut fields |> ArrayList.push_back(captured_field);
                            let f_field = {
                                .name = ident(&RcStr.allocate("f")),
                                .value = {
                                    .shape = Rc.new(:Ident fn_as_closure.signature.name),
                                },
                            };
                            &mut fields |> ArrayList.push_back(f_field);
                            fields
                        )
                    }),
                },
            };
            &mut ctx^.result.statics |> ArrayList.push_back(static);
        );
    );

    const make_sure_type_is_declared = (ty :: &Ir.Type) => with_return (
        if ty^.alias_name is :Some ref name then (
            let def = &(@current Context)^.program.types
                |> OrdMap.get(name)
                |> Option.unwrap;
            type_def(name, def);
            return;
        );
        match Ir.type_repr(ty)^.shape^ with (
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
            | :Named ref name => (
                let def = &(@current Context)^.program.types
                    |> OrdMap.get(name)
                    |> Option.unwrap;
                type_decl(name, def);
            )
            | :Fn ref fn_ty => (
                fn_type_ident(fn_ty);
            )
            | :Native _ => ()
            | :ContextObject => (
                let ctx_decl = {
                    .name = ident(&RcStr.allocate("Context")),
                    .def = :Alias :Struct ident(&RcStr.allocate("Context")),
                };
                &mut (@current Context)^.result.types
                    |> ArrayList.push_back(ctx_decl);
            )
        )
    );

    const make_sure_type_is_complete = (ty :: &Ir.Type) => (
        if ty^.alias_name is :Some ref name then (
            let def = &(@current Context)^.program.types
                |> OrdMap.get(name)
                |> Option.unwrap;
            type_def(name, def);
        );
        match Ir.type_repr(ty)^.shape^ with (
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
            | :Named ref name => (
                let def = &(@current Context)^.program.types
                    |> OrdMap.get(name)
                    |> Option.unwrap;
                type_def(name, def);
            )
            | :Fn ref fn_ty => (
                fn_type_ident(fn_ty);
            )
            | :Native _ => ()
            | :ContextObject => (
                let ctx = @current Context;
                let mut ctx_fields = ArrayList.new();
                for &{ .key = ref name, .value = ref ty } in &ctx^.program.contexts |> OrdMap.iter do (
                    make_sure_type_is_complete(ty);
                    let field = {
                        .name = ident(name),
                        .ty = convert_ty(ty),
                    };
                    &mut ctx_fields |> ArrayList.push_back(field);
                );
                let ctx_def = {
                    .name = ident(&RcStr.allocate("Context")),
                    .def = :Struct { .fields = ctx_fields },
                };
                &mut ctx^.result.types |> ArrayList.push_back(ctx_def);
            )
        )
    );

    const enum_variant_name = (enum_ty :: &Ir.Type, variant :: &RcStr) -> Ast.Ident => (
        let s = output_to_string(() => (
            Ir.Print.type_name(enum_ty);
            locked_output.write_str("_");
            locked_output.write(variant);
        ));
        ident(&RcStr.from_String(s))
    );

    const type_decl = (name :: &RcStr, def :: &Ir.TypeDef) => with_return (
        let ctx = @current Context;
        if &ctx^.declared_types |> OrdSet.contains(name) then (
            return;
        );
        &mut ctx^.declared_types |> OrdSet.add(name^);
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
        &mut ctx^.result.types |> ArrayList.push_back(def);
    );

    const type_def = (name :: &RcStr, def :: &Ir.TypeDef) => with_return (
        type_decl(name, def);
        let mut ctx = @current Context;
        if &ctx^.defined_types |> OrdSet.contains(name) then (
            return;
        );
        &mut ctx^.defined_types |> OrdSet.add(name^);
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
            | :Opaque => :Alias :Pointer Rc.new(:Void)
            | :Enum { .variants = ref variants } => :Enum (
                let mut idents = ArrayList.new();
                for variant in variants |> OrdSet.iter do (
                    &mut idents
                        |> ArrayList.push_back(
                            enum_variant_name(
                                &{
                                    .shape = Rc.new(:Named name^),
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
                for &{ .key = ref name, .value = ref ty } in variants |> OrdMap.iter do (
                    if ty^.shape^ is :Unit then (
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
                for &{ .key = ref name, .value = ref ty } in fields |> OrdMap.iter do (
                    if ty^.shape^ is :Unit then (
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
        &mut ctx^.result.types |> ArrayList.push_back(def);
    );

    const compile = (program :: Ir.Program) -> Compiled => (
        let mut ctx = {
            .declared_types = OrdSet.new(),
            .defined_types = OrdSet.new(),
            .fn_types = OrdMap.new(),
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
        with Context = &mut ctx;
        let ctx = @current Context;
        # for int32_t and similar
        &mut ctx^.result.includes
            |> OrdSet.add(RcStr.allocate("<stdint.h>"));
        &mut ctx^.result.includes |> OrdSet.add(RcStr.allocate("<sys/types.h>"));
        &mut ctx^.result.includes |> OrdSet.add(RcStr.allocate("<sys/types.h>"));
        &mut ctx^.result.includes |> OrdSet.add(RcStr.allocate("<stdalign.h>"));
        &mut ctx^.result.includes |> OrdSet.add(RcStr.allocate("<stddef.h>"));
        &mut ctx^.result.includes |> OrdSet.add(RcStr.allocate("<stdbool.h>"));
        &mut ctx^.result.includes |> OrdSet.add(RcStr.allocate("<assert.h>"));
        &mut ctx^.result.includes |> OrdSet.add(RcStr.allocate("<stdlib.h>"));
        make_sure_type_is_complete(
            &{ .shape = Rc.new(:ContextObject), .alias_name = :None }
        );
        for &{ .key = ref name, .value = ref ty_def } in &ctx^.program.types |> OrdMap.iter do (
            type_def(name, ty_def);
        );
        for &{ .key = ref name, .value = ref def } in &ctx^.program.fns |> OrdMap.iter do (
            add_fn(name, def);
        );

        (
            # consts
            let mut args = ArrayList.new();
            let ctx_var = new_ident("ctx");
            &mut args
                |> ArrayList.push_back({
                    .name = ctx_var,
                    .ty = :Pointer Rc.new(:Named ident(&RcStr.allocate("Context"))),
                });
            let no_captures = OrdMap.new();
            with FunctionContext = {
                .capture_mode = :ByRef,
                .captures = &no_captures,
                .result_ty = { .shape = Rc.new(:Unit), .alias_name = :None },
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
            for name in &ctx^.program.consts_order |> ArrayList.iter do (
                let value = &ctx^.program.consts |> OrdMap.get(name) |> Option.unwrap;
                let ty = &value^.ty;
                let value = calculate(value);
                if value.expr is :Some value then (
                    let static = {
                        .@"const" = false,
                        .ty = convert_ty(ty),
                        .name = ident(name),
                        .value = :None,
                    };
                    &mut ctx^.result.statics |> ArrayList.push_back(static);
                    insert_stmt(:Assign {
                        .assignee = {
                            .shape = Rc.new(:Ident ident(name)),
                        },
                        .value,
                    });
                );
            );
            let f :: Ast.Fn = {
                .signature = {
                    .name = ident(&RcStr.allocate("init_consts")),
                    .args,
                    .result_ty = :Void,
                },
                .body,
            };
            (@current UnwindContext).cleanup_scope_without_unwind();
            &mut ctx^.result.fns |> ArrayList.push_back(f);
        );
        ctx^.result
    );
);
