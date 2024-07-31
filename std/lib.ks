module:

# BrainoidGames: do you have type casting yet? if not, can we name it kast()?

const unwind_token = builtin "unwind_token";
const delimited_token = builtin "delimited_token";
const void = builtin "void";
const ast = builtin "ast";
const bool = builtin "bool";
const true = builtin "true";
const false = builtin "false";
const int32 = builtin "int32";
const int64 = builtin "int64";
const float32 = builtin "float32";
const float64 = builtin "float64";
const string = builtin "string";
const never = builtin "never";
const type = builtin "type";

const Option = forall (T :: type). (Some of T | None ofnone);
const Either = forall ((~left, ~right) :: ( left: type, right: type )). (Left of left | Right of right);
const Result = forall ((~ok, ~error) :: ( ok: type, error: type)). (Ok of ok | Error of error);

# args |> f
const pipe = macro (~f, ~args) => `((
    $f $args
));

const input :: void -> string = builtin "fn input";
const print :: string -> void = builtin "fn print";

const loop_context :: type = (
    finish_current_iteration: (bool -> never), # todo should be option[T] -> never
) as type;

# todo
# with = forall (arg :: type, result :: type, old_context :: type, new_context :: type).
# fn (body :: arg -> result with (old_context and new_context)) old_context {
# body arg
# }

const dbg = forall (T :: type). (
    builtin "fn dbg" :: T -> void
);

const unwind = forall (T :: type). (
    builtin "fn unwind" :: (token: unwind_token, value: T) -> never
);

let loop_fn = fn (body :: (void -> void with loop_context)) {
    let should_continue = unwindable_block fn(token :: unwind_token) {
        let current_loop_context = (
            finish_current_iteration: (x :: bool) -> never => unwind ( ~token, value: x ),
        );
        with current_loop_context (
            body ();
            true
        )
    };
    if should_continue then (loop_fn body);
};

const do_break = fn (value :: void) loop_context {
    (current loop_context).finish_current_iteration false
};

const do_continue = fn (void) loop_context {
    (current loop_context).finish_current_iteration true
};

const continue_impl = macro _ => (
    `(do_continue ())
);

const break_without_value = macro _ => `(do_break void);
const break_with_value = macro (~value) => `(do_break $value);

const throws = forall (error :: type). (throw: (error -> never));
const throw = forall (error :: type). (
	fn (e :: error) {
		(current throws[error]).throw e
	}
);

const do_try = forall
		(~ok :: type, ~error :: type). (
	fn (body :: (void -> ok with throws[error])) {
		unwindable_block fn(token :: unwind_token) {
			const result_type = Result[~ok, ~error];
			let throw_context = throw: (e :: error => unwind (~token, value: result_type.Error of e));
			with throw_context (
			 	result_type.Ok of (body ())
			)
		}
	}
);

const try_explicit = macro (~targs, ~expr) => `(
    let (~ok, ~error) = $targs;
    let f :: void -> ok with throws[error] = () => $expr;
    do_try[$targs] f
);

const try_implicit = macro (~expr) => `(
    do_try[_] (() => $expr)
);


const catch_impl = macro (~expr :: ast, ~e :: ast, ~catch_block :: ast) => `(
	match $expr (
		| Ok of value => value
		| Error of $e => $catch_block
	)
);

const is_same_type :: (a: type, b: type) -> bool = builtin "fn is_same_type";

const panic :: string -> never = builtin "fn panic";

const random = forall (T :: type). (
    (
    if is_same_type (a: T, b: int32) then
        builtin "fn random_int32"
    else (if is_same_type (a: T, b: float64) then
        builtin "fn random_float64"
    else
        panic "wtf")
    ) :: (min: T, max: T) -> T
);

const TypeName :: type = (
    name: string
) as type;

impl TypeName for void as (
    name: "void",
);

impl TypeName for type as (
    name: "type",
);

impl TypeName for bool as (
    name: "bool",
);

impl TypeName for int32 as (
    name: "int32",
);

impl TypeName for int64 as (
    name: "int64",
);

impl TypeName for float32 as (
    name: "float32",
);

impl TypeName for float64 as (
    name: "float64",
);

impl TypeName for string as (
    name: "string",
);

const type_name = forall (T :: type) where (T impl TypeName). (
    (T as TypeName).name
);

const Eq = forall (T :: type). (
    eq: (lhs: T, rhs: T) -> bool
);

impl Eq for int32 as (
    eq: builtin "fn =="
);

impl Eq for string as (
    eq: builtin "fn =="
);

const @"op binary ==" = macro (~lhs, ~rhs) => `(
    (_ as Eq).eq(lhs: $lhs, rhs: $rhs)
);

const @"op binary +" :: (lhs: int32, rhs: int32) -> int32 = builtin "fn binary +";
const @"op binary -" :: (lhs: int32, rhs: int32) -> int32 = builtin "fn binary -";

const @"op binary <" :: (lhs: int32, rhs: int32) -> bool = builtin "fn <";
const @"op binary >" :: (lhs: int32, rhs: int32) -> bool = builtin "fn >";

const Parse = forall (Self :: type). (
    parse: (string -> Self),
);

impl Parse for int32 as (
    parse: builtin "fn string_to_int32",
);

impl Parse for float64 as (
    parse: builtin "fn string_to_float64",
);

const parse = forall (T :: type) where (T impl Parse). (
    (T as Parse).parse
);

const sin :: float64 -> float64 = builtin "fn sin";

const yields = forall (~Yield :: type, ~Resume :: type). (
	yield: Yield -> Resume,
);

const delimited_block = forall (~Yield :: type, ~Resume :: type, ~Finish :: type). (
	const Args = (
		handler: (value: Yield, resume: Resume -> Finish) -> Finish,
		body: delimited_token -> Finish,
	);
	builtin "fn delimited_block" :: Args -> Finish
);

const loop_impl = macro (~body) => `(
    loop_fn (fn (void) { $body })
);

const for_loop = macro (~value_pattern, ~generator, ~body) => `(
	const value_type = string; # todo infer
    let context :: yields[Yield: value_type, Resume: void] = (
		(yield: fn ($value_pattern) {
            $body
        })
    );
    with context (
        $generator
    )
);

const GeneratorNext = forall (~Yield :: type, ~Finish :: type). (
    | Yielded of Yield | Finished of Finish
);

const Generator = forall (~Yield :: type, ~Resume :: type, ~Finish :: type). (
    next: Resume -> GeneratorNext[~Yield, ~Finish],
);

const GeneratorState = forall (~Yield :: type, ~Resume :: type, ~Finish :: type). (
    | NotStarted of (() -> Finish with yields[~Yield, ~Resume])
    | Suspended of (Resume -> GeneratorNext[~Yield, ~Finish] with yields[~Yield, ~Resume])
    | Finished ofnone
);

const generator_value = forall (~Yield :: type, ~Resume :: type, ~Finish :: type). (
    fn (generator :: () -> Finish with yields[~Yield, ~Resume]) -> Generator[~Yield, ~Resume, ~Finish] {
        let mut state = GeneratorState[~Yield, ~Resume, ~Finish].NotStarted of generator;
        next: fn(resume_value :: Resume) -> GeneratorNext[~Yield, ~Finish] {
            match state {
                # if none then resume_value is silently ignored?
                | NotStarted of generator => (
                    let handler = fn(~value :: Yield, ~resume :: Resume -> GeneratorNext[~Yield, ~Finish]) -> GeneratorNext[~Yield, ~Finish] {
                        state = GeneratorState[~Yield, ~Resume, ~Finish].Suspended of resume;
                        GeneratorNext[~Yield, ~Finish].Yielded of value
                    };
                    delimited_block (
                        ~handler,
                        body: fn(token :: delimited_token) -> GeneratorNext[~Yield, ~Finish] {
                            let context :: yields[~Yield, ~Resume] = (
                                yield: fn (value :: Yield) -> Resume {
                                    builtin "fn delimited_yield" (~token, ~value)
                                },
                            );
                            let final_value :: Finish = with context (
                                generator()
                            );
                            state = GeneratorState[~Yield, ~Resume, ~Finish].Finished ofnone;
                            GeneratorNext[~Yield, ~Finish].Finished of final_value
                        },
                    )
                )
                | Suspended of resume => (
                    resume(resume_value)
                )
                | Finished ofnone => (
                    panic "generator is finished"
                )
            }
        }
    }
);

const yield = forall (~T :: type, ~Resume :: type). (
	fn (value) {
		(current yields[Yield: T, ~Resume]).yield(value)
	} :: T -> Resume
);

const compile_to_js = forall (T :: type). (
    builtin "fn compile_to_js" :: T -> string
);
