# this goes into std

syntax for_loop <- 2 = "for" value_pattern "in" generator "{" body "}";

const generator_context = forall (~Yield :: type, ~Resume :: type, ~Finish :: type). (
	yield: Yield -> Resume,
	finish: Finish -> never,
);

dbg generator_context[Yield: void, Resume: void, Finish: void];

const delimited_block = forall (~YieldH :: type, ~Resume :: type, ~Finish :: type). (
	const Args = (
		handler: (value: YieldH, resume: Resume -> Finish) -> Finish,
		body: delimited_token -> Finish,
	);
	fn (args :: Args) -> Finish {
		builtin_fn_delimited_block args
	}
);

const for_loop = macro (~value_pattern, ~generator, ~body) => `(
	delimited_block (
		handler: (value : $value_pattern, ~resume) => (
			$body;
			let _ :: void = resume(void);
		),
		body: (token :: delimited_token) => (
			let context :: generator_context[Yield: _, Resume: void, Finish: void] = (
				(yield: value => delimited_switch (~token, ~value))
			);
			with context (
				let _ :: void = $generator;
			)
		),
	);
);

# actual example

# TODO mark generator with yielding[string]

let generator = fn(void) {
	yield "1";
	yield "2";
};

for value in generator () {
	print value;
}
