# this goes into std

syntax for_loop <- 2 = "for" value_pattern "in" generator "{" body "}";

const generator_context = forall (~Yield :: type, ~Resume :: type). (
	yield: Yield -> Resume,
);

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
	const value_type = string; # todo infer
	delimited_block (
		handler: (value : $value_pattern :: value_type, ~resume) => (
			$body;
			let _ :: void = resume(void);
		),
		body: (token :: delimited_token) => (
			let context :: generator_context[Yield: value_type, Resume: void] = (
				(yield: value => builtin_fn_delimited_yield (~token, ~value))
			);
			with context (
				let _ :: void = $generator;
			)
		),
	);
);

const yield = forall (~T :: type, ~Resume :: type). (
	fn (value :: T) {
		(current generator_context[Yield: T, ~Resume]).yield(value)
	}
);

# actual example
# TODO mark generator with 

let generator = fn(void) {
	print "yielding 1";
	yield[T: string, Resume: void] "1";
	print "yielding 2";
	yield "2";
};

for value in generator () {
	print value;
}
