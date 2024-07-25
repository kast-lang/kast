const test = forall (T :: type). (
	dbg T;
	(f: T -> void)
);

dbg test[int32];

# const generator_context = forall (~Yield :: type, ~Resume :: type, ~Finish :: type). (
# 	yield: Yield -> Resume,
# 	finish: Finish -> never,
# );
# 
# dbg generator_context[Yield: void, Resume: void, Finish: void];
