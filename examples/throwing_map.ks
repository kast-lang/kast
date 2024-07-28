# array.iter().map(|x| x.fallible()?).collect()

syntax map_impl <- 300 = generator "." "map" map_fn;

const map_impl = macro (~generator, ~map_fn) => `(
	const outer_y = _; const inner_y = _;
	let map_fn :: inner_y -> outer_y = $map_fn;
	dbg map_fn;
	let outer = current yields[Yield: outer_y, Resume: void];
	let inner :: yields[Yield: inner_y, Resume: void] = (
		yield: value => (
			outer.yield (map_fn value)
		),
	);
	with inner ( $generator )
);

let generator = fn(void) yields[Yield: int32, Resume: void] {
	yield (1 :: int32);
	yield (2 :: int32);
};

let map_fn :: int32 -> string = fn (s) {
	print "throwing";
	throw "haha"
};

let result = try[ok: void, error: string] (
	for value in (generator()) .map map_fn {
		print value;
	};
);
dbg result;
