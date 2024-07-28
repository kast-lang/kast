let test = fn (x :: int32) {
	let result :: Result[ok: string, error: int32] =
		try[ok: string, error: int32] (
			if x == 0 then
				"hello"
			else
				throw[int32] x
		);
	let value :: string = result catch e (dbg e; "thrown");
	print <| value;
};

test 1;
test 0;
