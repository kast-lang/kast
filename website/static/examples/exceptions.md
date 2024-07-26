```rs
let test = fn (x :: int32) {
	print <| try[ok: string, error: int32] {
		if x == 0 then
			"hello"
		else
			throw[int32] x
	} catch e {
		dbg e;
		"thrown"
	};
};

test 1;
test 0;
```
