{ ... }:

{
	source = builtins.toFile "test.ks" ''
		const id = forall (T :: type). ((x => x) :: T -> T);
		dbg <| id[int32] 123
	'';
	expected_output = builtins.toFile "expected" "123 :: int32\n";
}
