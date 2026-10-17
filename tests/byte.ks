const a :: Byte = 1;

let b :: Byte = 2;

if a + b != 3 then (
	panic("a + b != 3");
);

if b - a != 1 then (
	panic("b - a != 1");
);

if a % b != 1 then (
	panic("a % b != 1");
);

if b * (a + 1) != 4 then (
	panic("a * (a + 1) != 4");
);

const c :: Byte = 254;

let d :: Byte = 2;

if c + d != 0 then (
	panic("c + d != 0");
);

if @eval (a + c != 255) then (
	panic("@eval (a + c != 255)");
);

println!("all good \(69 :: Byte)");
