const N = 3;
let x :: Array.t[Int32, N] = @array { 1, 2, 3 };

let sub :: &Array.t[Int32, 1] = &x |> Array.subarray(1);
dbg.print(sub);
dbg.print(x);

let slice = &x |> Slice.of_array;
dbg.print(slice);

let sub = slice |> Slice.subslice(1, 2);
dbg.print(sub);

dbg.print(sub |> Slice.as_array[_, 2]);

for x in sub |> Slice.iter do (
    dbg.print(x);
);

let mut a = @array { 1, 2, 3 };
let s :: Slice.mutable[Float32] = Slice.of_array_mut(&mut a);
for x in s |> Slice.iter_mut do (
    x^ += 6.7;
);
dbg.print(a);
