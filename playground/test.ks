# rust: [T; N]
# c: T var[N]

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

