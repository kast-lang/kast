const Slice = (
    module:

    const t = [T] newtype {
        .length :: Int32,
        .array :: std.target.c.void_ptr,
    };

    const of_array = [T, N] (a :: &Array.t[T, N]) -> Slice.t[T] => {
        .length = N,
        .array = @native "\(a)",
    };

    const as_array = [T, N] (self :: Slice.t[T]) -> &Array.t[T, N] => (
        if self.length != N then (
            panic("Slice.as_array length mismatch");
            # std.panic_fmt!("slice_as_array expected length \(N), actual length = \(slice.length)");
        );
        @native "\(self.array)"
    );

    const subslice = [T] (
        slice :: Slice.t[T],
        offset :: Int32,
        length :: Int32,
    ) -> Slice.t[T] => @cfg (
        | target.name == "c" => {
            .array = @native "(char*)\(slice.array) + \(offset) * \(T)->stride",
            .length,
        }
        | true => panic("subslice only works in C")
    );

    const at = [T] (self :: Slice.t[T], idx :: Int32) -> &T => @cfg (
        | target.name == "c" => (
            @native "(void*)((char*)\(self.array) + \(idx) * \(T)->stride)"
        )
        | true => panic("Slice.at only works in C")
    );

    const iter = [T] (self :: Slice.t[T]) -> std.iter.Iterable[type (&T)] => {
        .iter = @move consumer => (
            for i in 0..self.length do (
                consumer(self |> at(i));
            );
        ),
    };
);
