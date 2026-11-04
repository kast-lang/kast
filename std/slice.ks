const Slice = (
    module:

    const t = [T :: Type] newtype {
        .length :: Int32,
        .array :: std.target.c.void_ptr,
    };

    const mutable = [T :: Type] newtype {
        .length :: Int32,
        .array :: std.target.c.void_ptr,
    };

    const unmut = [T] (self :: Slice.mutable[T]) -> Slice.t[T] => {
        .length = self.length,
        .array = self.array,
    };

    const of_array = [T, N] (a :: &Array.t[T, N]) -> Slice.t[T] => {
        .length = N,
        .array = @native "\(a)",
    };

    const of_array_mut = [T, N] (a :: &mut Array.t[T, N]) -> Slice.mutable[T] => {
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

    const as_array_mut = [T, N] (self :: Slice.mutable[T]) -> &mut Array.t[T, N] => (
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

    const subslice_mut = [T] (
        slice :: Slice.mutable[T],
        offset :: Int32,
        length :: Int32,
    ) -> Slice.mutable[T] => @cfg (
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

    const at_mut = [T] (self :: Slice.mutable[T], idx :: Int32) -> &mut T => @cfg (
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

    const iter_mut = [T] (self :: Slice.mutable[T]) -> std.iter.Iterable[type (&mut T)] => {
        .iter = @move consumer => (
            for i in 0..self.length do (
                consumer(self |> at_mut(i));
            );
        ),
    };

    const swap_at = [T] (self :: Slice.mutable[T], i :: Int32, j :: Int32) => (
        if i != j then (
            mem.swap(self |> at_mut(i), self |> at_mut(j));
        );
    );

    const split_at = [T] (
        self :: Slice.t[T],
        idx :: Int32,
    ) -> { Slice.t[T], Slice.t[T] } => {
        subslice(self, 0, idx),
        subslice(self, idx, self.length - idx),
    };

    const split_at_mut = [T] (
        self :: Slice.mutable[T],
        idx :: Int32,
    ) -> { Slice.mutable[T], Slice.mutable[T] } => {
        subslice_mut(self, 0, idx),
        subslice_mut(self, idx, self.length - idx),
    };

    const partition = [T] (
        self :: Slice.mutable[T],
        pivot :: Int32,
        compare :: std.cmp.Compare[T],
    ) -> { Slice.mutable[T], Slice.mutable[T] } => (
        let mut pivot = pivot;
        let mut less_bound = 0;
        let mut greater_bound = self.length;
        let mut i = 0;
        while i < greater_bound do (
            let cmp = compare(&at_mut(self, i)^, &at_mut(self, pivot)^);
            match cmp with (
                | :Less => (
                    if less_bound != i then (
                        swap_at(self, less_bound, i);
                        pivot = i;
                    );
                    less_bound += 1;
                    i += 1;
                )
                | :Equal => (
                    i += 1;
                )
                | :Greater => (
                    greater_bound -= 1;
                    swap_at(self, i, greater_bound);
                    if pivot == greater_bound then (
                        pivot = i;
                    );
                )
            );
        );
        if not less_bound < greater_bound then panic("huh??");
        {
            subslice_mut(self, 0, less_bound),
            subslice_mut(self, greater_bound, self.length - greater_bound),
        }
    );

    const sort_with = [T] (
        self :: Slice.mutable[T],
        compare :: std.cmp.Compare[T],
    ) => with_return (
        if self.length <= 1 then return;
        let pivot = (
            let i = 0;
            let j = self.length / 2;
            let k = self.length - 1;
            let self = self |> unmut;
            let ei = self |> at(i);
            let ej = self |> at(j);
            let ek = self |> at(k);
            if ei^ < ej^ then (
                if ej^ < ek^ then (
                    j
                ) else (
                    k
                )
            ) else (
                if ei^ < ek^ then (
                    i
                ) else (
                    k
                )
            )
        );
        let { left, right } = partition(self, pivot, compare);
        sort_with[T](left, compare);
        sort_with[T](right, compare);
    );

    const sort = [T] (self :: Slice.mutable[T]) => (
        sort_with[T](self, (T as std.cmp.Ord).compare);
    );

    const reverse = [T] (self :: Slice.mutable[T]) => (
        for i in 0..self.length / 2 do (
            swap_at(self, i, self.length - i);
        );
    );
);
