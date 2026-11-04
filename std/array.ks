const Array = (
    module:

    const t = [T :: Type, N :: Int32] type @array[T, N];

    const subarray = [T, N, M] (
        a :: &Array.t[T, N],
        offset :: Int32,
    ) -> &Array.t[T, M] => @cfg (
        | target.name == "c" => @native "(\(type (&Array.t[T, M])))(\(a)->array + \(offset))"
        | true => panic("subarray only works in C")
    );
);
