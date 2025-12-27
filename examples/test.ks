const Int32 = @native "int32";

const List = (
    module:

    const t = [T] newtype (
        | :Empty
        | :Node (
            .value :: T,
            .next :: t[T],
        )
    );
    const update_data = [T] (
        root,
        .next :: t[T],
    ) => (
        :Empty
    );
);

List.update_data[Int32]
