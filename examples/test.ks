const std = (
    module:

    const Int32 = @native "int32";

    const Treap = (
        module:
        
        const data = [T] type (
            .left :: t[T],
            .value :: T,
            .count :: Int32,
            .priority :: Int32,
            .right :: t[T],
        );
        const t = [T] type (
            | :Empty
            | :Node data[T]
        );
    );

    const List = (
        module:
        const t = [T] type (
            .inner :: Treap.t[T]
        );
    );
);

std.List.t[std.Int32]