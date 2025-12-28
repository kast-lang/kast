const Int32 = @native "int32";

const Treap = (
    module:

    const t = [T] newtype (
        | :Node t[T]
    );
    const update_data = [T] (x :: t[T]) -> t[T] => (
        :Node _
    );
    const join = [T] () -> t[T] => (
        update_data[T] _
    );
);
