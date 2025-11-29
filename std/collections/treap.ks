module:
const node_data = [T] type (
    .left :: node[T],
    .value :: T,
    .count :: int32,
    .priority :: int32,
    .right :: node[T],
);
const node = [T] type (
    | :Empty
    | :Node node_data[T]
);
const singleton = [T] (value :: T) -> node[T] => (
    :Node (
        .left = :Empty,
        .right = :Empty,
        .value,
        .count = 1,
        .priority = std.rng.gen_int32 (.min = 0, .max = 100),
    )
);
const count = [T] (v :: node[T]) -> int32 => (
    match v with (
        | :Empty => 0
        | :Node v => v.count
    )
);
const merge = [T] (left :: node[T], right :: node[T]) -> node[T] => (
    let update = (root :: node_data[T], (.left :: node[T], .right :: node[T])) => (
        :Node (
            .value = root.value,
            .priority = root.priority,
            .left,
            .right,
            .count = 1 + count (left) + count (right),
        )
    );
    match (left, right) with (
        | (:Empty, :Empty) => :Empty
        | (:Empty, other) => other
        | (other, :Empty) => other
        | (:Node (left_data :: node_data[T]), :Node (right_data :: node_data[T])) => (
            if left_data.priority > right_data.priority then (
                update (
                    left_data,
                    (
                        .left = left_data.left,
                        .right = merge[T] (left_data.right, right)
                    )
                )
            ) else (
                update (
                    right_data,
                    (
                        .left = merge[T] (left, right_data.left),
                        .right = right_data.right,
                    )
                )
            )
        )
    )
);
const iter = [T] (v :: node[T], f :: T -> ()) => (
    match v with (
        | :Empty => ()
        | :Node data => (
            iter[T] (data.left, f);
            f data.value;
            iter[T] (data.right, f);
        )
    )
);
