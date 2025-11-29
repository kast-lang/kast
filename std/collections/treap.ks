module:
const node_data = [node_dataT] type (
    .left :: node[node_dataT],
    .value :: node_dataT,
    .count :: int32,
    .priority :: int32,
    .right :: node[node_dataT],
);
const node = [nodeT] type (
    | :Empty
    | :Cons node_data[nodeT]
);
const singleton = [singletonT] (value :: singletonT) -> node[singletonT] => (
    :Cons (
        .left = :Empty,
        .right = :Empty,
        .value,
        .count = 1,
        .priority = std.rng.gen_int32 (.min = 0, .max = 100),
    )
);
const count = [countT] (v :: node[countT]) -> int32 => (
    match v with (
        | :Empty => 0
        | :Cons v => v.count
    )
);
const merge = [mergeT] (left :: node[mergeT], right :: node[mergeT]) -> node[mergeT] => (
    let update = (root :: node_data[mergeT], (.left :: node[mergeT], .right :: node[mergeT])) => (
        :Cons (
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
        | (:Cons (left_data :: node_data[mergeT]), :Cons (right_data :: node_data[mergeT])) => (
            if left_data.priority > right_data.priority then (
                update (
                    left_data,
                    (
                        .left = left_data.left,
                        .right = merge[mergeT] (left_data.right, right)
                    )
                )
            ) else (
                update (
                    right_data,
                    (
                        .left = merge[mergeT] (left, right_data.left),
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
        | :Cons data => (
            iter[T] (data.left, f);
            f data.value;
            iter[T] (data.right, f);
        )
    )
);