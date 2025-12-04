module:
const data = [T] type (
    .left :: treap[T],
    .value :: T,
    .count :: int32,
    .priority :: int32,
    .right :: treap[T],
);
const treap = [T] type (
    | :Empty
    | :Node data[T]
);
const t = treap;
const create = [T] () -> treap[T] => :Empty;
const singleton = [T] (value :: T) -> treap[T] => (
    :Node (
        .left = :Empty,
        .right = :Empty,
        .value,
        .count = 1,
        .priority = std.rng.gen_int32 (.min = 0, .max = 1000000000),
    )
);
const length = [T] (v :: treap[T]) -> int32 => (
    match v with (
        | :Empty => 0
        | :Node v => v.count
    )
);
const merge = [T] (left :: treap[T], right :: treap[T]) -> treap[T] => (
    let update = (root :: data[T], (.left :: treap[T], .right :: treap[T])) => (
        :Node (
            .value = root.value,
            .priority = root.priority,
            .left,
            .right,
            .count = 1 + length (left) + length (right),
        )
    );
    match (left, right) with (
        | (:Empty, :Empty) => :Empty
        | (:Empty, other) => other
        | (other, :Empty) => other
        | (:Node (left_data :: data[T]), :Node (right_data :: data[T])) => (
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
const split_at = [T] (v :: treap[T], idx :: int32) -> (treap[T], treap[T]) => (
    if idx == 0 then (
        :Empty, v
    ) else if idx == length v then (
        v, :Empty
    ) else (
        let v = match v with (
            | :Node v => v
            | :Empty => _ # TODO panic
        );
        if idx <= length v.left then (
            let left_left, left_right = split_at (v.left, idx);
            let right = :Node (
                .left = :Empty,
                .value = v.value,
                .count = length v.right + 1,
                .priority = v.priority,
                .right = v.right,
            );
            left_left, merge (left_right, right)
        ) else (
            let right_left, right_right = split_at (v.right, idx - length v.left - 1);
            let left = :Node (
                .left = v.left,
                .value = v.value,
                .count = length v.left + 1,
                .priority = v.priority,
                .right = :Empty,
            );
            merge (left, right_left), right_right
        )
    )
);
const at = [T] (v :: treap[T], idx :: int32) -> T => (
    let _, v = split_at (v, idx);
    let v, _ = split_at (v, 1);
    match v with (
        | :Empty => _ # TODO panic
        | :Node v => v.value
    )
);
const set_at = [T] (v :: treap[T], idx :: int32, value :: T) -> treap[T] => (
    let left, v = split_at (v, idx);
    let _, right = split_at (v, 1);
    merge (left, merge (singleton value, right))
);
const update = [T] (a :: treap[T], idx :: int32, f :: T -> T) -> treap[T] => (
    set_at (a, idx, f (at (a, idx)))
);
const iter = [T] (v :: treap[T], f :: T -> ()) => (
    match v with (
        | :Empty => ()
        | :Node data => (
            iter[T] (data.left, f);
            f data.value;
            iter[T] (data.right, f);
        )
    )
);
