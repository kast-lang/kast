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
const length = [T] (v :: &treap[T]) -> int32 => (
    match v^ with (
        | :Empty => 0
        | :Node v => v.count
    )
);
const update_data = [T] (root :: data[T], .left :: treap[T], .right :: treap[T]) => (
    :Node (
        .value = root.value,
        .priority = root.priority,
        .left,
        .right,
        .count = 1 + length &left + length &right,
    )
);
const merge = [T] (left :: treap[T], right :: treap[T]) -> treap[T] => (
    match (left, right) with (
        | (:Empty, :Empty) => :Empty
        | (:Empty, other) => other
        | (other, :Empty) => other
        | (:Node (left_data :: data[T]), :Node (right_data :: data[T])) => (
            if left_data.priority > right_data.priority then (
                update_data (
                    left_data,
                    .left = left_data.left,
                    .right = merge[T] (left_data.right, right),
                )
            ) else (
                update_data (
                    right_data,
                    .left = merge[T] (left, right_data.left),
                    .right = right_data.right,
                )
            )
        )
    )
);
const node_split_behavior = [T] type (
    | :LeftSubtree
    | :RightSubtree
    | :Node (T, T)
);
const node_splitter = [T] type (
    &data[T] -> node_split_behavior[T]
);
const split = [T] (v :: treap[T], f :: node_splitter[T]) -> (treap[T], treap[T]) => (
    match v with (
        | :Empty => (:Empty, :Empty)
        | :Node node => match f &node with (
            | :LeftSubtree => (
                let left_left, left_right = split (node.left, f);
                let node = update_data (
                    node,
                    .left = left_right,
                    .right = node.right,
                );
                left_left, node
            )
            | :RightSubtree => (
                let right_left, right_right = split (node.right, f);
                let node = update_data (
                    node,
                    .left = node.left,
                    .right = right_left,
                );
                node, right_right
            )
            | :Node (left, right) => (
                let left = singleton left;
                let right = singleton right;
                merge (node.left, left), merge (right, node.right)
            )
        )
    )
);
const split_at = [T] (v :: treap[T], idx :: int32) -> (treap[T], treap[T]) => (
    split (
        v,
        node => (
            if idx <= length &node^.left then (
                :LeftSubtree
            ) else (
                :RightSubtree
            )
        )
    )
);
const at = [T] (v :: &treap[T], idx :: int32) -> &T => (
    match v^ with (
        | :Empty => panic "oob"
        | :Node v => (
            if idx == length &v.left then (
                &v.value
            ) else if idx < length &v.left then (
                at (&v.left, idx)
            ) else (
                at (&v.right, idx - length &v.left - 1)
            )
        )
    )
);
const set_at = [T] (v :: treap[T], idx :: int32, value :: T) -> treap[T] => (
    let left, v = split_at (v, idx);
    let _, right = split_at (v, 1);
    merge (left, merge (singleton value, right))
);
const update_at = [T] (a :: treap[T], idx :: int32, f :: &T -> T) -> treap[T] => (
    set_at (a, idx, f (at (&a, idx)))
);
const iter = [T] (v :: &treap[T], f :: &T -> ()) => (
    match v^ with (
        | :Empty => ()
        | :Node data => (
            iter[T] (&data.left, f);
            f &data.value;
            iter[T] (&data.right, f);
        )
    )
);
