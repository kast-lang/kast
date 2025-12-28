module:

const data = [T] newtype (
    .left :: Treap.t[T],
    .value :: T,
    .count :: Int32,
    .priority :: Int32,
    .right :: Treap.t[T],
);
const t = [T] newtype (
    | :Empty
    | :Node data[T]
);
const create = [T] () -> Treap.t[T] => :Empty;
const singleton = [T] (value :: T) -> Treap.t[T] => (
    :Node (
        .left = :Empty,
        .right = :Empty,
        .value,
        .count = 1,
        .priority = std.random.gen_range (.min = 0, .max = 1000000000),
    )
);
const length = [T_length] (v :: &Treap.t[T_length]) -> Int32 => (
    match v^ with (
        | :Empty => 0
        | :Node v => v.count
    )
);
const update_data = [T] (root :: data[T], .left :: Treap.t[T], .right :: Treap.t[T]) => (
    :Node (
        .value = root.value,
        .priority = root.priority,
        .left,
        .right,
        .count = 1 + length &left + length &right,
    )
);
const join = [T] (left :: Treap.t[T], right :: Treap.t[T]) -> Treap.t[T] => (
    match (left, right) with (
        | (:Empty, :Empty) => :Empty
        | (:Empty, other) => other
        | (other, :Empty) => other
        | (:Node (left_data :: data[T]), :Node (right_data :: data[T])) => (
            if left_data.priority > right_data.priority then (
                update_data (
                    left_data,
                    .left = left_data.left,
                    .right = join[T] (left_data.right, right),
                )
            ) else (
                update_data (
                    right_data,
                    .left = join[T] (left, right_data.left),
                    .right = right_data.right,
                )
            )
        )
    )
);
# Where does the node we are at belong?
const node_split_behavior = [T] newtype (
    | :LeftSubtree
    | :RightSubtree
    | :Node (T, T)
);
const node_splitter = [T] type (
    &data[T] -> node_split_behavior[T]
);

const split = [T] (v :: t[T], f :: node_splitter[T]) -> (t[T], t[T]) => (
    match v with (
        | :Empty => (:Empty, :Empty)
        | :Node node => match f &node with (
            | :RightSubtree => (
                let left_left, left_right = split (node.left, f);
                let node = update_data (
                    node,
                    .left = left_right,
                    .right = node.right,
                );
                left_left, node
            )
            | :LeftSubtree => (
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
                join (node.left, left), join (right, node.right)
            )
        )
    )
);

const split_at = [T] (v :: Treap.t[T], mut idx :: Int32) -> (Treap.t[T], Treap.t[T]) => (
    split (
        v,
        node => (
            let this_node_idx = length &node^.left;
            if this_node_idx < idx then (
                idx -= this_node_idx + 1;
                :LeftSubtree
            ) else (
                :RightSubtree
            )
        )
    )
);
const at = [T] (v :: &Treap.t[T], idx :: Int32) -> &T => (
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
const at_mut = [T] (v :: &mut Treap.t[T], idx :: Int32) -> &mut T => (
    match v^ with (
        | :Empty => panic "oob"
        # TODO ref mut
        | :Node mut v => (
            if idx == length &v.left then (
                &mut v.value
            ) else if idx < length &v.left then (
                at_mut (&mut v.left, idx)
            ) else (
                at_mut (&mut v.right, idx - length &v.left - 1)
            )
        )
    )
);
const set_at = [T] (v :: Treap.t[T], idx :: Int32, value :: T) -> Treap.t[T] => (
    let left, v = split_at (v, idx);
    let _, right = split_at (v, 1);
    join (left, join (singleton value, right))
);
const update_at = [T] (a :: Treap.t[T], idx :: Int32, f :: &T -> T) -> Treap.t[T] => (
    set_at (a, idx, f (at (&a, idx)))
);
const to_string = [T] (v :: &Treap.t[T], t_to_string :: &T -> String) -> String => (
    let mut result = "[";
    let mut i :: Int32 = 0;
    iter (
        v,
        x => (
            if i != 0 then (
                result += ", ";
            );
            result += t_to_string x;
            i += 1;
        ),
    );
    result += "]";
    result
);
const iter = [T] (v :: &Treap.t[T], f :: &T -> ()) => (
    match v^ with (
        | :Empty => ()
        | :Node data => (
            iter[T] (&data.left, f);
            f &data.value;
            iter[T] (&data.right, f);
        )
    )
);
const iter_mut = [T] (v :: &mut Treap.t[T], f :: &mut T -> ()) => (
    match v^ with (
        | :Empty => ()
        # TODO ref mut
        | :Node mut data => (
            iter_mut (&mut data.left, f);
            f &mut data.value;
            iter_mut (&mut data.right, f);
        )
    )
);
