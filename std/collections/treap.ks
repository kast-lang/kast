module:

const data = [T] newtype {
    .left :: Box[Treap.t[T]],
    .right :: Box[Treap.t[T]],
    .value :: T,
    .count :: Int32,
    .priority :: Int32,
};
const t = [T] newtype (
    | :Empty
    | :Node data[T]
);

const new = [T] () -> Treap.t[T] => :Empty;
const singleton = [T] (value :: T) -> Treap.t[T] => :Node (
    {
        .left = Box_new(:Empty),
        .right = Box_new(:Empty),
        .value,
        .count = 1,
        .priority = std.random.gen_range(.min = 0, .max = 1000000000),
    }
);
const length = [T] (v :: &Treap.t[T]) -> Int32 => (
    match v^ with (
        | :Empty => 0
        | :Node ref v => v^.count
    )
);

const update_data = (data :: Ast, .new_left :: Ast, .new_right :: Ast) -> Ast => `(
    let count = 1 + length(&$new_left^) + length(&$new_right^);
    :Node {
        .left = $new_left,
        .right = $new_right,
        .value = $data.value,
        .count,
        .priority = $data.priority,
    }
);

(#
const update_data = [T] (
    data :: data[T],
) -> Treap.t[T] => (
    let count = 1 + length(&data.left^) + length(&data.right^);
    :Node {
        ...data,
        .count,
    }
);
#)

const join = [T] (left :: Treap.t[T], right :: Treap.t[T]) -> Treap.t[T] => (
    match ({ left, right } :: { _, _ }) with (
        | { :Empty, :Empty } => :Empty
        | { :Empty, other } => other
        | { other, :Empty } => other
        | { :Node (left :: data[T]), :Node (right :: data[T]) } => (
            if left.priority > right.priority then (
                let new_left = left.left;
                let new_right = Box_new(join[T](left.right^, :Node right));
                include_ast update_data(`(left), .new_left = `(new_left), .new_right = `(new_right))
            ) else (
                let new_left = Box_new(join[T](:Node left, right.left^));
                let new_right = right.right;
                include_ast update_data(`(right), .new_left = `(new_left), .new_right = `(new_right))
            )
        )
    )
);

const node_lookup_behavior = [T] newtype (
    | :LeftSubtree
    | :RightSubtree
    | :Here
);

const node_lookup = [T] type (
    &data[T] -> node_lookup_behavior[T]
);

const lookup = [T] (
    v :: &t[T],
    f :: node_lookup[T],
) -> Option.t[type (&T)] => with_return (
    let mut v = v;
    @loop (
        match v^ with (
            | :Empty => return :None
            | :Node ref node => (
                match f(node) with (
                    | :LeftSubtree => (
                        v = &node^.left^;
                    )
                    | :RightSubtree => (
                        v = &node^.right^;
                    )
                    | :Here => (
                        return :Some &node^.value;
                    )
                )
            )
        )
    )
);

const lookup_mut = [T] (
    v :: &mut t[T],
    f :: node_lookup[T],
) -> Option.t[type (&mut T)] => with_return (
    let mut v = v;
    @loop (
        match v^ with (
            | :Empty => return :None
            | :Node ref mut node => (
                match f(&node^) with (
                    | :LeftSubtree => (
                        v = &mut node^.left^;
                    )
                    | :RightSubtree => (
                        v = &mut node^.right^;
                    )
                    | :Here => (
                        return :Some &mut node^.value;
                    )
                )
            )
        )
    )
);

# Where does the node we are at belong?
const node_split_behavior = [T] newtype (
    | :LeftSubtree
    | :RightSubtree
    | :Node { T, T }
);
const node_splitter = [T] type (
    &data[T] -> node_split_behavior[T]
);

const split = [T] (v :: t[T], f :: node_splitter[T]) -> { t[T], t[T] } => (
    match v with (
        | :Empty => { :Empty, :Empty }
        | :Node node => match f(&node) with (
            | :RightSubtree => (
                let { left_left, left_right } = split[T](node.left^, f);
                let new_left = Box_new(left_right);
                let new_right = node.right;
                let node = include_ast update_data(`(node), .new_left = `(new_left), .new_right = `(new_right));
                { left_left, node }
            )
            | :LeftSubtree => (
                let { right_left, right_right } = split[T](node.right^, f);
                let new_left = node.left;
                let new_right = Box_new(right_left);
                let node = include_ast update_data(`(node), .new_left = `(new_left), .new_right = `(new_right));
                { node, right_right }
            )
            | :Node { left, right } => (
                let left = singleton(left);
                let right = singleton(right);
                { join(node.left^, left), join(right, node.right^) }
            )
        )
    )
);

const split_at = [T] (v :: Treap.t[T], mut idx :: Int32) -> { Treap.t[T], Treap.t[T] } => (
    split(
        v,
        node => (
            let this_node_idx = length(&node^.left^);
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
        | :Empty => panic("oob")
        | :Node ref v => (
            if idx == length(&v^.left^) then (
                &v^.value
            ) else if idx < length(&v^.left^) then (
                at[T](&v^.left^, idx)
            ) else (
                at[T](&v^.right^, idx - length(&v^.left^) - 1)
            )
        )
    )
);
const at_mut = [T] (v :: &mut Treap.t[T], idx :: Int32) -> &mut T => (
    match v^ with (
        | :Empty => panic("oob")
        | :Node ref mut v => (
            if idx == length(&v^.left^) then (
                &mut v^.value
            ) else if idx < length(&v^.left^) then (
                at_mut[T](&mut v^.left^, idx)
            ) else (
                at_mut[T](&mut v^.right^, idx - length(&v^.left^) - 1)
            )
        )
    )
);
const set_at = [T] (v :: Treap.t[T], idx :: Int32, value :: T) -> Treap.t[T] => (
    let { left, v } = split_at(v, idx);
    let { _, right } = split_at(v, 1);
    join(left, join(singleton(value), right))
);
const update_at = [T] (a :: Treap.t[T], idx :: Int32, f :: &T -> T) -> Treap.t[T] => (
    set_at(a, idx, f(at(&a, idx)))
);
const to_string = [T] (v :: &Treap.t[T], t_to_string :: &T -> String) -> String => (
    StringBuilder.build(() => (
        StringBuilder.add_str("[");
        let mut i :: Int32 = 0;
        for x in iter(v) do (
            if i != 0 then (
                StringBuilder.add_str(", ");
            );
            StringBuilder.add_String(t_to_string(x));
            i += 1;
        );
        StringBuilder.add_str("]");
    ))
);
const into_iter = [T] (v :: Treap.t[T]) -> std.iter.Iterable[T] => {
    .iter = @move f => (
        match v with (
            | :Empty => ()
            | :Node data => (
                (into_iter[T](data.left^)).iter(f);
                f(data.value);
                (into_iter[T](data.right^)).iter(f);
            )
        )
    )
};
const iter = [T] (v :: &Treap.t[T]) -> std.iter.Iterable[type (&T)] => {
    .iter = @move f => (
        match v^ with (
            | :Empty => ()
            | :Node ref data => (
                (iter[T](&data^.left^)).iter(f);
                f(&data^.value);
                (iter[T](&data^.right^)).iter(f);
            )
        )
    )
};
const iter_mut = [T] (v :: &mut Treap.t[T]) -> std.iter.Iterable[type (&mut T)] => {
    .iter = @move f => (
        match v^ with (
            | :Empty => ()
            | :Node ref mut data => (
                (iter_mut[T](&mut data^.left^)).iter(f);
                f(&mut data^.value);
                (iter_mut[T](&mut data^.right^)).iter(f);
            )
        )
    ),
};
