module:
const queue = [T] treap.t[T];
const t = queue;

const create = [T] () -> queue[T] => (
    treap.create ()
);
const push = [T] (q :: queue[T], value :: T) -> queue[T] => (
    treap.merge (q, treap.singleton value)
);
const pop = [T] (q :: queue[T]) -> (T, queue[T]) => (
    let first, rest = treap.split_at (q, 1);
    treap.at (first, 0), rest
);
const front = [T] (q :: queue[T]) -> T => (
    treap.at (q, 0)
);
const length = [T] (q :: queue[T]) -> int32 => (
    treap.length q
);
