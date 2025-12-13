module:
const queue = [T] newtype (
    .inner :: treap.t[T]
);
const t = queue;

const create = [T] () -> queue[T] => (
    .inner = treap.create ()
);
const push = [T] (q :: &queue[T], value :: T) => (
    q^.inner = treap.join (q^.inner, treap.singleton value);
);
const pop = [T] (q :: &queue[T]) -> T => (
    let first, rest = treap.split_at (q^.inner, 1);
    q^.inner = rest;
    (treap.at (&first, 0))^
);
const front = [T] (q :: &queue[T]) -> &T => (
    treap.at (&q^.inner, 0)
);
const length = [T] (q :: &queue[T]) -> int32 => (
    treap.length &q^.inner
);
