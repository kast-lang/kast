module:
const t = [T] newtype (
    .inner :: Treap.t[T]
);

const create = [T] () -> Queue.t[T] => (
    .inner = Treap.create ()
);
const push = [T] (q :: &Queue.t[T], value :: T) => (
    q^.inner = Treap.join (q^.inner, Treap.singleton value);
);
const pop = [T] (q :: &Queue.t[T]) -> T => (
    let first, rest = Treap.split_at (q^.inner, 1);
    q^.inner = rest;
    (Treap.at (&first, 0))^
);
const front = [T] (q :: &Queue.t[T]) -> &T => (
    Treap.at (&q^.inner, 0)
);
const length = [T] (q :: &Queue.t[T]) -> Int32 => (
    Treap.length &q^.inner
);
