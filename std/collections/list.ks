module:
const list = [T] type (
    .inner :: treap.t[T]
);
const t = list;

const create = [T] () -> list[T] => (
    .inner = treap.create ()
);
const push_back = [T] (a :: &list[T], value :: T) => (
    a^.inner = treap.join (a^.inner, treap.singleton value);
);
const iter = [T] (a :: &list[T], f :: &T -> ()) -> () => (
    treap.iter (&a^.inner, f);
);
const at = [T] (a :: &list[T], idx :: int32) -> &T => (
    treap.at (&a^.inner, idx)
);
const length = [T] (q :: &list[T]) -> int32 => (
    treap.length &q^.inner
);
