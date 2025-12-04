module:
const list = [T] treap.t[T];
const t = list;

const create = [T] () -> list[T] => (
    treap.create ()
);
const push_back = [T] (a :: &list[T], value :: T) => (
    a^ = treap.merge (a^, treap.singleton value);
);
const iter = [T] (a :: &list[T], f :: &T -> ()) -> () => (
    treap.iter (a, f);
);
const at = [T] (a :: &list[T], idx :: int32) -> &T => (
    treap.at (a, idx)
);
const length = [T] (q :: &list[T]) -> int32 => (
    treap.length q
);
