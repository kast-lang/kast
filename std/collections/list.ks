module:
const list = [T] treap.t[T];
const t = list;
const create = [T] () -> list[T] => (
    treap.create ()
);
const push_back = [T] (a :: list[T], value :: T) -> list[T] => (
    treap.merge (a, treap.singleton value)
);
const iter = [T] (a :: list[T], f :: (T -> ())) -> () => (
    treap.iter (a, f);
);
const at = [T] (a :: list[T], idx :: int32) -> T => (
    treap.at (a, idx)
);
const set_at = [T] (a :: list[T], idx :: int32, value :: T) -> list[T] => (
    treap.set_at (a, idx, value)
);
const update = [T] (a :: list[T], idx :: int32, f :: T -> T) -> list[T] => (
    set_at (a, idx, f (at (a, idx)))
);
const length = [T] (q :: list[T]) -> int32 => (
    treap.length q
);
