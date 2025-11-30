module:
const list = [T] treap.t[T];
const t = list;
const create = [T] () -> list[T] => treap.create ();
const push_back = [T] (a :: list[T], value :: T) -> list[T] => (
    treap.merge (a, treap.singleton value)
);
const iter = [T] (a :: list[T], f :: (T -> ())) -> () => (
    treap.iter (a, f);
);
