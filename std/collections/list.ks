module:
const t = [T] newtype (
    .inner :: Treap.t[T]
);
const create = [T] () -> List.t[T] => (
    .inner = Treap.create ()
);
const push_back = [T] (a :: &List.t[T], value :: T) => (
    a^.inner = Treap.join (a^.inner, Treap.singleton value);
);
const iter = [T] (a :: &List.t[T], f :: &T -> ()) -> () => (
    Treap.iter (&a^.inner, f);
);
const at = [T] (a :: &List.t[T], idx :: Int32) -> &T => (
    Treap.at (&a^.inner, idx)
);
const length = [T] (q :: &List.t[T]) -> Int32 => (
    Treap.length &q^.inner
);
