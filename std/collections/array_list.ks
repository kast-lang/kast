module:

const t = [T] newtype {
    .inner :: Treap.t[T]
};
const new = [T] () -> ArrayList.t[T] => {
    .inner = Treap.new[T]()
};
const push_back = [T] (a :: &mut ArrayList.t[T], value :: T) => (
    a^.inner = Treap.join(a^.inner, Treap.singleton(value));
);
const iter = [T] (a :: &ArrayList.t[T]) -> std.iter.Iterable[type (&T)] => (
    Treap.iter(&a^.inner)
);
const iter_mut = [T] (a :: &mut ArrayList.t[T]) -> std.iter.Iterable[type (&mut T)] => (
    Treap.iter_mut(&mut a^.inner)
);
const at = [T] (a :: &ArrayList.t[T], idx :: Int32) -> &T => (
    Treap.at(&a^.inner, idx)
);
const at_mut = [T] (a :: &mut ArrayList.t[T], idx :: Int32) -> &mut T => (
    Treap.at_mut(&mut a^.inner, idx)
);
const length = [T] (q :: &ArrayList.t[T]) -> Int32 => (
    Treap.length(&q^.inner)
);
const to_string = [T] (a :: &ArrayList.t[T], f :: &T -> String) -> String => (
    Treap.to_string(&a^.inner, f)
);
