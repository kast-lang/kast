module:
const t = [T] newtype (
    .inner :: Treap.t[T]
);
const create = [T] () -> List.t[T] => (
    .inner = Treap.create[T] ()
);
const push_back = [T] (a :: &mut List.t[T], value :: T) => (
    a^.inner = Treap.join (a^.inner, Treap.singleton value);
);
const iter = [T] (a :: &List.t[T], f :: &T -> ()) -> () => (
    Treap.iter (&a^.inner, f);
);
const iter_mut = [T] (a :: &mut List.t[T], f :: &mut T -> ()) -> () => (
    Treap.iter_mut (&mut a^.inner, f);
);
const at = [T] (a :: &List.t[T], idx :: Int32) -> &T => (
    Treap.at (&a^.inner, idx)
);
const at_mut = [T] (a :: &mut List.t[T], idx :: Int32) -> &mut T => (
    Treap.at_mut (&mut a^.inner, idx)
);
const length = [T] (q :: &List.t[T]) -> Int32 => (
    Treap.length &q^.inner
);
const to_string = [T] (a :: &List.t[T], f :: &T -> String) -> String => (
    Treap.to_string (&a^.inner, f)
);
