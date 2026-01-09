module:

const t = [T] newtype (
    | :Some T
    | :None
);
const Option = t;

const is_some = [T] (opt :: &Option[T]) -> Bool => match opt^ with (
    | :Some _ => true
    | :None => false
);

const is_none = [T] (opt :: &Option[T]) -> Bool => match opt^ with (
    | :Some _ => false
    | :None => true
);

const map = [T, U] (opt :: Option[T], f :: T -> U) -> Option[U] => match opt with (
    | :Some x => :Some (f x)
    | :None => :None
);

const and_then = [T, U] (opt :: Option[T], f :: T -> Option[U]) -> Option[U] => match opt with (
    | :Some x => f x
    | :None => :None
);

const as_ref = [T] (opt :: &Option[T]) -> Option[type (&T)] => match opt^ with (
    | :Some (ref x) => :Some x
    | :None => :None
);

const as_deref = [T] (opt :: Option[type (&T)]) -> Option[T] => match opt with (
    | :None => :None
    | :Some x => :Some x^
);

const unwrap = [T] (opt :: Option[T]) -> T => match opt with (
    | :Some x => x
    | :None => panic "unwrapped :None"
);
