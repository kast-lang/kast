module:

const t = [T, E] newtype (
    | :Ok T
    | :Error E
);
const Result = t;

const is_ok = [T, E] (res :: &Result[T, E]) -> Bool => match res^ with (
    | :Ok _ => true
    | :Error _ => false
);

const is_err = [T, E] (res:: &Result[T, E]) -> Bool => not is_ok(res);

const map = [T, U, E] (res :: Result[T, E], f :: T -> U) -> Result[U, E] => (
    match res with (
        | :Ok x => :Ok f(x)
        | :Error e => :Error e
    )
);
const map_err = [T, E, F] (res :: Result[T, E], f :: E -> F) -> Result[T, F] => (
    match res with (
        | :Ok x => :Ok x
        | :Error e => :Error f(e)
    )
);

const ok = [T, E] (res :: Result[T, E]) -> std.Option.t[T] => (
    match res with (
        | :Ok x => :Some x
        | :Error _ => :None
    )
);

const err = [T, E] (res :: Result[T, E]) -> std.Option.t[E] => (
    match res with (
        | :Ok _ => :None
        | :Error e => :Some e
    )
);

const inspect = [T, E] (res :: Result[T, E], f :: &T -> ()) -> Result[T, E] => (
    match res with (
        | :Ok x => (
            f(&x);
            res
        )
        | :Error _ => res
    )
);

const inspect_err = [T, E] (res :: Result[T, E], f :: &E -> ()) -> Result[T, E] => (
    match res with (
        | :Ok _ => res
        | :Error e => (
            f(&e);
            res
        )
    )
);

const and_then = [T, U, E] (res :: Result[T, E], f :: T -> Result[U, E]) -> Result[U, E] => (
    match res with (
        | :Ok x => f(x)
        | :Error e => :Error e
    )
);

const as_ref = [T, E] (res :: &Result[T, E]) -> Result[type (&T), E] => (
    match res^ with (
        | :Ok ref x => :Ok x
        | :Error e => :Error e
    )
);

const as_deref = [T, E] (res :: Result[type (&T), E]) -> Result[T, E] => (
    match res with (
        | :Ok x => :Ok x^
        | :Error e => :Error e
    )
);

const or_else = [T, E] (res :: Result[T, E], default :: E -> Result[T, E]) -> Result[T, E] => (
    match res with (
        | :Ok _ => res
        | :Error e => default(e)
    )
);

const unwrap = [T, E] (res :: Result[T, E]) -> T => match res with (
    | :Ok x => x
    # TODO: Also add error to panic message using E's ToString impl
    | :Error _ => panic("unwrapped :Error")
);

const expect = [T, E] (res :: Result[T, E], msg :: &String) -> T => match res with (
    | :Ok x => x
    # TODO: Also add error to panic message using E's ToString impl
    | :Error _ => panic(msg^)
);

const expect_err = [T, E] (res :: Result[T, E], msg :: &String) -> E => match res with (
    # TODO: Also add ok value to panic message using T's ToString impl
    | :Ok _ => panic(msg^)
    | :Error err => err
);

const unwrap_or_else = [T, E] (res :: Result[T, E], default :: E -> T) -> T => (
    match res with (
        | :Ok x => x
        | :Error e => default(e)
    )
);

const unwrap_or = [T, E] (res :: Result[T, E], default :: T) -> T => (
    match res with (
        | :Ok x => x
        | :Error _ => default
    )
);
