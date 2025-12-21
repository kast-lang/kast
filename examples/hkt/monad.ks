use std.prelude.*;

const Monad = [M :: [type] -> type] newtype (
    .ret :: [T] T -> M[T],
    .flat_map :: [A, B] (M[A], (A -> M[B])) -> M[B],
);

impl Option.t as Monad = (
    .ret = [T] (x :: T) => :Some x,
    .flat_map = [A, B] (opt, f) => match opt with (
        | :Some x => f x
        | :None => :None
    ),
);

use std.collections.Treap;
impl Treap.t as Monad = (
    .ret = Treap.singleton,
    .flat_map = [A, B] (a, f) => (
        let mut result = Treap.create ();
        Treap.iter (
            &a,
            &x => (
                result = Treap.join (result, f x);
            ),
        );
        result
    ),
);

const join = [M :: [_unused :: type] type] (
    [T] (a :: M[M[T]]) -> M[T] => (
        (M as Monad).flat_map (a, x => x)
    )
);

(
    let t :: Treap.t[Treap.t[Int32]] = Treap.join (
        Treap.singleton (
            Treap.join (
                Treap.singleton 1,
                Treap.singleton 2,
            )
        ),
        Treap.singleton (Treap.singleton 3),
    );
    let a = join[Treap.t][_] (t);
    print <| Treap.to_string (&a, &x => x |> to_string);
);

const compose = [M :: [type] -> type] [A, B] (a :: M[A], b :: M[B]) -> M[B] => (
    (M as Monad).flat_map (a, _ => b)
);

@syntax ">>=" 6.5 wrap if_any = a " "/"\n" ">>=" " " b ->;
impl syntax (a >>= b) = `(
    (Option.t as Monad).flat_map ($a, $b)
    # TODO (_ as Monad).flat_map ($a, $b)
);

@syntax ">>" 6.5 wrap if_any = a " "/"\n" ">>" " " b ->;
impl syntax (a >> b) = `(
    compose ($a, $b)
);

@syntax "do" 6.5 wrap if_any = "do" " "/"\n\t" body:any " "/"\\\n" "done";
impl syntax (do body done) = `($body);

@syntax "and_then" 0 wrap if_any = a ";" ";" " "/"\n" b ->;
impl syntax (a;; b) = `(compose ($a, $b));

@syntax "bind_and_then" 0 wrap if_any = var " " "<-" " " expr ";" ";" " "/"\n" b ->;
impl syntax (var <- expr;; b) = `($expr >>= ($var => $b));

let opt :: Option.t[Int32] = :Some 1;

let result = do
    x <- opt;;
    y <- :Some 2;;
    :Some (x + y)
done;

dbg.print result;
