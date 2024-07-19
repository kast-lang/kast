# class Functor f where
#     fmap :: (a -> b) -> f a -> f b
#
# instance Functor Maybe where
#   fmap _ Nothing = Nothing
#   fmap f (Just a) = Just (f a)

dbg Option[int32]; # todo instantiate the template to make sure its compiled to fix Hashtbl issue

const Hkt = forall (t :: type). (type);

const Functor = trait (
    let fmap = forall (~a :: type, ~b :: type).
        (f: (a -> b), this: self[a]) -> self[b];
);

impl Functor for Option as (
    self: Option,
    fmap: (forall (~a :: type, ~b :: type). (
        (~f :: (a -> b), ~this :: Option[a]) => match this (
            | Some of a => Option[b].Some <| f a
            | None ofnone => Option[b].None
        )
    )),
);

let test = fn (int_option :: Option[int32]) {
    dbg int_option;
    let f = fn (x :: int32) {
        "im a string now!"  
    };
    let string_option = (Option as Functor).fmap[a: int32, b: string] (~f, this: int_option);
    dbg string_option;
};

test <| Option[int32].Some 123;
test <| Option[int32].None;