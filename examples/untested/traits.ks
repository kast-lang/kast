use std.*;

const Foo :: type = ( .a = int32, .b = int32);
const Clone = forall[Self] {
    .clone = &Self -> Self,
};
impl Foo as Clone = (
    .clone = self => ( .a = (self^).a, .b = (self^).b),
);
let duplicate = forall[T] {
    fn (x :: T) -> (T, T) {
        (
            (T as Clone).clone(&x),
            (_ as Clone).clone(&x),
        )
    }
};
let foo :: (Foo, Foo) = duplicate ( .a = 1, .b = 2 );
dbg foo;
