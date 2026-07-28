const std = (
    module:

    const Type = @native "Type";

    const String :: Type = @native "String";
    const Int :: Type = @native "Int32";

    const dbg = [T] (x :: T) -> () => (@native "dbg.print")(x);

    const Tree = (
        module:

        const Node = [T] type {
            .left :: Tree.t[T],
        };
        
        const t = [U] type (
            | :Empty
            | :Node Node[U]
        );
    );
);

use std.*;

dbg(123 :: Int);
dbg("Hello, world!");

let tree :: Tree.t[Int] = :Node {
    .left = :Empty,
    .right = :Node {
        .left = Tree.empty(),
        .right = :Empty,
    },
};
dbg(tree);

