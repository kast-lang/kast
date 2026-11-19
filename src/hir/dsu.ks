use std.rc.Rc;

module:

const Dsu = (
    module:

    const Root = [T] Rc.t[newtype {
        .value :: T,
        .count :: Int32,
    }];

    const Node = [T] Rc.t[newtype (
        | :Root Root[T]
        | :NonRoot { .closer_to_root :: Node[T] }
    )];
    
    let Node_new = [T] (value :: T) -> Node[T] => Rc.new(
        :Root Rc.new({
            .value,
            .count = 1,
        })
    );

    const find_root_node = [T] (mut node :: Node[T]) -> Node[T] => (
        match node^ with (
            | :Root _ => node
            | :NonRoot { .closer_to_root = ref mut closer_to_root } => (
                let root = find_root_node[T](closer_to_root^);
                closer_to_root^ = root;
                root
            )
        )
    );

    const find_root = [T] (node :: Node[T]) -> Root[T] => (
        match (find_root_node(node))^ with (
            | :Root root => root
            | :NonRoot _ => panic("bug")
        )
    );

    const get_value = [T] (node :: &Node[T]) -> &T => (
        &find_root(node^)^.value
    );

    const merge = [T] (
        a :: Node[T],
        b :: Node[T],
        .merge_value :: (T, T) -> T,
    ) -> (
        | :WereAlreadySame
        | :Merged
    ) => with_return (
        let a = find_root_node(a);
        let b = find_root_node(b);
        let root_a = find_root(a);
        let root_b = find_root(b);
        if Rc.ptr_eq(&root_a, &root_b) then (
            return :WereAlreadySame;
        );
        let attach = (a, b) => (
            let root_a = find_root(a);
            let root_b = find_root(b);
            root_b^.value = merge_value(root_a^.value, root_b^.value);
            root_b^.count += root_a^.count;
            a^ = :NonRoot { .closer_to_root = b };
        );
        if root_a^.count < root_b^.count then (
            attach(a, b);
        ) else (
            attach(b, a)
        );
        :Merged
    );
    const is_same = [T] (a :: Node[T], b :: Node[T]) -> Bool => (
        let root_a = find_root(a);
        let root_b = find_root(b);
        Rc.ptr_eq(&root_a, &root_b)
    );
);
