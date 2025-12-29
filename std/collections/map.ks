module:

const KV = [K, V] type (
    .key :: K,
    .value :: V,
);
const t = [K, V] type (
    .inner :: Treap.t[KV[K, V]],
);
const create = [K, V] () -> Map.t[K, V] => (
    .inner = Treap.create ()
);

const add = [K, V] (map :: &mut Map.t[K, V], key :: K, value :: V) => (
    get_or_init (map, key, () => value);
);

const get = [K, V] (map :: &Map.t[K, V], key :: K) -> Option.t[type (&V)] => (
    let less, greater_or_equal = Treap.split (
        map^.inner,
        data => (
            if data^.value.key < key then (
                :LeftSubtree
            ) else (
                :RightSubtree
            )
        ),
    );
    let equal, greater = Treap.split (
        greater_or_equal,
        data => (
            if data^.value.key <= key then (
                :LeftSubtree
            ) else (
                :RightSubtree
            )
        ),
    );
    if Treap.length &equal == 0 then (
        :None
    ) else (
        :Some (&(Treap.at (&equal, 0))^.value)
    )
);

const get_or_init = [K, V] (
    map :: &mut Map.t[K, V],
    key :: K,
    init :: () -> V,
) -> &mut V => (
    let less, greater_or_equal = Treap.split (
        map^.inner,
        data => (
            if data^.value.key < key then (
                :LeftSubtree
            ) else (
                :RightSubtree
            )
        ),
    );
    let mut equal, greater = Treap.split (
        greater_or_equal,
        data => (
            if data^.value.key <= key then (
                :LeftSubtree
            ) else (
                :RightSubtree
            )
        ),
    );
    if Treap.length &equal == 0 then (
        equal = Treap.singleton (.key, .value = init ());
    );
    map^.inner = Treap.join (less, Treap.join (equal, greater));
    &mut (Treap.at_mut (&mut equal, 0))^.value
);

const iter = [K, V] (map :: &Map.t[K, V]) -> std.iter.Iterable[type (&KV[K, V])] => (
    Treap.iter (&map^.inner)
);
