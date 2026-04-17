module:

const KV = [K, V] newtype {
    .key :: K,
    .value :: V,
};
const t = [K, V] newtype {
    .inner :: Treap.t[KV[K, V]],
    .compare :: std.cmp.Compare[K],
};

const length = [K, V] (self :: &OrdMap.t[K, V]) -> Int32 => (
    &self^.inner |> Treap.length
);

const new = [K, V] () -> OrdMap.t[K, V] => (
    new_with_compare(std.cmp.default_compare[K])
);
const new_with_compare = [K, V] (compare :: std.cmp.Compare[K]) -> OrdMap.t[K, V] => {
    .inner = Treap.new(),
    .compare,
};

const split_inner_at_key = [K, V](
    map :: OrdMap.t[K, V],
    key :: K,
) -> {
    .less :: Treap.t[KV[K, V]],
    .equal :: Treap.t[KV[K, V]],
    .greater :: Treap.t[KV[K, V]],
} => (
    let split_with = (treap, f) => Treap.split(
        treap,
        data => if map.compare(data^.value.key, key) |> f then (
            :LeftSubtree
        ) else (
            :RightSubtree
        ),
    );
    let { less, greater_or_equal } = split_with(
        map.inner,
        std.cmp.Ordering.is_less,
    );
    let { equal, greater } = split_with(
        greater_or_equal,
        std.cmp.Ordering.is_less_or_equal,
    );
    { .less, .equal, .greater }
);

const add = [K, V] (map :: &mut OrdMap.t[K, V], key :: K, value :: V) => (
    let { .less, .equal = _, .greater } = split_inner_at_key(map^, key);
    let equal = Treap.singleton({ .key, .value });
    map^.inner = Treap.join(less, Treap.join(equal, greater));
);

const get = [K, V] (map :: &OrdMap.t[K, V], key :: K) -> Option.t[type (&V)] => (
    let { .equal, ... } = split_inner_at_key(map^, key);
    match equal with (
        | :Empty => :None
        | :Node data => :Some &data.value.value
    )
);

const get_mut = [K, V] (map :: &mut OrdMap.t[K, V], key :: K) -> Option.t[type (&mut V)] => (
    let { .equal, ... } = split_inner_at_key(map^, key);
    match equal with (
        | :Empty => :None
        | :Node mut data => :Some &mut data.value.value
    )
);

const remove = [K, V] (map :: &mut OrdMap.t[K, V], key :: K) -> Option.t[V] => (
    let { .less, .equal, .greater } = split_inner_at_key(map^, key);
    map^.inner = Treap.join(less, greater);
    match equal with (
        | :Empty => :None
        | :Node mut data => :Some data.value.value
    )
);

const get_or_init = [K, V] (
    map :: &mut OrdMap.t[K, V],
    key :: K,
    init :: () -> V,
) -> &mut V => (
    let { .less, .equal, .greater } = split_inner_at_key(map^, key);
    let mut equal = match equal with (
        | :Empty => Treap.singleton({ .key, .value = init() })
        | :Node _ => equal
    );
    map^.inner = Treap.join(less, Treap.join(equal, greater));
    &mut Treap.at_mut(&mut equal, 0)^.value
);

const into_iter = [K, V] (map :: OrdMap.t[K, V]) -> std.iter.Iterable[KV[K, V]] => (
    Treap.into_iter(map.inner)
);

const iter = [K, V] (map :: &OrdMap.t[K, V]) -> std.iter.Iterable[type (&KV[K, V])] => (
    Treap.iter(&map^.inner)
);

const iter_mut = [K, V] (map :: &mut OrdMap.t[K, V]) -> std.iter.Iterable[type (&mut KV[K, V])] => (
    Treap.iter_mut(&mut map^.inner)
);

