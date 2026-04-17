module:

const t = [T] newtype {
    .inner :: OrdMap.t[T, type ()],
};

const new = [T] () -> OrdSet.t[T] => {
    .inner = OrdMap.new(),
};

const clone = [T] (self :: &OrdSet.t[T]) -> OrdSet.t[T] => {
    .inner = {
        ...self^.inner
    },
};

const contains = [T] (self :: &OrdSet.t[T], x :: T) -> Bool => (
    match &self^.inner |> OrdMap.get(x) with (
        | :Some _ => true
        | :None => false
    )
);

const remove = [T] (self :: &mut OrdSet.t[T], x :: T) -> Bool => (
    match &mut self^.inner |> OrdMap.remove(x) with (
        | :None => false
        | :Some () => true
    )
);

const length = [T] (self :: &OrdSet.t[T]) -> Int32 => (
    &self^.inner |> OrdMap.length
);

const add = [T] (self :: &mut OrdSet.t[T], x :: T) => (
    &mut self^.inner |> OrdMap.add(x, ());
);

const iter = [T] (self :: &OrdSet.t[T]) -> std.iter.Iterable[type (&T)] => (
    &self^.inner |> OrdMap.iter |> std.iter.map(&{ .key = ref value, .value = () } => value)
);

const into_iter = [T] (self :: OrdSet.t[T]) -> std.iter.Iterable[T] => (
    self.inner |> OrdMap.into_iter |> std.iter.map({ .key = value, .value = () } => value)
);