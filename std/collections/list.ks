module:
const list2 = type (
    | :Empty
    | :Cons (
        .value :: int32,
        .rest :: list2,
    )
);
const t = list2;
# const list = [T] type (:Empty | :Cons (.value :: T, .rest :: list[T]));
# const t = list;
# const create = [T] () -> list[T] => :Empty;
