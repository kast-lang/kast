module:
const list = [T] type (:Empty | :Cons (.value :: T, .rest :: list[T]));
const t = list;
const create = [T] () -> list[T] => :Empty;
