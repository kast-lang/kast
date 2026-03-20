module:

const t = [T] (@native "List")(T);
const new = [T] () -> ArrayList.t[T] => @cfg (
    | target.name == "interpreter" => (@native "List.new")()
    | target.name == "javascript" => @native "[]"
);
const push_back = [T] (a :: &mut ArrayList.t[T], value :: T) => @cfg (
    | target.name == "interpreter" => (@native "List.push_back")(a, value)
    | target.name == "javascript" => @native "\(a^).push(\(value))"
);
const iter = [T] (a :: &ArrayList.t[T]) -> std.iter.Iterable[type (&T)] => {
    .iter = consumer => (
        for i in 0..length(a) do (
            consumer(a |> at(i))
        );
    ),
};
const iter_mut = [T] (a :: &mut ArrayList.t[T]) -> std.iter.Iterable[type (&mut T)] => {
    .iter = consumer => (
        for i in 0..length(&a^) do (
            consumer(a |> at_mut(i))
        );
    ),
};
const at = [T] (a :: &ArrayList.t[T], idx :: Int32) -> &T => @cfg (
    | target.name == "interpreter" => (@native "List.at")(a, idx)
    | target.name == "javascript" => @native "{get:()=>\(a^)[\(idx)]}"
);
const at_mut = [T] (a :: &mut ArrayList.t[T], idx :: Int32) -> &mut T => @cfg (
    | target.name == "interpreter" => (@native "List.at")(a, idx)
    | target.name == "javascript" => @native "{get:()=>\(a^)[\(idx)],set:x=>{\(a^)[\(idx)]=x}}"
);
const length = [T] (a :: &ArrayList.t[T]) -> Int32 => @cfg (
    | target.name == "interpreter" => (@native "List.length")(a)
    | target.name == "javascript" => @native "\(a^).length"
);
const to_string = [T] (a :: &ArrayList.t[T], t_to_string :: &T -> String) -> String => (
    let mut result = "[";
    let mut i :: Int32 = 0;
    for x in iter(a) do (
        if i != 0 then (
            result += ", ";
        );
        result += t_to_string(x);
        i += 1;
    );
    result += "]";
    result
);
