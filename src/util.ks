module:

const single_element_list = [T] (x :: T) -> ArrayList.t[T] => (
    let mut result = ArrayList.new();
    &mut result |> ArrayList.push_back(x);
    result
);
