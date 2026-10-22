const Box = [T :: Type] ((@native "Box")(T) :: Type);
const Box_new = [T] (value :: T) -> Box[T] => @cfg (
    | target.name == "c" => (
        @native "Box_\(type T)_new(\(value), \(T))"
    )
    | true => panic("TODO box")
);

const Box_leak = [T] (box :: Box[T]) -> &mut T => @cfg (
    | target.name == "c" => (
        @native "Box_\(type T)_leak(\(box))"
    )
    | true => (
        let mut box = box;
        &mut box^
    )
);

