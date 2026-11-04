const Box = (
    module:

    const t = [T :: Type] ((@native "Box")(T) :: Type);

    const new = [T] (value :: T) -> Box.t[T] => @cfg (
        | target.name == "interpreter" => (
            (@native "Box.new")(value)
        )
        | target.name == "c" => (
            @native "Box_\(type T)_new(\(value), \(T))"
        )
    );

    const leak = [T] (box :: Box.t[T]) -> &mut T => @cfg (
        | target.name == "interpreter" => (
            let mut box = box;
            &mut box^
        )
        | target.name == "c" => (
            @native "Box_\(type T)_leak(\(box))"
        )
    );
);
