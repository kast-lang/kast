module:

const Rc = (
    module:

    const t = [T :: Type] ((@native "Rc")(T) :: Type);

    const new = [T] (value :: T) -> Rc.t[T] => @cfg (
        | target.name == "c" => (
            @native "RefCounted_\(type T)_new(\(value), \(T))"
        )
        | true => panic("TODO rc")
    );
);

