module:

const Rc = (
    module:

    const t = [T :: Type] ((@native "Rc")(T) :: Type);

    const new = [T] (value :: T) -> Rc.t[T] => @cfg (
        | target.name == "interpreter" => (
            (@native "Rc.new")(value)
        )
        | target.name == "c" => (
            @native "RefCounted_\(type T)_new(\(value), \(T))"
        )
    );

    const ptr_eq = [T] (a :: &Rc.t[T], b :: &Rc.t[T]) -> Bool => @cfg (
        | target.name == "interpreter" => (
            (@native "place_eq")(&a^, &b^)
        )
        | target.name == "c" => (
            @native "RefCounted_\(type T)_ptr_eq(\(a), \(b))"
        )
    );

    const debug_dump_refs = [T] (rc :: &Rc.t[T]) => @cfg (
        | target.name == "c" => (
            @native "RefCounted_\(type T)_debug_dump_refs(\(rc))";
        )
    );
);

