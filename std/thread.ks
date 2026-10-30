module:

const Id = @opaque_type "pthread_t";

const sleep_ns = (ns :: Int64) => (
    @native "Kast_sleep_ns(\(ns))";
);

const join = (thread :: Id) => (
    let result :: @opaque_type "int" = @native "pthread_join(\(thread), NULL)";
    if @native "\(result) != 0" then (
        panic("Failed to join thread");
    );
);

const spawn = (f :: () -> ()) -> Id => (
    const F_Type = typeof f;
    const spawn_impl = fn @call "C" (f :: @opaque_type "void*") -> @opaque_type "void*" => (
        let f :: Box[type F_Type] = @native "Box_\(type F_Type)_from_raw(\(f), \(F_Type))";
        let mut new_context = @native "Context_new()";
        let &@context = &mut new_context;
        with std.PanicHandler = std.default_panic_handler;
        f^();
        @native "NULL"
    );
    let f = Box_leak(Box_new(f));
    let mut id = @native "(pthread_t){}";
    let result :: @opaque_type "int" = @native ''
        pthread_create(\(&mut id), NULL, \(spawn_impl), \(f))
    '';
    if @native "\(result) != 0" then (
        panic("Failed to spawn thread");
    );
    id
);
