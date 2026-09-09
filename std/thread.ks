module:

const Id = @opaque_type "pthread_t";

const sleep_ns = (ns :: Int64) => (
    @native "Kast_sleep_ns(\(ns))";
);

const spawn = (f :: () -> ()) -> Id => (
    let f = @call "C" () -> @opaque_type "void*" => (
        let mut new_context = @native "(Context){}";
        let &@context = &mut new_context;
        with std.PanicHandler = std.default_panic_handler;
        f();
        @native "NULL"
    );
    let mut id = @native "(pthread_t){}";
    let result :: @opaque_type "int" = @native ''
        pthread_create(\(&mut id), NULL, \(f).f, \(f).captured)
    '';
    if @native "\(result) != 0" then (
        panic("Failed to spawn thread");
    );
    id
);
