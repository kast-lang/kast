module:

const Id = @opaque_type "thrd_t";

const sleep_ns = (ns :: Int64) => (
    let s = ns / 1000000000;
    let ns = ns % 1000000000;
    let result :: Int32 = @native "thrd_sleep(&(struct timespec){.tv_sec=\(s),.tv_nsec=\(ns)}, NULL)";
    if result != 0 then (
        panic("Failed to sleep");
    );
);

const spawn = (f :: () -> ()) -> Id => (
    let f = @call "C" () -> Int32 => (
        let mut new_context = @native "(Context){}";
        let &@context = &mut new_context;
        with std.PanicHandler = std.default_panic_handler;
        f();
        0
    );
    let mut id = @native "(thrd_t){}";
    let result :: @opaque_type "int" = @native ''
        thrd_create(\(&mut id), \(f).f, \(f).captured)
    '';
    if @native "\(result) != thrd_success" then (
        panic("Failed to spawn thread");
    );
    id
);
