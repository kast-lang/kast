module:

const RawMutex = (
    module:

    const t = @opaque_type "pthread_mutex_t*";

    const new = () -> RawMutex.t => (
        let mut mutex = @native "Kast_malloc(sizeof(pthread_mutex_t))";
        let err :: Int32 = @native "pthread_mutex_init(\(mutex), NULL)";
        if err != 0 then (
            panic("Failed to init mutex");
        );
        mutex
    );

    const lock = (mutex :: RawMutex.t) => (
        let err :: Int32 = @native "pthread_mutex_lock(\(mutex))";
        if err != 0 then (
            panic("Failed to lock mutex");
        );
    );

    const unlock = (mutex :: RawMutex.t) => (
        let err :: Int32 = @native "pthread_mutex_unlock(\(mutex))";
        if err != 0 then (
            panic("Failed to unlock mutex");
        );
    );
);

const MutexGuard = [T] newtype {
    .raw :: RawMutex.t,
    .value :: &mut T,
};

const Mutex = (
    module:

    const t = [T] newtype {
        .raw :: RawMutex.t,
        .value :: &mut T,
    };

    const new = [T] (mut value :: T) -> Mutex.t[T] => {
        .raw = RawMutex.new(),
        .value = &mut value,
    };

    const lock = [T] (mutex :: &Mutex.t[T]) -> MutexGuard[T] => (
        RawMutex.lock(mutex^.raw);
        {
            .raw = mutex^.raw,
            .value = mutex^.value,
        }
    );

    const unlock = [T] (guard :: MutexGuard[T]) => (
        RawMutex.unlock(guard.raw);
    );
);
