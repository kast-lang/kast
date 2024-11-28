use std.*;

# TODO smth
const unsafe :: type = newtype :Unsafe;

let unsafe_fn = fn (s :: string) with unsafe {
    print s;
};

(
    with :Unsafe :: unsafe;
    unsafe_fn "hello";
)
