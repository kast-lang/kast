module:

const push = forall[T] {
    native "list.push" :: (&list[T], T) -> ()
};
const set = forall[T] {
    native "list.set" :: (&list[T], int32, T) -> ()
};
const length = forall[T] {
    native "list.length" :: &list[T] -> int32
};
const iter = forall[T] {
    native "list.iter" :: &list[T] -> () with generator_handler[&T]
};
const get = forall[T] {
    native "list.get" :: (&list[T], int32) -> &T
};

