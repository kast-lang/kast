module:

const HashMap = forall[K :: type, V :: type] {
    native "HashMap" (K, V) :: type
};
const new = forall[K :: type, V :: type] {
    native "HashMap.new" :: () -> HashMap[K, V]
};
const insert = forall[K :: type, V :: type] {
    native "HashMap.insert" :: (&HashMap[K, V], K, V) -> ()
};
const get = forall[K :: type, V :: type] {
    native "HashMap.get" :: (&HashMap[K, V], &K) -> Option[&V]
};
const size = forall[K :: type, V :: type] {
    native "HashMap.size" :: &HashMap[K, V] -> int32
};
const into_iter = forall[K :: type, V :: type] {
    native "HashMap.into_iter" :: HashMap[K, V] -> () # with generator_handler[K, V]
};
