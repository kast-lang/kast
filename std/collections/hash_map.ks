module:

()
/*
const HashMap = forall[K :: type, V :: type] {
    comptime print "yo";
    let result = native "HashMap" (K, V) :: type;
    comptime dbg result;
    result
};

const insert = forall[K :: type, V :: type] {
    native "HashMap.insert" :: (HashMap[K, V], K, V) -> HashMap[K, V]
};
const get = forall[K :: type, V :: type] {
    native "HashMap.get" :: (HashMap[K, V], K) -> Option[V]
};
const size = forall[K :: type, V :: type] {
    native "HashMap.size" :: HashMap[K, V] -> int32
};
const iter = forall[K :: type, V :: type] {
    native "HashMap.iter" :: HashMap[K, V] -> () with generator_handler[K, V]
};
*/
