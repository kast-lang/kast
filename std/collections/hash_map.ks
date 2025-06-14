module:

const HashMap = forall[K :: type, V :: type] {
    native "HashMap" (K, V) :: type
};
const new = forall[K :: type, V :: type] {
    () => cfg_if {
        | target.name == "interpreter" => native "HashMap.new"
        | target.name == "javascript" => native "new Map()"
    } :: () -> HashMap[K, V] with ()
};
const insert = forall[K :: type, V :: type] {
    (map, key, value) => cfg_if {
        | target.name == "interpreter" => native "HashMap.insert" (map, key, value)
        | target.name == "javascript" => native "$(map).set($(key),$(value))"
    } :: (&HashMap[K, V], K, V) -> () with ()
};
const get = forall[K :: type, V :: type] {
    (map, key) => cfg_if {
        | target.name == "interpreter" => native "HashMap.get" (map, key)
        | target.name == "javascript" => native "$(map).has($(key))?{variant:'Some','value':$(map).get($(key))}:{variant:'None'}"
    } :: (&HashMap[K, V], &K) -> Option[&V] with ()
};
const size = forall[K :: type, V :: type] {
    (map) => cfg_if {
        | target.name == "interpreter" => native "HashMap.size" map
        | target.name == "javascript" => native "$(map).size"
    } :: &HashMap[K, V] -> int32 with ()
};
const into_iter = forall[K :: type, V :: type] {
    (map) => cfg_if {
        | target.name == "interpreter" => native "HashMap.into_iter" map
        | target.name == "javascript" => native "new Map()"
    } :: HashMap[K, V] -> () with () # with generator_handler[K, V]
};
