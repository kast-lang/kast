module:

const HashMap = forall[K :: type, V :: type] {
    native "HashMap" (K, V) :: type
};
const new = forall[K :: type, V :: type] {
    fn(()) -> HashMap[K, V] with () {
        cfg_if {
            | target.name == "interpreter" => native "HashMap.new" ()
            | target.name == "javascript" => native "new Map()"
        }
     }
};

const insert = forall[K :: type, V :: type] {
    fn(map :: &HashMap[K, V], key :: K, value :: V) -> () with () {
        cfg_if {
            | target.name == "interpreter" => native "HashMap.insert" (map, key, value)
            | target.name == "javascript" => native "$(map).get().set($(key),$(value))"
        }
    }
};
const get = forall[K :: type, V :: type] {
    fn(map_arg :: &HashMap[K, V], key_arg :: &K) -> Option[&V] with () {
        cfg_if {
            | target.name == "interpreter" => native "HashMap.get" (map_arg, key_arg)
            | target.name == "javascript" => (
                let map = native "$(map_arg).get()";
                let key = native "$(key_arg).get()";
                if native "$(map).has($(key))" then
                    :Some (&(native "$(map).get($(key))"))
                else
                    :None
            )
        }
    }
};
const size = forall[K :: type, V :: type] {
    fn (map :: &HashMap[K, V]) -> int32 with () {
        cfg_if {
            | target.name == "interpreter" => native "HashMap.size" map
            | target.name == "javascript" => native "$(map).get().size"
        }
    }
};
const into_iter = forall[K :: type, V :: type] {
    fn (map :: HashMap[K, V]) -> () with () { # with generator_handler[K, V]
        cfg_if {
            | target.name == "interpreter" => native "HashMap.into_iter" map
            | target.name == "javascript" => native "new Map()"
        }
    } 
};
