module:

const physically_equal = [T] (a :: T, b :: T) -> Bool => @cfg (
    | target.name == "interpreter" => (@native "repr.physically_equal")(a, b)
    | target.name == "c" => @native "memcmp(\(&a), \(&b), sizeof(\(type T))) == 0"
    | target.name == "javascript" => (@native "Kast.physically_equal")(a, b)
);
const structurally_equal = [T] (a :: T, b :: T) -> Bool => @cfg (
    | target.name == "interpreter" => (@native "repr.structurally_equal")(a, b)
    | target.name == "c" => physically_equal(a, b) # TODO
    | target.name == "javascript" => (@native "Kast.structurally_equal")(a, b)
);
