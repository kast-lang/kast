module:

const physically_equal = [T] (a :: T, b :: T) -> Bool => @cfg (
    | target.name == "interpreter" => (@native "repr.physically_equal")(a, b)
    | target.name == "javascript" => (@native "Kast.physically_equal")(a, b)
);
const structurally_equal = [T] (a :: T, b :: T) -> Bool => @cfg (
    | target.name == "interpreter" => (@native "repr.structurally_equal")(a, b)
    | target.name == "javascript" => (@native "Kast.structurally_equal")(a, b)
);