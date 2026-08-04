const Never = newtype (@empty_variant);

const from_never = [T] (_ :: Never) -> T => @cfg (
    | target.name == "interpreter" => @native "never???"
    | target.name == "javascript" => @native "undefined"
    | target.name == "c" => @native "#unreachable"
);
