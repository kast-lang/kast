# let Distribution = trait (
#     gen_type : type,
#     gen : void -> gen_type,
# );
# 
# let random :: forall D where (D implements Distribution). (void -> D.gen_type) =
#     forall D. D.gen;
# 
# let Parse = trait (
#     parse: string -> self
# );
# 
# let parse :: forall T where (T implements Parse). string -> T = forall T. T.parse;

# discard (T :: type) =>
#     (x :: T) =>
#         x;

# TODO: syntax highglighting extension for VScode

const identity_type = forall (T :: type). T -> T;

let identity =
    forall (T :: type). (x :: T => x);

# let int32_identity = identity[int32];

let generic_dbg = forall T. (value :: T => dbg value);

const foo = forall (T :: type). ( a : T, b : T );
let foo_value :: foo[int32] = ( a : 1, b : 2);
dbg foo_value;