module:
const int32 :: type = native "int32";
const string :: type = native "string";
const print :: string with output -> () = native "print";
const get_random_int32 :: int32 with rng -> int32 = native "rng";
const int32_to_string :: int32 -> string = native "int32_to_string";