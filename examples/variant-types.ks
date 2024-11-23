use std.*;

let value = :Some "hello" :: Option[string];

if value is :Some value then (
  print "some";
  print value;
);

match value {
  | :Some _ => (
    print "some";
  )
  | :None => print "none"
};

const either_type :: type = Either[.left = int32, .right = string];
let value_left = either_type of :Left 123;
let value_right :: either_type = :Right "right value";
let check_either = (value :: either_type) => (
    match value {
        | :Left value => print "left"
        | :Right value => print value
    }
);
check_either value_left;
check_either value_right;

const result_type :: type = Result[.ok = (), .error = string];
let unwrap = (result :: result_type) => (
    match result {
        | :Ok value => (
            print "unwrapped successfully";
            value
        )
        | :Error error => panic error
    }
);

:Ok () |> unwrap;
:Error "this is going to panic" |> unwrap;
