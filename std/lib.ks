module:

const @"syntax" = import "./syntax.ks";

impl syntax @"syntax".invoke_macro = macro (.@"macro", .arg) => `(
  compile_ast ($@"macro" !! `($arg))
);

impl syntax @"syntax".pipe_right = macro (.arg, .f) => `(
  let arg = $arg;
  let f = $f;
  f arg
);
impl syntax @"syntax".pipe_left = macro (.f, .arg) => `(
  let f = $f;
  let arg = $arg;
  f arg
);
impl syntax @"syntax".let_infer = macro (.pattern) => `(
  (let $pattern = _; $pattern)
);

const type = native "type";
const ast :: type = native "ast";

const bool :: type = native "bool";

impl syntax @"syntax".@"true" = macro _ => `(native "true");
impl syntax @"syntax".@"false" = macro _ => `(native "false");

const int32 :: type = native "int32";
const int64 :: type = native "int64";
const float64 :: type = native "float64";
const char :: type = native "char";
const string :: type = native "string";

const output :: type = native "output";

const default_number_type :: type = native "default_number_type";

# TODO panic should return never
const panic :: string -> () = native "panic";

const print :: string -> () with output = line => (
  let output = current output;
  output.write line;
  output.write "\n";
);

const filesystem :: type = native "filesystem";

const read_file :: string -> string with filesystem = fn (path) {
    (current filesystem).read_file path
};

const contains :: (.s = string, .substring = string) -> bool = native "contains";

const dbg = forall[T] {
  (value :: T) => (
    let output = current output;
    output.write <| native "dbg" value;
    output.write " :: ";
    output.write <| native "dbg_type" T;
    output.write "\n";
  )
};

# TODO `:: type` should be inferred
const Option = forall[T :: type] { newtype :Some T | :None };

const Either = forall[.left :: type, .right :: type] { newtype :Left left | :Right right };

const Result = forall[.ok :: type, .error :: type] { newtype :Ok ok | :Error error };

# TODO where T: Num or smth
impl syntax @"syntax".@"op binary +" = forall[T] { native "+" :: (.lhs = T, .rhs = T) -> T };
impl syntax @"syntax".@"op binary -" = forall[T] { native "-" :: (.lhs = T, .rhs = T) -> T };
impl syntax @"syntax".@"op binary *" = forall[T] { native "*" :: (.lhs = T, .rhs = T) -> T };
impl syntax @"syntax".@"op binary /" = forall[T] { native "/" :: (.lhs = T, .rhs = T) -> T };
impl syntax @"syntax".@"op binary %" = forall[T] { native "%" :: (.lhs = T, .rhs = T) -> T };

impl syntax @"syntax".@"op binary <" = forall[T] { native "<" :: (.lhs = T, .rhs = T) -> bool };
impl syntax @"syntax".@"op binary <=" = forall[T] { native "<=" :: (.lhs = T, .rhs = T) -> bool };
impl syntax @"syntax".@"op binary ==" = forall[T] { native "==" :: (.lhs = T, .rhs = T) -> bool };
impl syntax @"syntax".@"op binary !=" = forall[T] { native "!=" :: (.lhs = T, .rhs = T) -> bool };
impl syntax @"syntax".@"op binary >=" = forall[T] { native ">=" :: (.lhs = T, .rhs = T) -> bool };
impl syntax @"syntax".@"op binary >" = forall[T] { native ">" :: (.lhs = T, .rhs = T) -> bool };

const TypeName = (.name = string) as type;

impl int32 as TypeName = (.name = "int32");

const Parse = forall[Self] {
  .parse = string -> Self,
};

impl int32 as Parse = (
  .parse = native "parse",
);

impl int64 as Parse = (
  .parse = native "parse",
);

impl float64 as Parse = (
  .parse = native "parse",
);

const parse = forall[T] {
  (T as Parse).parse
};

const generator_handler = forall[T] {
  (.handle = T -> () with ()) :: type
};

const loop_context = forall[T] {
  (
    .@"break" = T -> () with (), # TODO never
    .@"continue" = () -> () with (),
  ) :: type
};

impl syntax @"syntax".for_loop = macro (.value_pattern, .generator, .body) => `(
  unwindable for_loop (
    let handler = $value_pattern => (
      const BodyResult = forall[T] { newtype :Break T | :Continue };
      let body_result :: BodyResult[_] = unwindable body (
        with (
          .@"break" = () => unwind body (:Break ()),
          .@"continue" = () => unwind body (:Continue),
        ) :: loop_context[()];
        $body;
        :Continue
      );
      match body_result {
        | :Break value => unwind for_loop value
        | :Continue => ()
      };
    );
    with (.handle = handler) :: generator_handler[_];
    $generator;
  )
);

impl syntax @"syntax".@"yield" = macro (.value) => `(
  (current generator_handler[_]).handle $value
);

impl syntax @"syntax".break_without_value = macro _ => `(
  break ()
);

impl syntax @"syntax".break_with_value = macro (.value) => `(
  (current loop_context[_]).@"break" $value
);

#trait Iterator {
#    type Item;
#    fn next(&mut self) -> Option<<Self as Iterator>::Item>;
#}

/*
const Iterator = forall[Self] {
    .Item = type,
    .next = Self -> (Self as Iterator).Item,
};

/*
const generator = forall[Self] {
    rec (
        let item = type;
        let generate = Self -> () with generator_handler[item];
    )
};

impl string as generator = (
    .item = char,
    .generate :: string -> () with generator_handler[char] = native "iterate_over_string",
);
*/
