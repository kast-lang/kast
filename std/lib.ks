module:

const @"syntax" = import "./syntax.ks";

impl syntax @"syntax".invoke_macro = macro (.@"macro", .arg) => `(
    include_ast ($@"macro" !! `($$arg))
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

# const type = native "type";
const ast :: type = native "ast";
const expr :: type = native "expr";

const eval_ast = forall[T] { native "eval_ast" :: ast -> T };
const compile_ast = forall[T] { native "compile_ast" :: ast -> T };

const bool :: type = native "bool";

impl syntax @"syntax".@"true" = macro _ => `(native "true");
impl syntax @"syntax".@"false" = macro _ => `(native "false");

# TODO where T: Num or smth
const @"op unary +" = forall[T] { (x :: T,) => x };
impl syntax @"syntax".@"op unary +" = macro (x,) => `(@"op unary +" $x);
const @"op unary -" = forall[T] { native "unary -" :: T -> T };
impl syntax @"syntax".@"op unary -" = macro (x,) => `(@"op unary -" $x);

# nobody's using spacetimedb, but maybe they should
impl syntax @"syntax".@"op binary +" = macro (.lhs, .rhs) => `(@"op binary +" (.lhs = $lhs, .rhs = $rhs));
impl syntax @"syntax".@"op binary -" = macro (.lhs, .rhs) => `(@"op binary -" (.lhs = $lhs, .rhs = $rhs));
impl syntax @"syntax".@"op binary *" = macro (.lhs, .rhs) => `(@"op binary *" (.lhs = $lhs, .rhs = $rhs));
impl syntax @"syntax".@"op binary /" = macro (.lhs, .rhs) => `(@"op binary /" (.lhs = $lhs, .rhs = $rhs));
impl syntax @"syntax".@"op binary %" = macro (.lhs, .rhs) => `(@"op binary %" (.lhs = $lhs, .rhs = $rhs));

impl syntax @"syntax".@"op binary <" = macro (.lhs, .rhs) => `(
    @"op binary <"(.lhs = & $lhs, .rhs = & $rhs)
);
impl syntax @"syntax".@"op binary <=" = macro (.lhs, .rhs) => `(
    @"op binary <="(.lhs = & $lhs, .rhs = & $rhs)
);
impl syntax @"syntax".@"op binary ==" = macro (.lhs, .rhs) => `(
    @"op binary =="(.lhs = & $lhs, .rhs = & $rhs)
);
impl syntax @"syntax".@"op binary !=" = macro (.lhs, .rhs) => `(
    @"op binary !="(.lhs = & $lhs, .rhs = & $rhs)
);
impl syntax @"syntax".@"op binary >=" = macro (.lhs, .rhs) => `(
    @"op binary >="(.lhs = & $lhs, .rhs = & $rhs)
);
impl syntax @"syntax".@"op binary >" = macro (.lhs, .rhs) => `(
    @"op binary >"(.lhs = & $lhs, .rhs = & $rhs)
);
const @"op binary <" = forall[T] {
    cfg_if {
        | target.name == "interpreter" => native "<"
        | target.name == "javascript" => (.lhs, .rhs) => ( native "($(lhs).get()<$(rhs).get())" )
    } :: (.lhs = &T, .rhs = &T) -> bool
};
const @"op binary <=" = forall[T] {
    cfg_if {
        | target.name == "interpreter" => native "<="
        | target.name == "javascript" => (.lhs, .rhs) => ( native "($(lhs).get()<=$(rhs).get())" )
    } :: (.lhs = &T, .rhs = &T) -> bool
};
const @"op binary ==" = forall[T] {
    cfg_if {
        # Can't have normal target.name == "interpreter" because it will recurse
        | native "==" (.lhs = &target.name, .rhs = &"interpreter") => native "=="
        | target.name == "javascript" => (.lhs, .rhs) => ( native "($(lhs).get()==$(rhs).get())" )
    } :: (.lhs = &T, .rhs = &T) -> bool
};
const @"op binary !=" = forall[T] {
    cfg_if {
        | target.name == "interpreter" => native "!="
        | target.name == "javascript" => (.lhs, .rhs) => ( native "($(lhs).get()!=$(rhs).get())" )
    } :: (.lhs = &T, .rhs = &T) -> bool
};
const @"op binary >=" = forall[T] {
    cfg_if {
        | target.name == "interpreter" => native ">="
        | target.name == "javascript" => (.lhs, .rhs) => ( native "($(lhs).get()>=$(rhs).get())" )
    } :: (.lhs = &T, .rhs = &T) -> bool
};
const @"op binary >" = forall[T] {
    cfg_if {
        | target.name == "interpreter" => native ">"
        | target.name == "javascript" => (.lhs, .rhs) => ( native "($(lhs).get()>$(rhs).get())" )
    } :: (.lhs = &T, .rhs = &T) -> bool
};

impl syntax @"syntax".@"op +=" = macro (.target, .value) => `($target = $target + $value);
impl syntax @"syntax".@"op -=" = macro (.target, .value) => `($target = $target - $value);
impl syntax @"syntax".@"op *=" = macro (.target, .value) => `($target = $target * $value);
impl syntax @"syntax".@"op /=" = macro (.target, .value) => `($target = $target / $value);
impl syntax @"syntax".@"op %=" = macro (.target, .value) => `($target = $target % $value);

const @"op binary +" = forall[T] {
    cfg_if {
        | target.name == "interpreter" => native "+"
        | target.name == "javascript" => (.lhs, .rhs) => ( native "($(lhs)+$(rhs))" )
    } :: (.lhs = T, .rhs = T) -> T
};
const @"op binary -" = forall[T] {
    cfg_if {
        | target.name == "interpreter" => native "-"
        | target.name == "javascript" => (.lhs, .rhs) => ( native "($(lhs)-$(rhs))" )
    } :: (.lhs = T, .rhs = T) -> T
};
const @"op binary *" = forall[T] {
    cfg_if {
        | target.name == "interpreter" => native "*"
        | target.name == "javascript" => (.lhs, .rhs) => ( native "($(lhs)*$(rhs))" )
    } :: (.lhs = T, .rhs = T) -> T
};
const @"op binary /" = forall[T] {
    cfg_if {
        | target.name == "interpreter" => native "/"
        | target.name == "javascript" => (.lhs, .rhs) => (
            # TODO its based on T
            native "Math.trunc($(lhs)/$(rhs))"
        )
    } :: (.lhs = T, .rhs = T) -> T
};
const @"op binary %" = forall[T] {
    cfg_if {
        | target.name == "interpreter" => native "%"
        | target.name == "javascript" => (.lhs, .rhs) => ( native "($(lhs)%$(rhs))" )
    } :: (.lhs = T, .rhs = T) -> T
};

const @"not" :: bool -> bool = x => cfg_if {
    | target.name == "interpreter" => native "not" x
    | target.name == "javascript" => native "!$(x)"
};
impl syntax @"syntax".@"not" = macro (e,) => `(@"not" $e);

const int32 :: type = native "int32";
const int64 :: type = native "int64";
const float64 :: type = native "float64";
const char :: type = native "char";
const string :: type = native "string";

const output :: type = native "output";
# const output :: type = (
#     .write = &string -> (),
# );
const input :: type = (
    .read_line = () -> string with (),
);
native "set_native" (.name = "input", .value = input);

const default_number_type :: type = native "default_number_type";
let default_number_type_based_on_dot :: default_number_type = (
    # TODO should return Option[type]
    .default_number_type = s => (
      if contains (.s, .substring=".") then
        float64
      else
        int32
    ),
);

# TODO panic should return never
const panic = fn(msg :: string) {
    cfg_if {
        | target.name == "interpreter" => native "panic" msg
        | target.name == "javascript" => native "(()=>{throw $(msg);})()"
    }
};

const print :: &string -> () with output = line => (
    let output = current output;
    output.write line;
    output.write &"\n";
);

const read_line :: () -> string with input = () => (
    (current input).read_line ()
);

const filesystem :: type = native "filesystem";

const read_file :: string -> string with filesystem = fn (path) {
    (current filesystem).read_file path
};

const contains :: (.s = string, .substring = string) -> bool = native "contains";

const dbg = forall[T] {
    (value :: T) => (
        let output = current output;
        cfg_if {
            | target.name == "interpreter" => (
                output.write <| &(native "dbg" value);
                output.write &" :: ";
                output.write <| &(native "dbg_type" T);
                output.write &"\n";
            )
            | target.name == "javascript" => (
                output.write &(native "require('node:util').format('%o',$(value))");
                output.write "\n";
                # native "console.log($(value))";
            )
        }
    )
};

# TODO T: randomizable
const random = forall[T] {
    fn (.min :: T, .max :: T) -> T {
        cfg_if {
            | target.name == "interpreter" => native "random" (.min, .max)
            | target.name == "javascript" => (
                # TODO not only ints
                let len = max - min;
                native "Math.floor(Math.random() * $(len) + $(min))"
            )
        }
    }
};

# TODO `:: type` should be inferred
const Option = forall[T :: type] { newtype :Some T | :None };
native "set_native" (.name = "Option", .value = Option);

const Either = forall[.left :: type, .right :: type] { newtype :Left left | :Right right };

const Result = forall[.ok :: type, .error :: type] { newtype :Ok ok | :Error error };

const TypeName = (.name = string) as type;

impl int32 as TypeName = (.name = "int32");

const Copy = forall[Self :: type] { () };

native "set_native" (.name = "Copy", .value = Copy);

impl int32 as Copy = ();
impl int64 as Copy = ();
impl float64 as Copy = ();
impl bool as Copy = ();
impl char as Copy = ();
impl type as Copy = ();

# TODO Clone trait
const clone = forall[T] {
    fn (x :: &T) -> T {
        native "clone" x
    }
};

const Parse = forall[Self] {
    .parse = &string -> Self,
};

impl int32 as Parse = (
    .parse = fn(s :: &string) -> int32 {
        cfg_if {
            | target.name == "interpreter" => native "parse" s
            | target.name == "javascript" => native "parseInt($(s).get())"
        }
    },
);

impl int64 as Parse = (
    .parse = native "parse",
);

impl float64 as Parse = (
    .parse = native "parse",
);

const parse = forall[T] {
    s => (T as Parse).parse s
};

const generator_handler = forall[T :: type] { # TODO contexts
    (.handle = T -> () with output) :: type
};
native "set_native" (.name = "generator_handler", .value = generator_handler);

const loop_context = forall[T] {
    (
        .@"break" = T -> () with (), # TODO never
        .@"continue" = () -> () with (),
    ) :: type
};

impl syntax @"syntax".@"loop" = macro (.body) => `(
    unwindable for_loop (
        let body = () => (
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
        cfg_if {
            | target.name == "interpreter" => native "loop" body
            | target.name == "javascript" => native "await(async function(){for(;;)await $(body)(ctx)})()"
        }
    )
);

impl syntax @"syntax".@"while" = macro (.cond, .body) => `(
    loop {
        if $cond then $body else (break);
    }
);

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

impl syntax @"syntax".@"continue" = macro _ => `(
    (current loop_context[()]).@"continue" () # TODO infer loop context arg
);

const char_ord :: char -> int32 = native "char_ord";
const chars :: &string -> () with generator_handler[char] = native "chars";
const push_char :: (string, char) -> string = native "push_char";

const exec_mode = native "exec_mode";

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

const collections = include "./collections/_mod.ks";
const time = include "./time.ks";
const javascript = include "./javascript.ks";
const web = include "./web/_mod.ks";

const prelude = (
    # TODO use type, bool, int32, int64, float64, char, string;
    # .type = type,
    .bool = bool,
    .int32 = int32,
    .int64 = int64,
    .float64 = float64,
    .char = char,
    .string = string,
    
    .print = print,

    .Option = Option,
    .Either = Either,
    .Result = Result,
    
    .panic = panic,
    .dbg = dbg,

    .List = collections.List,
    .HashMap = collections.HashMap,
);
