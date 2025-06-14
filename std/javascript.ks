module:

const transpile = forall[T] { native "transpile_to_javascript" :: T -> string };

const debug_print = (s :: string) => (
    cfg_if {
        | target.name == "javascript" => native "console.log($(s))"
        | true => ()
    };
);