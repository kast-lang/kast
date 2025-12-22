const Int32 = @native "int32";
const String = @native "string";
const print = (s :: String) -> () => (@native "io.print") s;
const panic = [T] (s :: String) -> T => (@native "panic") s;

const List = (
    module:

    const t = [T] type (
        | :Nil
        | :Cons t[T]
    );
);

const Foo = type (
    .bar :: List.t[Int32],
);

impl Foo as module = (
    module:
    let new = () -> Foo => (
        print "here";
        panic "never"
    );
);

let p = Foo.new ();