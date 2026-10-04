const PanicHandlerT = newtype {
    .handle :: &str -> Never,
};
const PanicHandler = @context PanicHandlerT;

const default_panic_handler :: PanicHandlerT = {
    .handle = (s :: &str) -> Never => @cfg (
        | target.name == "interpreter" => (
            (@native "panic")(s)
        )
        | target.name == "c" => (
            @native "default_panic_handler(\(s))";
            @native "#unreachable"
        )
        | target.name == "javascript" => (
            (@native "Kast.panic")(s)
        )
    ),
};

const panic = [T] (s :: &str) -> T => (
    (@current PanicHandler).handle(s) |> from_never
);
