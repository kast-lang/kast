impl syntax (arg |> f) = `(
    let _arg = $arg;
    let _f = $f;
    _f _arg
);
impl syntax (f <| arg) = `(
    $f $arg
);
impl syntax (a < b) = `(
    std.cmp.less[_] ($a, $b)
);
impl syntax (a <= b) = `(
    std.cmp.less_or_equal[_] ($a, $b)
);
impl syntax (a == b) = `(
    std.cmp.equal[_] ($a, $b)
);
impl syntax (a != b) = `(
    std.cmp.not_equal[_] ($a, $b)
);
impl syntax (a >= b) = `(
    std.cmp.greater_or_equal[_] ($a, $b)
);
impl syntax (a > b) = `(
    std.cmp.greater[_] ($a, $b)
);
impl syntax (not x) = `(
    if $x then false else true
);
impl syntax (a + b) = `(
    std.op.add ($a, $b)
);
impl syntax (a - b) = `(
    std.op.sub ($a, $b)
);
impl syntax (a * b) = `(
    std.op.mul ($a, $b)
);
impl syntax (a / b) = `(
    std.op.div ($a, $b)
);
impl syntax (a % b) = `(
    std.op.rem ($a, $b)
);
impl syntax (-x) = `(
    std.op.neg $x
);
impl syntax (+x) = `(
    std.op.pos $x
);
impl syntax (a += b) = `(
    let _ref = &mut $a;
    _ref^ = _ref^ + $b
);
impl syntax (a -= b) = `(
    let _ref = &mut $a;
    _ref^ = _ref^ - $b
);
impl syntax (a *= b) = `(
    let _ref = &mut $a;
    _ref^ = _ref^ * $b
);
impl syntax (a /= b) = `(
    let _ref = &mut $a;
    _ref^ = _ref^ / $b
);
impl syntax (a %= b) = `(
    let _ref = &mut $a;
    _ref^ = _ref^ % $b
);
impl syntax (@context ty) = `(
    (@native "create_context_type") $ty
);
impl syntax (loop ( body )) = `(
    unwindable loop_block (
        @comptime with std.LoopBlock = loop_block;
        @loop (
            unwindable loop_body (
                @comptime with std.LoopBody = loop_body;
                $body
            );
        );
    );
);
impl syntax (break) = `(
    unwind (@binding @current std.LoopBlock) ();
);
impl syntax (continue) = `(
    unwind (@binding @current std.LoopBody) ();
);
impl syntax (let rec pattern = value) = `(
    let _mod = (
        module:
        let $pattern = $value
    );
    let $pattern = _mod.$pattern;
);
impl syntax (if cond then stmt) = `(
    if $cond then $stmt else ()
);
impl syntax (while cond do body) = `(
    loop (
        if $cond then $body else break
    )
);
impl syntax (for pattern in iterable do body) = `(
    unwindable loop_block (
        @comptime with std.LoopBlock = loop_block;
        $iterable.iter (
            $pattern => unwindable loop_body (
                @comptime with std.LoopBody = loop_body;
                $body;
            )
        );
    );
);
impl syntax (with_return body) = `(
    unwindable _returnable (
        $body
    )
);
impl syntax (return) = `(
    unwind _returnable ()
);
impl syntax (return value) = `(
    unwind _returnable $value
);
impl syntax (@opaque_type) = `(
    (@native "new_opaque_type") ()
);

impl syntax ([arg] -> body) = `(
    [_ :: $arg] $body
);

impl syntax (start..end) = `(
    std.range.range ($start, $end)
);

impl syntax (if value is pattern then body) = `(
    match $value with (
        | $pattern => $body
        | _ => ()
    )
);
impl syntax (if value is pattern then body else else_body) = `(
    match $value with (
        | $pattern => $body
        | _ => $else_body
    )
);
