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
impl syntax (-x) = `(
    std.op.neg $x
);
impl syntax (+x) = `(
    std.op.pos $x
);
impl syntax (a += b) = `(
    let _ref = &$a;
    _ref^ = _ref^ + $b
);
impl syntax (a -= b) = `(
    let _ref = &$a;
    _ref^ = _ref^ - $b
);
impl syntax (a *= b) = `(
    let _ref = &$a;
    _ref^ = _ref^ * $b
);
impl syntax (a /= b) = `(
    let _ref = &$a;
    _ref^ = _ref^ / $b
);
impl syntax (@context ty) = `(
    (@native "create_context_type") $ty
);
impl syntax (loop ( body )) = `(
    unwindable loop_block (
        @comptime with std.loop_block = loop_block;
        @loop (
            unwindable loop_body (
                @comptime with std.loop_body = loop_body;
                $body
            );
        );
    );
);
impl syntax (break) = `(
    unwind (@binding @current std.loop_block) ();
);
impl syntax (continue) = `(
    unwind (@binding @current std.loop_body) ();
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
impl syntax (for pattern in start..end do body) = `(
    let _loop_var = $start;
    while _loop_var < $end do (
        unwindable loop_body (
            @comptime with std.loop_body = loop_body;
            let $pattern = _loop_var;
            $body;
        );
        _loop_var += 1;
    )
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
