impl syntax (arg |> f) = `(
    let arg = $arg;
    let f = $f;
    f arg
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
impl syntax (@context ty) = `(
    (@native "create_context_type") $ty
);
impl syntax (loop ( body )) = `(
    unwindable block (
        @comptime with std.loop_block = block;
        @loop (
            $body
        );
    );
);
impl syntax (break) = `(
    unwind (@binding @current std.loop_block) ();
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
impl syntax (a += b) = `(
    $a = $a + $b
);
impl syntax (a -= b) = `(
    $a = $a - $b
);
impl syntax (a *= b) = `(
    $a = $a * $b
);
impl syntax (a /= b) = `(
    $a = $a / $b
);
impl syntax (while cond do body) = `(
    loop (
        if $cond then $body else break
    )
);