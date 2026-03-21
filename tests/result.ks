use std.*;

const safe_div = (numerator :: Int32, denominator :: Int32) -> Result.t[Int32, String] => (
    if denominator == 0 then (
        :Error "div by zero"
    ) else (
        :Ok (numerator / denominator)
    )
);

const ok = safe_div(49, 7);
const err = safe_div(49, 0);

ok |> dbg.print;
err |> dbg.print;

Result.is_ok(&ok) |> dbg.print;
Result.is_ok(&err) |> dbg.print;
Result.is_err(&ok) |> dbg.print;
Result.is_err(&err) |> dbg.print;

Result.map(ok, op.neg[Int32]) |> dbg.print;
Result.map_err(err, String.length) |> dbg.print;

Result.ok(ok) |> dbg.print;
Result.ok(err) |> dbg.print;
Result.err(ok) |> dbg.print;
Result.err(err) |> dbg.print;

ok |> Result.inspect(num => dbg.print(num^));
ok |> Result.inspect_err(msg => dbg.print(msg^));
err |> Result.inspect(num => dbg.print(num^));
err |> Result.inspect_err(msg => dbg.print(msg^));
ok |> Result.unwrap |> dbg.print;

ok |> Result.and_then(q => safe_div(q, 7)) |> dbg.print;
ok |> Result.and_then(q => safe_div(q, 0)) |> dbg.print;
(ok |> Result.and_then(_ => ok) == ok) |> dbg.print;

(&ok |> Result.as_ref |> Result.unwrap)^ |> dbg.print;
&ok |> Result.as_ref |> Result.as_deref |> Result.unwrap |> dbg.print;

err |> Result.or_else(err => :Ok String.length(err)) |> dbg.print;
err |> Result.unwrap_or_else(String.length) |> dbg.print;
err |> Result.unwrap_or(0) |> dbg.print;

# panic and end
io.print("Going 2 panic now...");
err |> Result.unwrap;
