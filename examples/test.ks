const x :: Int32 = 517533251;
const y :: Int64 = @eval (
    let mut x = 1;
    for i in 0..18 do (
        x *= 10;
        x += i |> to_string |> parse;
    );
    x
);
dbg.print(.x, .y, .y_div_10 = y / 10);
