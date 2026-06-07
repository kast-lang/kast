let mut sum = 0;

let is_prime = x => with_return (
    for i in 2..x do (
        if x % i == 0 then (
            return false;
        )
    );
    true
);

for x in 2..1000 do (
    if is_prime(x) then (
        sum += x;
    );
);
std.dbg.print(sum);
