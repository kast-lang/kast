let mut sum = 0;

let is_prime = x => with_return (
    for i in 2..x do (
        if x % i == 0 then (
            return false;
        )
    );
    true
);

for x in 2..10000 do (
    if is_prime(x) then (
        sum += x;
    );
);
dbg.print(sum);
