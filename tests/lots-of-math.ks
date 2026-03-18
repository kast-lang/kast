
let mut sum = 0;

let is_prime = x => (
    unwindable block (
        for i in 2..x do (
            if x % i == 0 then (
                unwind block false;
            )
        );
        true
    )
);

for x in 2..30000 do (
    if is_prime(x) then (
        sum += x;
    );
);
std.dbg.print(sum);
