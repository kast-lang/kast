use std.*;

let assert_eq = fn(actual :: int32, expected :: int32) {
    if actual != expected then (
        dbg actual;
        dbg expected;
        panic "assertion failed";
    );
};

let sum_from_1_to = fn(n :: int32) -> int32 {
    (1 + n) * n / 2
};

let difference_of_sums = fn(n :: int32, m :: int32) -> int32 {
    # num2 = m + 2m + 3m + ... <= n
    let num2 = sum_from_1_to(n / m) * m;
    let num1 = sum_from_1_to(n) - num2;
    num1 - num2
};

# assert_eq (difference_of_sums(10, 3), 19);
# assert_eq (difference_of_sums(5, 6), 15);
# assert_eq (difference_of_sums(5, 1), -15);

let js_code :: string = std.javascript.transpile difference_of_sums;
print "var differenceOfSums=";
print &js_code;

let test = fn (value :: (int32, int32)) {
    print "console.log(differenceOfSums({},";
    print &(std.javascript.transpile value);
    print "))";
};

test (10, 3);
test (5, 6);
test (5, 1);

print "let f=differenceOfSums";
print "differenceOfSums=(a, b)=>f({},{\"0\":a,\"1\":b})";

