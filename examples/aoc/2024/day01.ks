 #!/usr/bin/env kast
use std.prelude.*;
std.sys.chdir (std.path.dirname __FILE__);
let input = std.fs.read_file "day01.input.txt";
std.String.lines (
    input,
    line => (
        print line;
        print "==="
    ),
);
# let mut a :: list[int32] = list[];
# let mut b :: list[int32] = list[];
# for (line :: string) in lines(input) {
#     let mut i :: int32 = 0;
#     # dbg line;
#     for (item :: string) in split_whitespace(line) {
#         if i == 0 then (
#             a = list_push(a, parse item);
#         ) else (
#             b = list_push(b, parse item);
#         );
#         i = i + 1;
#     }
# };
# print "read input";
# a = sort a;
# b = sort b;
# print "sorted input";
# let mut i = 0;
# let mut result = 0;
# loop {
#     if i >= list_length a then (
#         break;
#     );
#     result = result + abs(list_get (a, i) - list_get (b, i));
#     i = i + 1;
# };
# dbg result;
