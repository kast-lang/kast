# const range = mut n => {
#     .iter = consumer => (
#         unwindable foo (
#             @loop (
#                 unwindable foo_body (
#                     if n > 0 then (
#                         consumer(n);
#                         n -= 1;
#                     ) else (
#                         unwind foo ();
#                     )
#                 )
#             )
#         );
#     ),
# };

# for x in range(10) do (
#     dbg.print(x);
# );

dbg.print(std.sys.get_env("FOO") |> Option.unwrap);