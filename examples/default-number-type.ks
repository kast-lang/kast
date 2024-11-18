(
  comptime with (.default_number_type = _ => std.int32) :: std.default_number_type;
  std.dbg 123;
);

(
  comptime with (.default_number_type = _ => std.int64) :: std.default_number_type;
  std.dbg 123;
);

(
  comptime with (
    .default_number_type = s => (
      if std.contains (.s, .substring=".") then
        std.float64
      else
        std.int32
    ),
  ) :: std.default_number_type;

  std.dbg 123;
  std.dbg 123.0;
);

if false then (
  # no default - this is going to fail
  std.dbg 123;
);
