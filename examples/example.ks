syntax custom 5 wrap never = "hello" "," " " "world";
hello, world;
syntax sql 5 wrap never = "sql" "(" _ ")";
# sql(SELECT * FROM Table WHERE column > 2);
if a then b else c;
if true then print "hello, world" else print "hello, world (but false)";
(
  (# if a then b else if a then b else if a then (
    b
  ) else #)
  if a then b else if a then b else c
);
if true then (
  nothing
) else (
  if (
    a or b
  ) and (c or d) then (
    f (a, b, c, d, e, f, g);
    (# SOME COMMENT #) print (# SOME COMMENT #) "true";
    # comment
    print "true"# before semicolon
    ;
    foo 5;
    syntax foo 10 wrap never = "foo" " " _;
    foo 5;
    print "true";
    # hello
    print "hi" "true";
    print (
      a * b
      + c / d
    );
    a
      .method ()
      .method ()
      .method ()
      #.method () 
      .method ()
      (# .method () 
      .method () #)
      .method ();
    a
      |> b ()
      |> c ()
      |> d ();
    ()
  ) else (
    print "false";
  )
);
(
  foo 5;
  a + b
# helo        
)
