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
  a + b
# helo        
)# hello