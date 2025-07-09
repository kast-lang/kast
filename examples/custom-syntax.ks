syntax for 10 wrap never = "for" " " elem " " "in" " " iterator " " "do" " " body;
impl syntax (for x in a do e) = `(
    # not a real loop
    let $x = $a;
    $e
);
impl syntax "for" = (.elem = x, .iterator = a, .body = e) => `(same thing);
for x in "123" do std.io.print x
