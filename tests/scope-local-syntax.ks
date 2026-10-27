(
    @syntax "foo" 10 @wrap never = a " " "foo" " " b;
    6     foo
    7
);
(
    @syntax "foo" 100 @wrap if_any = a ""/"\n\t" "foo" " " b ""/"\\";
    6     foo
    7;
    # foo smth
);
foo
