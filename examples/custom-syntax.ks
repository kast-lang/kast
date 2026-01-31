@syntax "for_once" 10 @wrap never = "for_once" " " elem " " "in" " " iterator " " "do" " " body;
impl syntax (for_once x in a do e) = `(
    # not a real loop
    let $x = $a;
    $e
);
# impl syntax "for" = (.elem = x, .iterator = a, .body = e) => `(same thing);
for_once x in "123" do std.io.print(x);

@syntax "sql" 10 @wrap if_any = "@sql" " " "(" ""/"\n\t" sql:any :: SQL ""/"\\\n" ")";

@syntax SQL."SELECT" 10 @wrap never = "SELECT" " " columns :: SQL " " "FROM" " " table :: SQL;
@syntax SQL."SELECT*" 10 @wrap never = "SELECT" " " "*" " " "FROM" " " table :: SQL;
@syntax SQL."," 20 @wrap never = <- _ "," " " _;
impl syntax (@sql (sql)) = `(
    dbg.print(sql);
);

@sql (SELECT a, b, c FROM MyTable);
@sql (SELECT * FROM MyTable);
