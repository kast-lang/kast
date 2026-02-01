@syntax "sql" 10 @wrap if_any = "@sql" " " "(" ""/"\n\t" sql:any :: SQL ""/"\\\n" ")";

@syntax SQL."SELECT" 10 @wrap never = "SELECT" " " columns " " "FROM" " " table;

@syntax SQL."SELECT*" 10 @wrap never = "SELECT" " " "*" " " "FROM" " " table;
@syntax SQL."," 20 @wrap never = <- _ "," " " _;

@syntax SQL."kast" 20 @wrap never = "$" _: >=10 :: GLOBAL;

impl syntax (@sql (sql)) = `(
    dbg.print(sql);
);
