@syntax "comma" 1 @wrap if_any_assoc = <- _ "," " "/"\n" _;
@syntax "field" 2 @wrap never = name ":" " " value;
@syntax "obj" 10 @wrap if_any = "{" " "/"\n\t" fields:any " "/"\\\n" "}";
@syntax "list" 10 @wrap if_any = "[" " "/"\n\t" elements:any " "/"\\\n" "]";
