@syntax from_scratch;
@syntax "comma" 1 @wrap if_any_assoc = <- _ "," " "/"\n" _;
@syntax "field" 2 @wrap never = name ":" " " value;
@syntax "obj" 10 @wrap if_any = "{" " "/"\n\t" _:any " "/"\\\n" "}";
@syntax "array" 10 @wrap if_any = "[" " "/"\n\t" _:any " "/"\\\n" "]";
@syntax "null" 100 @wrap if_any = "null";
@syntax "true" 100 @wrap if_any = "true";
@syntax "false" 100 @wrap if_any = "false";
@syntax "negative" 100 @wrap if_any = "-" _;

