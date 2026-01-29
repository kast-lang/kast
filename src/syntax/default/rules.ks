@syntax from_scratch;
@syntax "core:module" 0 @wrap always = "module" ":" "\n" _ ->;
@syntax "core:then" 0 @wrap always = _ ";" "\n" _ ->;
@syntax "core:stmt" 0 @wrap always = _ ";";
@syntax "core:use .*" 1 @wrap never = "use" " " _ ".*";
@syntax "core:use" 1 @wrap never = "use" " " _;
@syntax "core:impl syntax" 1 @wrap never = "impl" " " "syntax" " " name " " "=" " " impl;
@syntax "core:impl_cast" 1 @wrap never = "impl" " " value " " "as" " " target " " "=" " " impl;
@syntax "core:impl_as_module" 1 @wrap never = "impl" " " value " " "as" " " "module" " " "=" " " impl;
@syntax "core:assign" 2 @wrap never = assignee " " "=" " " value;
@syntax "core:const" 2 @wrap never = "const" " " pattern " " "=" " " value;
@syntax "add_assign" 2 @wrap never = _ " " "+=" " " _;
@syntax "sub_assign" 2 @wrap never = _ " " "-=" " " _;
@syntax "mul_assign" 2 @wrap never = _ " " "*=" " " _;
@syntax "div_assign" 2 @wrap never = _ " " "/=" " " _;
@syntax "rem_assign" 2 @wrap never = _ " " "%=" " " _;
@syntax "core:let" 3 @wrap never = "let" " " pattern;
@syntax "let rec" 3 @wrap never = "let" " " "rec" " " pattern " " "=" " " value;

@syntax "core:leading union" 3.99 @wrap never = "|" " " _;
@syntax "core:union" 4 @wrap if_any_assoc = <- _ " "/"\n" "|" " " _;

@syntax "core:comma" 5 @wrap if_any_assoc = <- _ "," " "/"\n" _;
@syntax "core:trailing comma" 5 @wrap never = <- _ ",";

@syntax "core:unpack" 5.5 @wrap never = "..." _;

@syntax "core:field init" 6 @wrap never = "." label type=(" " "::" " " _)? value=(" " "=" " " _)?;

@syntax "core:type ascribe" 6.2 @wrap never = expr " " "::" " " type;
@syntax "prefix type ascribe" 6.2 @wrap never = _=(@wrap if_any "(" "::" " "/"\n\t" type:any ""/"\\\n" ")") " " expr;

@syntax "core:generic" 6.5 @wrap never = "[" ""/"\n\t" arg:any ""/"\\\n" "]" " " body ->;
@syntax "generic_non_dependent" 6.5 @wrap never = "[" ""/"\n\t" arg:any ""/"\\\n" "]" " " "->" " " body ->;
@syntax "core:fn_type" 6.5 @wrap never = arg " " context_args=("with" " " _ " ")? result=("->" " " _ _=(" " "with" " " context " ")?);

@syntax "core:fn" 7 @wrap never = arg " " context_args=("with" " " _ " ")? result=("->" " " _ " " _=("with" " " context " ")?)? "=>" " " body;

@syntax "if_without_else" 7.5 @wrap never = "if" " " cond " " "then" " " then_case;
@syntax "core:if" 7.5 @wrap never = "if" " " cond " " "then" " " then_case " " "else" " " else_case ->;
@syntax "if_is" 7.5 @wrap never = "if" " " value " " "is" " " pattern " " "then" " " body;
@syntax "if_is_else" 7.5 @wrap never = "if" " " value " " "is" " " pattern " " "then" " " body " " "else" " " else_body ->;
@syntax "core:match" 7.5 @wrap if_any = "match" " " value " " "with" " " "(" " "/"\n\t" branches:any " "/"\\\n" ")";
@syntax "while" 7.5 @wrap never = "while" " " cond " " "do" " " body;
@syntax "for" 7.5 @wrap never = "for" " " pattern " " "in" " " iterable " " "do" " " body;

@syntax "range" 8 @wrap never = start ".." end;

@syntax "core:import" 9 @wrap never = "import" " " path;
@syntax "core:include" 9 @wrap never = "include" " " path;
@syntax "core:include_ast" 9 @wrap never = "include_ast" " " _;
@syntax "create_context_type" 9 @wrap never = "@context" " " type;
@syntax "core:comptime" 9 @wrap never = "@eval" " " _;
@syntax "core:no_hygiene" 9 @wrap never = "@no_hygiene" " " _;
@syntax "core:binding" 9 @wrap never = "@binding" " " _;
@syntax "core:native" 9 @wrap never = "@native" " " _;
@syntax "core:inject_context" 9.5 @wrap never = "with" " " context_type " " "=" " " value;
@syntax "core:current_context" 9.5 @wrap never = "@current" " " context_type;
@syntax "core:or" 10 @wrap if_any_assoc = <- _ " "/"\n" "or" " " _;
@syntax "core:and" 11 @wrap if_any_assoc = <- _ " "/"\n" "and" " " _;
@syntax "not" 12 @wrap never = "not" " " _;
@syntax "<" 14 @wrap never = _ " " "<" " " _;
@syntax "<=" 14 @wrap never = _ " " "<=" " " _;
@syntax "==" 14 @wrap never = _ " " "==" " " _;
@syntax "!=" 14 @wrap never = _ " " "!=" " " _;
@syntax ">=" 14 @wrap never = _ " " ">=" " " _;
@syntax ">" 14 @wrap never = _ " " ">" " " _;

@syntax "|>" 14.5 @wrap if_any_assoc = <- arg " "/"\n\t" "|>" " " f ""/"\\";
@syntax "|>()" 14.5 @wrap if_any_assoc = <- first_arg " "/"\n\t" "|>" " " f _=(@wrap if_any "(" ""/"\n\t" args:any ""/"\\\n" ")") ""/"\\";
@syntax "<|" 14.5 @wrap if_any_assoc = <- f " "/"\n\t" "<|" " " arg ""/"\\";

@syntax "+" 15 @wrap if_any_assoc = <- _ " "/"\n" "+" " " _;
@syntax "-" 15 @wrap if_any_assoc = <- _ " "/"\n" "-" " " _;
@syntax "*" 17 @wrap if_any_assoc = <- _ " "/"\n" "*" " " _;
@syntax "/" 17 @wrap if_any_assoc = <- _ " "/"\n" "/" " " _;
@syntax "%" 17 @wrap if_any_assoc = <- _ " "/"\n" "%" " " _;

@syntax "return_without_value" 40 @wrap never = "return";
@syntax "return_with_value" 40 @wrap never = "return" " " value;
@syntax "core:variant_without_value" 42 @wrap never = ":" label: >=1000;
@syntax "core:variant" 42 @wrap never = ":" label: >=1000 " " value ->;
@syntax "unary -" 45 @wrap never = "-" _;
@syntax "unary +" 45 @wrap never = "+" _;
@syntax "with_return" 60 @wrap never = "with_return" " " body;
@syntax "core:cast" 60.5 @wrap never = value " " "as" " " target;
@syntax "core:ref" 61 @wrap never = "&" _ ->;
@syntax "core:ref_mut" 61 @wrap never = "&" "mut" " " _ ->;
@syntax "core:instantiate_generic" 70 @wrap never = <- generic _=("[" ""/"\n\t" arg:any ""/"\\\n" "]");
@syntax "core:." 70 @wrap never = <- obj ""/"\n\t" "." field ""/"\\";
@syntax "core:deref" 70 @wrap never = <- _ "^";
@syntax "core:apply" 70 @wrap never = <- f _=(@wrap if_any "(" ""/"\n\t" arg:any ""/"\\\n" ")");
@syntax "invoke_macro" 70 @wrap never = <- macro "!" _=(@wrap if_any "(" ""/"\n\t" ast:any ""/"\\\n" ")");
@syntax "core:unwindable" 80 @wrap never = "unwindable" " " token " " body;
@syntax "core:unwind" 80 @wrap never = "unwind" " " token " " value;
@syntax "core:mut" 500 @wrap never = "mut" " " _;
@syntax "core:type expr" 500 @wrap never = "type" " " _;
@syntax "core:newtype" 500 @wrap never = "newtype" " " _;
@syntax "core:typeof" 500 @wrap never = "typeof" " " _;
# @syntax "core:type" 500 @wrap never = "type";
@syntax "core:true" 500 @wrap never = "true";
@syntax "core:false" 500 @wrap never = "false";
@syntax "loop" 500 @wrap if_any = "loop" " " "(" " "/"\n\t" _:any " "/"\\\n" ")";
@syntax "break" 500 @wrap never = "break";
@syntax "continue" 500 @wrap never = "continue";
@syntax "core:by_ref" 500 @wrap never = "ref" " " _;
@syntax "core:by_ref_mut" 500 @wrap never = "ref" " " "mut" " " _;
@syntax "opaque_type" 500 @wrap never = "@opaque_type";
@syntax "core:loop" 500 @wrap if_any = "@loop" " " "(" " "/"\n\t" _:any " "/"\\\n" ")";
@syntax "core:scope" 1000 @wrap if_any = "(" ""/"\n\t" _:any ""/"\\\n" ")";
@syntax "core:record" 1000 @wrap if_any = "{" " "/"\n\t" _:any " "/"\\\n" "}";
@syntax "core:placeholder" 1000 @wrap never = "_";
@syntax "core:quote" 1000 @wrap if_any = "`" "(" ""/"\n\t" _:any ""/"\\\n" ")";
@syntax "core:unquote" 1000 @wrap never = "$" _ ->;
@syntax "core:target_dependent" 1000 @wrap always = "@cfg" " " "(" ""/"\n\t" branches:any ""/"\\\n" ")";
@syntax "core:__FILE__" 1000 @wrap never = "__FILE__";
@syntax "core:current_compiler_scope" 1000 @wrap never = "@current_scope";

