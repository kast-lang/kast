syntax from scratch

syntax @"builtin macro then" -> "0" = a ";;" b;;
syntax @"builtin macro syntax_module" -> "0" = "syntax_module" "{" body "}";;

syntax_module {
  syntax @"builtin macro then" -> 0 = a ";" b;
  syntax @"builtin macro then" -> 0 = a ";";

  syntax @"builtin macro struct_def" <- "-1" = "module" ":" body;

  syntax @"builtin macro use" <- 1 = "use" namespace ".*";

  # syntax return <- 2 = "return" value;
  syntax break_with_value <- 2 = "break" value;
  syntax break_without_value <- 2 = "break";
  syntax continue <- 2 = "continue";
  syntax yield <- 2 = "yield" value;

  syntax @"while" <- 3 = "while" cond "{" body "}";
  syntax @"loop" <- 3 = "loop" "{" body "}";
  syntax for_loop <- 3 = "for" value_pattern "in" generator "{" body "}";
  syntax @"builtin macro impl_cast" <- 4 = "impl" value "as" target "=" impl;
  syntax @"builtin macro impl_syntax" <- 4 = "impl" "syntax" def "=" impl;
  syntax @"builtin macro let" <- 4 = "let" pattern "=" value;
  syntax @"builtin macro const_let" <- 4 = "const" pattern "=" value;
  syntax @"builtin macro assign" <- 4 = pattern "=" value;

  syntax @"op +=" <- 4 = target "+=" value;
  syntax @"op -=" <- 4 = target "-=" value;
  syntax @"op *=" <- 4 = target "*=" value;
  syntax @"op /=" <- 4 = target "/=" value;
  syntax @"op %=" <- 4 = target "%=" value;

  syntax @"builtin macro newtype" <- 4.3 = "newtype" def;

  syntax @"builtin macro tuple" <- 4.5 = a "," b;
  syntax @"builtin macro tuple" <- 4.5 = a ",";

  syntax @"builtin macro field" <- 4.75 = "." name "=" value;
  syntax @"builtin macro field" <- 4.75 = "." name;

  syntax @"builtin macro comptime" <- 4.9 = "comptime" value;

  # syntax @"builtin macro with_context" <- 5 = "with" new_context "(" expr ")";
  syntax @"builtin macro with_context" <- 5 = "with" new_context;
  syntax @"builtin macro current_context" <- 5 = "current" context_type;
  syntax @"builtin macro macro" <- 5 = "macro" def;
  syntax @"builtin macro oneof" <- 5 = "oneof" def;

  syntax @"builtin macro merge" <- 5.5 = "|" a;
  syntax @"builtin macro merge" <- 5.5 = a "|" b;

  syntax @"builtin macro template_def" <- 6 = "[" arg "]" "=>" body;
  syntax @"builtin macro template_def" <- 6 = "forall" "[" arg "]" "{" body "}";
  syntax @"builtin macro template_def" <- 6 = "forall" "[" arg "]" "where" where "{" body "}";
  syntax @"builtin macro function_def" -> 6 = arg "=>" body;

  syntax @"builtin macro type_ascribe" <- 7.1 = value "::" type;
  syntax @"builtin macro type_ascribe" <- 7.1 = type "of" value;
  syntax @"builtin macro type_ascribe" <- 7.1 = type ":>" value;
  syntax @"builtin macro type_ascribe" <- 7.1 = value "<:" type;

  syntax @"builtin macro mutable_pattern" <- 7.25 = "mut" pattern;

  syntax @"builtin macro function_type" -> 7.5 = arg "->" result;
  syntax @"builtin macro function_type" -> 7.5 = arg "->" result "with" contexts;
  syntax @"builtin macro function_def" -> 7.5 = arg "->" returns "=>" body;

  syntax @"builtin macro if" -> 12.9 = cond "then" then "else" else;
  # syntax @"builtin macro match" <- 13 = "match" value "(" branches ")";
  syntax @"builtin macro match" <- 13 = "match" value "{" branches "}";
  syntax @"builtin macro if" <- 13 = "if" cond "then" then;
  syntax @"builtin macro if" <- 13 = "if" cond "then" then "else" else;

  syntax @"builtin macro if" -> 13.1 = cond "?" then ":" else;

  syntax @"builtin macro function_def" <- 13.5 = "fn" "(" arg ")" "with" contexts "{" body "}";
  syntax @"builtin macro function_def" <- 13.5 = "fn" "(" arg ")" "->" result_type "{" body "}";
  syntax @"builtin macro function_def" <- 13.5 = "fn" "(" arg ")" "->" result_type "with" contexts "{" body "}";
  syntax @"builtin macro function_def" <- 13.5 = "fn" "(" arg ")" "{" body "}";

  syntax implements <- 14 = type "implements" trait;

  syntax pipe_right <- 15 = arg "|>" f;
  syntax pipe_left <- 15 = f "<|" arg;

  syntax try_explicit <- 16 = "try" "[" targ "]" expr;
  syntax try_implicit <- 16 = "try" expr;
  syntax catch_impl <- 16 = expr "catch" e "{" catch_block "}";
  syntax catch_impl <- 16 = expr "catch" e "(" catch_block ")";

  syntax @"builtin macro or" <- 17 = lhs "or" rhs;
  syntax @"builtin macro and" <- 18 = lhs "and" rhs;

  syntax @"builtin macro is" <- 18.5 = value "is" pattern;

  syntax @"op binary <" <- 19 = lhs "<" rhs;
  syntax @"op binary <=" <- 19 = lhs "<=" rhs;
  syntax @"op binary ==" <- 19 = lhs "==" rhs;
  syntax @"op binary !=" <- 19 = lhs "!=" rhs;
  syntax @"op binary >=" <- 19 = lhs ">=" rhs;
  syntax @"op binary >" <- 19 = lhs ">" rhs;

  syntax @"builtin macro cast" <- 20 = value "as" target;
  #syntax @"builtin macro check_impl" <- 21 = value "impl" trait;

  syntax @"op unary +" <- 25 = "+" _;
  syntax @"op unary -" <- 25 = "-" _;
  syntax @"op binary +" <- 25 = lhs "+" rhs;
  syntax @"op binary -" <- 25 = lhs "-" rhs;

  syntax @"op binary *" <- 40 = lhs "*" rhs;
  syntax @"op binary /" <- 40 = lhs "/" rhs;
  syntax @"op binary %" <- 40 = lhs "%" rhs;

  # syntax @"op binary ^" -> 60 = lhs "^" rhs;

  syntax @"op postfix ++" <- 100 = x "++";
  syntax @"op prefix ++" <- 100 = "++" x;
  syntax @"op postfix --" <- 100 = x "--";
  syntax @"op prefix --" <- 100 = "--" x;
  syntax @"not" <- 100 = "not" _;

  syntax @"builtin macro call" <- 100 = f arg;

  syntax @"builtin macro ref" -> 110 = "&" place;
  syntax @"builtin macro deref" <- 115 = ref "^";

  syntax @"builtin macro typeof" <- 120 = "typeof" expr;
  syntax @"builtin macro typeofvalue" <- 120 = "typeofvalue" expr;
  syntax @"builtin macro compile_ast" <- 120 = "compile_ast" ast;

  syntax @"builtin macro native" <- 150 = "native" name;
  syntax @"builtin macro import" <- 150 = "import" path;
  syntax @"builtin macro include" <- 150 = "include" path;
  syntax invoke_macro <- 150 = @"macro" "!" arg;
  syntax @"builtin macro call_macro" <- 150 = @"macro" "!!" arg;

  syntax @"builtin macro quote" -> 200 = "`" "(" expr ")";
  #syntax @"builtin macro variant" <- 250 = type ":" name value;
  #syntax @"builtin macro variant" <- 250 = type ":" name;
  syntax @"builtin macro variant" <- 250 = ":" name value;
  syntax @"builtin macro variant" <- 250 = ":" name;

  syntax @"builtin macro field_access" <- 300 = obj "." field;

  syntax @"builtin macro instantiate_template" <- 300 = template "[" arg "]";

  syntax @"builtin macro struct_def" <- 500 = "rec" "(" body ")";
  syntax @"builtin macro struct_def" <- 500 = "rec" "{" body "}";
  syntax @"builtin macro struct_def" <- 500 = "struct" "(" body ")";
  syntax @"builtin macro struct_def" <- 500 = "struct" "{" body "}";
  syntax @"builtin macro unwindable" <- 500 = "unwindable" name body;
  syntax @"builtin macro unwind" <- 500 = "unwind" name value;

  syntax @"builtin macro unquote" -> 500 = "$" expr;
  syntax @"builtin macro unquote" -> 500 = "$" "(" expr ")";

  syntax let_infer <- 500 = "_let" pattern;

# syntax @"builtin macro function_def" <- 100000 = "{" body "}";

  syntax @"builtin macro list" <- 100000 = "list" "[" values "]";
  syntax @"builtin macro list" <- 100000 = "list" "[" "]";
  syntax @"builtin macro scope" <- 100000 = "(" _ ")";
  syntax @"builtin macro make_unit" <- 100000 = "(" ")";
  syntax @"builtin macro placeholder" <- 100000 = "_";
  
  syntax true <- 100000 = "true";
  syntax false <- 100000 = "false";

# const @"postfix ++" = macro (.x :: ast) => `(x += 1);

}
