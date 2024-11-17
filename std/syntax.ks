syntax from scratch

syntax @"builtin macro then" -> "0" = a ";;" b;;
syntax @"builtin macro syntax_module" -> "0" = "syntax_module" "{" body "}";;

syntax_module {
  syntax @"builtin macro then" -> 0 = a ";" b;
  syntax @"builtin macro then" -> 0 = a ";";

  syntax @"builtin macro struct_def" <- "-1" = "module" ":" body;

  syntax @"builtin macro use" <- 1 = "use" namespace ".*";

  # syntax return <- 2 = "return" value;
  # syntax break_with_value <- 2 = "break" value;
  # syntax break_without_value <- 2 = "break";
  # syntax continue_impl <- 2 = "continue";

  # syntax loop_impl <- 3 = "loop" "{" body "}";
  # syntax for_loop <- 3 = "for" value_pattern "in" generator "{" body "}";
  syntax @"builtin macro impl_cast" <- 4 = "impl" value "as" target "=" impl;
  syntax @"builtin macro impl_syntax" <- 4 = "impl" "syntax" def "=" impl;
  syntax @"builtin macro let" <- 4 = "let" pattern "=" value;
  syntax @"builtin macro const_let" <- 4 = "const" pattern "=" value;
  # syntax @"builtin macro assign" <- 4 = pattern "=" value;

  syntax @"builtin macro newtype" <- 4.3 = "newtype" def;

  syntax @"builtin macro tuple" <- 4.5 = a "," b;
  syntax @"builtin macro tuple" <- 4.5 = a ",";

  syntax @"builtin macro field" <- 4.75 = "." name "=" value;
  syntax @"builtin macro field" <- 4.75 = "." name;

  # syntax @"builtin macro unwindable_block" <- 5 = "unwindable_block" def;
  # syntax @"builtin macro with_context" <- 5 = "with" new_context "(" expr ")";
  # syntax @"builtin macro current_context" <- 5 = "current" context_type;
  syntax @"builtin macro macro" <- 5 = "macro" def;
  syntax @"builtin macro comptime" <- 5 = "comptime" value;
  syntax @"builtin macro oneof" <- 5 = "oneof" def;

  syntax @"builtin macro merge" <- 6 = "|" a;
  syntax @"builtin macro merge" <- 6 = a "|" b;
  syntax @"builtin macro function_def" -> 7 = arg "=>" body;

  syntax @"builtin macro type_ascribe" <- 7.1 = value "::" type;
  syntax @"builtin macro type_ascribe" <- 7.1 = type "of" value;
  syntax @"builtin macro type_ascribe" <- 7.1 = type ":>" value;
  syntax @"builtin macro type_ascribe" <- 7.1 = value "<:" type;

  syntax @"builtin macro mutable_pattern" <- 7.25 = "mut" pattern;

  syntax @"builtin macro function_type" -> 7.5 = arg "->" result;
  syntax @"builtin macro function_type" -> 7.5 = arg "->" result "with" contexts;
  syntax @"builtin macro function_def" -> 7.5 = arg "->" returns "=>" body;

  syntax @"builtin macro template_def" <- 9 = "forall" "[" arg "]" "{" body "}";
  syntax @"builtin macro template_def" <- 9 = "forall" "[" arg "]" "where" where "{" body "}";

  syntax @"builtin macro if" -> 12.9 = cond "then" then "else" else;
  syntax @"builtin macro match" <- 13 = "match" value "(" branches ")";
  syntax @"builtin macro match" <- 13 = "match" value "{" branches "}";
  syntax @"builtin macro if" <- 13 = "if" cond "then" then;
  syntax @"builtin macro if" <- 13 = "if" cond "then" then "else" else;

  syntax @"builtin macro if" -> 13.1 = cond "?" then ":" else;

  syntax @"builtin macro function_def" <- 13.5 = "fn" "(" arg ")" contexts "{" body "}";
  syntax @"builtin macro function_def" <- 13.5 = "fn" "(" arg ")" "->" result_type "{" body "}";
  syntax @"builtin macro function_def" <- 13.5 = "fn" "(" arg ")" "{" body "}";

  syntax implements <- 14 = type "implements" trait;

  syntax pipe_right <- 15 = arg "|>" f;
  syntax pipe_left <- 15 = f "<|" arg;

  syntax try_explicit <- 16 = "try" "[" targ "]" expr;
  syntax try_implicit <- 16 = "try" expr;
  syntax catch_impl <- 16 = expr "catch" e "{" catch_block "}";
  syntax catch_impl <- 16 = expr "catch" e "(" catch_block ")";

  syntax @"builtin fn or" <- 17 = lhs "or" rhs;
  syntax @"builtin fn and" <- 18 = lhs "and" rhs;

  syntax @"builtin macro is" <- 18.5 = value "is" pattern;

  syntax @"op binary <" <- 19 = lhs "<" rhs;
  syntax @"op binary <=" <- 19 = lhs "<=" rhs;
  syntax @"op binary ==" <- 19 = lhs "==" rhs;
  syntax @"op binary !=" <- 19 = lhs "!=" rhs;
  syntax @"op binary >=" <- 19 = lhs ">=" rhs;
  syntax @"op binary >" <- 19 = lhs ">" rhs;

  syntax @"builtin macro cast" <- 20 = value "as" target;
  #syntax @"builtin macro check_impl" <- 21 = value "impl" trait;

  syntax @"op unary +" <- 25 = "+" x;
  syntax @"op unary -" <- 25 = "-" x;
  syntax @"op binary +" <- 25 = lhs "+" rhs;
  syntax @"op binary -" <- 25 = lhs "-" rhs;

  syntax @"op binary *" <- 40 = lhs "*" rhs;
  syntax @"op binary /" <- 40 = lhs "/" rhs;
  syntax @"op binary %" <- 40 = lhs "%" rhs;

  syntax @"op binary ^" -> 60 = lhs "^" rhs;

  syntax @"op postfix ++" <- 100 = x "++";
  syntax @"op prefix ++" <- 100 = "++" x;
  syntax @"op postfix --" <- 100 = x "--";
  syntax @"op prefix --" <- 100 = "--" x;

  syntax @"builtin macro call" <- 100 = f arg;

  syntax @"builtin macro typeof" <- 120 = "typeof" expr;
  syntax @"builtin macro typeofvalue" <- 120 = "typeofvalue" expr;

  syntax @"builtin macro quote" -> 200 = "`" "(" expr ")";
  syntax @"builtin macro unquote" -> 200 = "$" expr;
  syntax @"builtin macro unquote" -> 200 = "$" "(" expr ")";

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
  syntax @"builtin macro native" <- 500 = "native" name;
  syntax @"builtin macro import" <- 500 = "import" path;
  syntax let_infer <- 500 = "_let" pattern;

# syntax @"builtin macro function_def" <- 100000 = "{" body "}";

  syntax @"builtin macro scope" <- 100000 = "(" _ ")";
  syntax @"builtin macro make_unit" <- 100000 = "(" ")";
  syntax @"builtin macro placeholder" <- 100000 = "_";

# const @"postfix ++" = macro (.x :: ast) => `(x += 1);

  impl syntax pipe_right = macro (.arg, .f) => `((let arg = $arg; let f = $f; f arg));
  impl syntax pipe_left = macro (.f, .arg) => `((let f = $f; let arg = $arg; f arg));

  impl syntax let_infer = macro (.pattern) => `(
    (let $pattern = _; $pattern)
  );
}
