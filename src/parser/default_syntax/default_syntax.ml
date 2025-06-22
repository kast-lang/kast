let ruleset : Parser.ruleset =
  Parser.RuleSet.of_list
    [
      (let priority = 0.0 in
       {
         name = "then";
         priority;
         parts =
           [
             Value { name = None; priority = Greater priority };
             Keyword ";";
             Value { name = None; priority = GreaterOrEqual priority };
           ];
       });
      (let priority = 0.0 in
       {
         name = "stmt";
         priority;
         parts =
           [ Value { name = None; priority = Greater priority }; Keyword ";" ];
       });
      (let priority = 1.0 in
       {
         name = "use .*";
         priority;
         parts =
           [
             Keyword "use";
             Value { name = None; priority = Greater priority };
             Keyword ".*";
           ];
       });
      (let priority = 1.0 in
       {
         name = "if";
         priority;
         parts =
           [
             Keyword "if";
             Value { name = Some "cond"; priority = Greater priority };
             Keyword "then";
             Value { name = Some "then"; priority = Greater priority };
             Keyword "else";
             Value { name = Some "else"; priority = GreaterOrEqual priority };
           ];
       });
      (let priority = 1.5 in
       {
         name = "|>";
         priority;
         parts =
           [
             Value { name = Some "arg"; priority = GreaterOrEqual priority };
             Keyword "|>";
             Value { name = Some "f"; priority = Greater priority };
           ];
       });
      (let priority = 2.0 in
       {
         name = "apply";
         priority;
         parts =
           [
             Value { name = Some "f"; priority = GreaterOrEqual priority };
             Value { name = Some "arg"; priority = Greater priority };
           ];
       });
      (let priority = 1000.0 in
       {
         name = "scope";
         priority;
         parts =
           [ Keyword "("; Value { name = None; priority = Any }; Keyword ")" ];
       });
    ]
