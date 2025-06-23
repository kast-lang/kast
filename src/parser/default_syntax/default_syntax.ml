let ruleset : Parser.ruleset =
  Parser.RuleSet.of_list
    [
      {
        name = "then";
        priority = 0.0;
        parts =
          [
            Value { name = None; priority = Greater };
            Keyword ";";
            Value { name = None; priority = GreaterOrEqual };
          ];
      };
      {
        name = "stmt";
        priority = 0.0;
        parts = [ Value { name = None; priority = Greater }; Keyword ";" ];
      };
      {
        name = "use .*";
        priority = 1.0;
        parts =
          [
            Keyword "use";
            Value { name = None; priority = Greater };
            Keyword ".*";
          ];
      };
      {
        name = "if";
        priority = 1.0;
        parts =
          [
            Keyword "if";
            Value { name = Some "cond"; priority = Greater };
            Keyword "then";
            Value { name = Some "then"; priority = Greater };
            Keyword "else";
            Value { name = Some "else"; priority = GreaterOrEqual };
          ];
      };
      {
        name = "|>";
        priority = 1.5;
        parts =
          [
            Value { name = Some "arg"; priority = GreaterOrEqual };
            Keyword "|>";
            Value { name = Some "f"; priority = Greater };
          ];
      };
      {
        name = "apply";
        priority = 2.0;
        parts =
          [
            Value { name = Some "f"; priority = GreaterOrEqual };
            Value { name = Some "arg"; priority = Greater };
          ];
      };
      {
        name = "scope";
        priority = 1000.0;
        parts =
          [ Keyword "("; Value { name = None; priority = Any }; Keyword ")" ];
      };
    ]
