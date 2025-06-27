module Parser = Kast_parser

let ruleset : Parser.ruleset = Parser.RuleSet.parse_lines [%blob "rules"]
