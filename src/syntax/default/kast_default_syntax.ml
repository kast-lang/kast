module Parser = Kast_parser

let ruleset : Parser.ruleset = Parser.Ruleset.parse_lines [%blob "rules.ks"]
