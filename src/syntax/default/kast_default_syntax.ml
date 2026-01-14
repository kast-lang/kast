module Parser = Kast_parser

let ruleset : Parser.ruleset = Parser.Ruleset.parse_lines [%include_file "rules.ks"]
