default:
    echo Hello there!
    just --list

test:
    fd --type file --extension ks --exec-batch self-kast tokenize
    self-kast parse_syntax_rules tests/kast_syntax_rules.ks tests/simple_rules.ks
