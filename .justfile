default:
    echo Hello there!
    just --list

test:
    fd --type file --extension ks --exec-batch self-kast tokenize
    self-kast parse_syntax_rules tests/syntax/*.ks
    self-kast parse-json tests/test.json
    self-kast parse-json tests/lsp-init.json
    # fd --type file --extension ks --exec-batch self-kast parse
