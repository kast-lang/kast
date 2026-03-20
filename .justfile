default:
    echo Hello there!
    just --list

test:
    fd --type file --extension ks --exec-batch self-kast tokenize
