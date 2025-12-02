module:
let is_whitespace = (c :: char) -> bool => (
    c == ' ' or c == '\n' or c == '\t'
);