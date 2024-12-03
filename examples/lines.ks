use std.*;

let lines = fn(s) {
    let mut line = "";
    for c in chars(s) {
        if c == '\n' then (
            yield line;
            line = "";
        ) else (
            line = push_char(line, c);
        )
    };
    if line != "" then (
        yield line;
    );
};

for line in lines(read_file "examples/lines.ks") {
    print line;
}
