use std.target.c.*;

module:

const exists = (path :: &str) -> Bool => (
    @native "fs.existsSync(\(path))"
);

const File = @opaque_type "FILE*";

impl File as module = (
    module:

    const create = (path :: &str) -> File => (
        let path_c = C_String.from_str(path);
        @defer C_String.drop(path_c);
        let result = @native ''fopen(\(path_c), "w")'';
        if @native "\(result) == NULL" then (
            panic_fmt!("Failed to create file \(path)");
        );
        result
    );
    const write = (self :: &mut File, s :: &str) => (
        @native "Kast_write(\(self^), \(s))";
    );
    const close = (self :: File) => (
        @native "fclose(\(self))";
    );
);

const write = (path :: &str, s :: &str) => (
    let mut file = File.create(path);
    @defer File.close(file);
    &mut file |> File.write(s);
);
