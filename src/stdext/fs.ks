use std.target.c.*;

module:

const exists = (path :: &str) -> Bool => (
    let path_c = C_String.from_str(path);
    @defer C_String.drop(path_c);
    let f :: File = @native ''fopen(\(path_c), "r")'';
    if @native "\(f) == NULL" then (
        false
    ) else (
        @native "fclose(\(f))";
        true
    )
);

const File = @opaque_type "FILE*";

impl File as module = (
    module:

    const Mode = newtype {
        .append :: Bool,
    };

    const create_with_mode = (path :: &str, mode :: Mode) -> File => (
        let path_c = C_String.from_str(path);
        @defer C_String.drop(path_c);
        let mode :: std.target.c.C_str = if mode.append then (
            @native ''"a"''
        ) else (
            @native ''"w"''
        );
        let result = @native ''fopen(\(path_c), \(mode))'';
        if @native "\(result) == NULL" then (
            panic_fmt!("Failed to create file \(path)");
        );
        result
    );
    const create = (path :: &str) -> File => (
        create_with_mode(path, { .append = false })
    );
    const write = (self :: &mut File, s :: &str) => (
        @native "Kast_write(\(self^), \(s))";
    );
    const flush = (self :: &mut File) => (
        @native "fflush(\(self^))"
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
