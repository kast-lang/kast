module:

const Int = Int32;
const Long = Int64;
const SizeT = UInt32;
const void_ptr = @opaque_type "void*";

const RawUnwindToken = @opaque_type "RawUnwindToken";

const UnwindTokenImpl = [T] newtype {
    .raw :: RawUnwindToken,
    .value :: T,
};

@exec_fork (
    impl_native("target.c.UnwindToken", UnwindTokenImpl);
);

const C_String = @opaque_type "const char*";

impl C_String as module = (
    module:

    const as_c_str = (self :: &C_String) -> C_str => (
        @native "\(self^)"
    );

    const from_str = (s :: &str) -> C_String => (
        @native "StringView_to_C_String(\(s))"
    );

    const drop = (self :: C_String) => (
        @native "Kast_free(\(self))";
    );
);

const C_str = @opaque_type "const char*";

impl C_str as module = (
    module:

    const to_str = (s :: C_str) -> &str => (
        @native "StringView_from_C_StringView(\(s))"
    );

    const to_String = (s :: C_str) -> String => (
        @native "String_from_C_StringView(\(s))"
    );
);

const panic_errno = (message :: &str) -> Never => (
    @native ''exit_errno("%.*s", (int)\(message).length, \(message).buf)'';
    @native "#unreachable"
);

const File = @opaque_type "FILE*";

impl File as module = (
    module:

    const open = (path :: &str, mode :: &str) -> File => (
        let path_c = C_String.from_str(path);
        @defer C_String.drop(path_c);
        let path_c = &path_c |> C_String.as_c_str;
        let mode_c = C_String.from_str(mode);
        @defer C_String.drop(mode_c);
        let mode_c = &mode_c |> C_String.as_c_str;
        let f :: File = @native "fopen(\(path_c), \(mode_c))";
        if @native "\(f) == NULL" then (
            panic_errno("Failed to fopen");
        );
        f
    );

    const close = (f :: File) => (
        if @native "fclose(\(f)) != 0" then (
            panic_errno("Failed to fclose");
        );
    );

    const check_error = (f :: &File) => (
        let e :: Int = @native "ferror(\(f^))";
        if @native "\(e) != 0" then (
            @native ''exit_with_error("File error %d (%s)", \(e), strerror(\(e)))'';
        );
    );

    const read = (f :: &mut File, buf :: Slice.mutable[Byte]) -> SizeT => (
        let read :: SizeT = @native "fread(\(buf.array), 1, \(buf.length), \(f^))";
        if read == 0 then (
            check_error(&f^);
        );
        read
    );

    const read_exactly = (f :: &mut File, mut buf :: Slice.mutable[Byte]) => (
        while buf.length != 0 do (
            let read = read(f, buf);
            # TODO no Int32
            let read :: Int32 = @native "\(read)";
            buf = Slice.subslice_mut(buf, read, buf.length - read);
        );
    );

    const read_to_end_slowly = (
        f :: &mut File,
    ) -> std.collections.ArrayList.t[Byte] => (
        let mut result = std.collections.ArrayList.with_capacity(4);
        loop (
            let remaining_buf :: Slice.mutable[Byte] = {
                .length = @native "\(&result)->capacity - \(&result)->length",
                .array = @native "\(&result)->buf",
            };
            if remaining_buf.length == 0 then (
                let new_capacity = @native "\(&result)->capacity * 2";
                &mut result |> std.collections.ArrayList.reserve(new_capacity);
            );
            let read = read(f, remaining_buf);
            @native "\(&mut result)->length += \(read)";
            if read == 0 then break;
        );
        result
    );

    const read_to_end = (
        f :: &mut File,
    ) -> std.collections.ArrayList.t[Byte] => with_return (
        let seek_res :: Int = @native "fseek(\(f^), 0, SEEK_END)";
        if seek_res < 0 then (
            if @native "errno == ESPIPE" then (
                return read_to_end_slowly(f);
            );
            panic_errno("Kast_read_to_end.fseek(1)");
        );
        let size :: Long = @native "ftell(\(f^))";
        if size < 0 then (
            panic_errno("Kast_read_to_end.ftell");
        );
        let size :: SizeT = @native "\(size)";
        let seek_res :: Int = @native "fseek(\(f^), 0, SEEK_SET)";
        if seek_res < 0 then (
            panic_errno("Kast_read_to_end.fseek(2)");
        );
        let mut buf = std.collections.ArrayList.with_capacity(size);
        @native "\(&buf)->length = \(size)";
        read_exactly(f, &mut buf |> std.collections.ArrayList.as_slice_mut);
        buf
    );
);

