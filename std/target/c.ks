module:

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

