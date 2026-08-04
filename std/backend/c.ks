module:

const RawUnwindToken = @opaque_type "RawUnwindToken";

const UnwindTokenImpl = [T] newtype {
    .raw :: RawUnwindToken,
    .value :: T,
};

@eval impl_native("backend.c.UnwindToken", UnwindTokenImpl);
