const Int32 = @native "Int32";

impl syntax (@context ty) = `(
    (@native "create_context_type")($ty)
);

const Foo = newtype {
    .x :: Int32,
};

const Ctx = @context Foo;

with Ctx = { .x = 0 };

(@current Ctx).x = 69;

&(@current Ctx);
