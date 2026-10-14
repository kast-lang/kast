const g = [T] (x :: T) => ();
const f = [T] (x :: T) => (
    const infer = _;
    (@native "dbg.print")(infer);
    g[infer](x);
);

const Int32 = @native "Int32";
f[Int32](0)
