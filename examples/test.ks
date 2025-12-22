const id :: (
    [T] T -> T
) = (
    # [T2] T2 -> T2
    [T2] (x :: T2) => x
);
id[@native "int32"]
