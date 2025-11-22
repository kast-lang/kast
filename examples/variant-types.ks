 #  let some1and2 = :Option[int32].Some (1, 2);
let a :: (:Some _ | :None | :Any) = :None;
a = :Some 123;
let b = :Any;
b = :None;
b = :Any;
a = b;
let f = x :: std.int32 => :Some x;
