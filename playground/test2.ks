module:

const Type = @native "Type";

const String :: Type = @native "String";
const Int :: Type = @native "Int32";

const dbg = [T] (x :: T) -> () => (@native "dbg.print")(x);

const A = [T] type { B[T] };
const B = [U] type { A[U] };

A[U](U->T)(T->U) == A[U]

A[U] is defined in scope of B
A[U](U->T) is defined in scope of B->A
A[U](U->T)(T->U) is defined in B->A->B

# in rust this would be:
# struct A<T>(B<T>);
# struct B<T>(A<T>);