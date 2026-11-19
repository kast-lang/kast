use (import "./dsu.ks").*;
use (import "../output.ks").*;

module:

const Inference = (
    module:

    const Var = (
        module:

        const Data = [T] newtype {
            .inferred :: Option.t[T],
        };

        const t = [T] Dsu.Node[Var.Data[T]];

        const print = [T] (self :: &Var.t[T], print_inferred :: &T -> ()) => (
            let data = Dsu.get_value(self);
            match data^.inferred with (
                | :None => locked_output.write_str("_")
                | :Some ref inferred => print_inferred(inferred)
            );
        );

        const new_inferred = [T] (inferred :: T) -> Var.t[T] => (
            Dsu.Node_new({ .inferred = :Some inferred })
        );

        const new_not_inferred = [T] () -> Var.t[T] => (
            Dsu.Node_new({ .inferred = :None })
        );

        const unite = [T] (
            a :: Var.t[T],
            b :: Var.t[T],
            unite_T :: (T, T) -> T,
        ) => (
            let merge_value = (
                { .inferred = inferred_a },
                { .inferred = inferred_b },
            ) => (
                let inferred = match { inferred_a, inferred_b } with (
                    | { :None, :None } => :None
                    | { :Some a, :None } => :Some a
                    | { :None, :Some b } => :Some b
                    | { :Some a, :Some b } => :Some unite_T(a, b)
                );
                { .inferred }
            );
            Dsu.merge(a, b, .merge_value);
        );

        const ensure_inferred_or = [T] (
            self :: &Var.t[T],
            f :: () -> Never,
        ) -> &T => (
            let data = Dsu.get_value(self);
            if data^.inferred is :Some ref inferred then (
                inferred
            ) else (
                f() |> from_never
            )
        );
    );
);
