use (import "./span.ks").*;

module:

const SyntaxRule = (
    module:
    
    const t = newtype {
        .name :: String,
        .priority :: Priority,
        .parts :: ArrayList.t[Part],
        .span :: Span,
        .wrap_mode :: WrapMode,
    };
    
    const Priority = Float64;

    const PriorityFilter = newtype (
        | :Any
        | :Greater Priority
        | :GreaterOrEqual Priority
    );
    
    const Part = newtype (
        | :Value {
            .name :: Option.t[String],
            .priority_filter :: PriorityFilter,
        }
        | :Keyword String
        | :Group Group
        | :Whitespace {
            .wrap :: String,
            .no_wrap :: String,
        }
    );

    const Group = newtype {
        .name :: Option.t[String],
        .parts :: ArrayList.t[Part],
        .quantifier :: Quantifier,
        .wrap_mode :: Option.t[WrapMode],
    };

    const Quantifier = newtype (
        | :None
        | :Optional
        # TODO | :Repeated
    );

    const WrapMode = newtype (
        | :Never
        | :Always
        | :IfAnyNonAssoc
        | :IfAnyAssoc
    );
);
