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
        | :GreaterOrEqual Priority
        | :Greater Priority
    );

    const priority_matches = (
        priority :: Priority,
        filter :: PriorityFilter,
    ) -> Bool => match filter with (
        | :Any => true
        | :GreaterOrEqual min_priority => priority >= min_priority
        | :Greater min_priority => priority > min_priority
    );
    
    const priority_filter_tag_idx = (self :: PriorityFilter) -> Int32 => (
        match self with (
            | :Any => 0
            | :GreaterOrEqual _ => 1
            | :Greater _ => 2
        )
    );
    
    const compare_priority_filter = (
        a :: PriorityFilter,
        b :: PriorityFilter,
    ) -> std.cmp.Ordering => with_return (
        match std.cmp.default_compare(priority_filter_tag_idx(a), priority_filter_tag_idx(b)) with (
            | :Equal => ()
            | ordering => return ordering
        );
        match { a, b } with (
            | { :Any, :Any } => :Equal
            | { :GreaterOrEqual a, :GreaterOrEqual b } => std.cmp.default_compare(a, b)
            | { :Greater a, :Greater b } => std.cmp.default_compare(a, b)
            | _ => panic("unreachable")
        )
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
        .span :: Span,
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
