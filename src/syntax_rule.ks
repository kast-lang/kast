use (import "./span.ks").*;
use (import "./rc_str.ks").*;

module:

const SyntaxRule = (
    module:

    const t = newtype {
        .name :: RcStr,
        .priority :: Priority,
        .parts :: ArrayList.t[Part],
        .span :: Span,
        .wrap_mode :: WrapMode,
        .can_undo_indentation :: Bool,
    };

    const is_same = (a :: &SyntaxRule.t, b :: &SyntaxRule.t) -> Bool => (
        # TODO can have same name rule defined in different places
        # should compare identity, but no way to do it currently
        a^.name == b^.name
    );

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
        match std.cmp.default_compare(&priority_filter_tag_idx(a), &priority_filter_tag_idx(b)) with (
            | :Equal => ()
            | ordering => return ordering
        );
        match { a, b } with (
            | { :Any, :Any } => :Equal
            | { :GreaterOrEqual a, :GreaterOrEqual b } => std.cmp.default_compare(&a, &b)
            | { :Greater a, :Greater b } => std.cmp.default_compare(&a, &b)
            | _ => panic("unreachable")
        )
    );

    const Part = newtype (
        | :Value {
            .name :: Option.t[RcStr],
            .priority_filter :: PriorityFilter,
        }
        | :Keyword RcStr
        | :Group Group
        | :Whitespace {
            .wrap :: RcStr,
            .no_wrap :: RcStr,
        }
    );

    const Group = newtype {
        .name :: Option.t[RcStr],
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
        | :IfAnyNonAssociative
        | :IfAnyAssociative
    );
);
