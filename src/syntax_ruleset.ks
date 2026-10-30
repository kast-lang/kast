use (import "./log.ks").*;
use (import "./output.ks").*;
use (import "./diagnostic.ks").*;
use (import "./syntax_rule.ks").*;
use (import "./rc_str.ks").*;
use std.rc.Rc;
use std.collections.OrdMap;
use std.collections.OrdSet;

module:

const SyntaxRuleset = (
    module:

    const t = newtype {
        .keywords :: OrdSet.t[RcStr],
        .root :: Node,
        .syntax_keyword :: Option.t[RcStr],
        .rules :: ArrayList.t[Rc.t[SyntaxRule.t]],
    };

    const EdgeKey = newtype (
        | :Keyword RcStr
        | :Value
    );

    impl EdgeKey as std.cmp.Ord = {
        .compare = (a, b) => match ({ a^, b^ } :: { _, _ }) with (
            | { :Keyword a, :Keyword b } => std.cmp.default_compare(&a, &b)
            | { :Keyword _, :Value } => :Less
            | { :Value, :Keyword _ } => :Greater
            | { :Value, :Value } => :Equal
        ),
    };

    const Edge = newtype {
        .key :: EdgeKey,
        .target :: Node,
        ## max priority filter of values parsed by THIS edge only
        .max_value_priority :: SyntaxRule.PriorityFilter,
        ## max priority of rules reachable from this edge
        .max_rule_priority :: SyntaxRule.Priority,
    };

    const EdgeMap = OrdMap.t[EdgeKey, Edge];
    const new_edge_map = () -> EdgeMap => (
        OrdMap.new()
    );

    const Node = newtype {
        .terminal :: Option.t[Rc.t[SyntaxRule.t]],
        .next :: EdgeMap,
    };

    const new_node = () -> Node => {
        .terminal = :None,
        .next = new_edge_map(),
    };

    const new = () -> SyntaxRuleset.t => {
        .keywords = OrdSet.new(),
        .root = new_node(),
        .syntax_keyword = :Some RcStr.from_String(String.from_str("@syntax")),
        .rules = ArrayList.new(),
    };

    const clone = (self :: &SyntaxRuleset.t) -> SyntaxRuleset.t => (
        let mut result = new();
        for &rule in &self^.rules |> ArrayList.iter do (
            add(&mut result, rule);
        );
        result
    );

    const Context = @context type {
        .ruleset :: &mut SyntaxRuleset.t,
        .rule :: &SyntaxRule.t,
    };

    const follow_edge = (
        node :: &mut Node,
        edge_key :: EdgeKey,
        .value_priority :: SyntaxRule.PriorityFilter,
    ) -> &mut Node => (
        let ctx = @current Context;
        let edge = &mut node^.next
            |> OrdMap.get_or_init(
                edge_key,
                () => {
                    .key = edge_key,
                    .target = new_node(),
                    .max_value_priority = value_priority,
                    .max_rule_priority = ctx.rule^.priority,
                }
            );
        if SyntaxRule.compare_priority_filter(value_priority, edge^.max_value_priority) is :Greater then (
            edge^.max_value_priority = value_priority;
        );
        if ctx.rule^.priority > edge^.max_rule_priority then (
            edge^.max_rule_priority = ctx.rule^.priority;
        );
        &mut edge^.target
    );

    const iter_cps_from = [T] (
        index :: Int32,
        list :: &ArrayList.t[T],
        .node :: &mut Node,
        .consumer :: (&mut Node, &T, .continuation :: &mut Node -> ()) -> (),
        .continuation :: &mut Node -> (),
    ) => (
        if index < ArrayList.length(list) then (
            let continuation = node => (
                iter_cps_from[T](index + 1, list, .node, .consumer, .continuation);
            );
            consumer(node, ArrayList.at(list, index), .continuation);
        ) else (
            continuation(node);
        )
    );

    const follow_parts = (
        node :: &mut Node,
        parts :: &ArrayList.t[SyntaxRule.Part],
        .continuation :: &mut Node -> (),
    ) => (
        let follow_part = (node, part, .continuation) => (
            match part^ with (
                | :Whitespace _ => (
                    continuation(node);
                )
                | :Keyword keyword => (
                    &mut (@current Context).ruleset^.keywords |> OrdSet.add(keyword);
                    let node = follow_edge(node, :Keyword keyword, .value_priority = :Any);
                    continuation(node);
                )
                | :Value ref value => (
                    let node = follow_edge(node, :Value, .value_priority = value^.priority_filter);
                    continuation(node);
                )
                | :Group ref group => (
                    match group^.quantifier with (
                        | :None => (
                            follow_parts(node, &group^.parts, .continuation);
                        )
                        | :Optional => (
                            continuation(node);
                            follow_parts(node, &group^.parts, .continuation);
                        )
                    )
                )
            );
        );
        iter_cps_from(0, parts, .node, .consumer = follow_part, .continuation);
    );

    const add = (self :: &mut SyntaxRuleset.t, rule :: Rc.t[SyntaxRule.t]) => (
        &mut self^.rules |> ArrayList.push_back(rule);
        with Context = { .ruleset = self, .rule = &rule^ };
        follow_parts(
            &mut self^.root,
            &rule^.parts,
            .continuation = node => (
                if node^.terminal is :Some existing_rule then (
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Other,
                        .span = rule^.span,
                        .message = () => (
                            locked_output.write_str("Conflicting syntax rules ");
                            locked_output.write_escaped(&existing_rule^.name);
                            locked_output.write_str(" and ");
                            locked_output.write_escaped(&rule^.name);
                        ),
                        .related = (
                            let mut related = ArrayList.new();
                            let existing_rule_info = {
                                .span = existing_rule^.span,
                                .message = () => (
                                    locked_output.write_str("Existing rule is defined here");
                                ),
                            };
                            &mut related |> ArrayList.push_back(existing_rule_info);
                            related
                        ),
                    };
                    Diagnostic.report(diagnostic);
                );
                Log.debug!("Added rule \(rule^.name)");
                node^.terminal = :Some rule;
            ),
        );
    );

    const print = (self :: &SyntaxRuleset.t) => (
        print_node(&self^.root);
    );

    const print_node = (node :: &Node) => (
        if node^.terminal is :Some rule then (
            ansi.with_mode(
                :Green,
                () => locked_output.write_escaped(&rule^.name),
            );
            locked_output.write_str("\n");
        );
        for &{ .key = _, .value = ref edge } in &node^.next |> OrdMap.iter do (
            match edge^.key with (
                | :Keyword ref keyword => (
                    ansi.with_mode(
                        :Magenta,
                        () => locked_output.write(keyword),
                    );
                )
                | :Value => (
                    ansi.with_mode(
                        :Yellow,
                        () => locked_output.write_str("<value>"),
                    );
                )
            );
            locked_output.write_str(" {\n");
            locked_output.inc_indentation();
            print_node(&edge^.target);
            locked_output.dec_indentation();
            locked_output.write_str("}\n");
        );
    );
);
