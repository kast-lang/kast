use (import "./common.ks").*;
use (import "./log.ks").*;
use (import "./output.ks").*;
use (import "./error.ks").*;
use (import "./syntax_rule.ks").*;
use std.collections.OrdMap;

module:

const SyntaxRuleset = (
    module:
    
    const t = newtype {
        .root :: Node,
    };
    
    const EdgeKey = newtype (
        | :Keyword String
        | :Value
    );
    
    const compare_edge_key = (a :: EdgeKey, b :: EdgeKey) -> std.cmp.Ordering => (
        match { a, b } with (
            | { :Keyword a, :Keyword b } => std.cmp.default_compare(a, b)
            | { :Keyword _, :Value } => :Less
            | { :Value, :Keyword _ } => :Greater
            | { :Value, :Value } => :Equal
        )
    );
    
    const Edge = newtype {
        .key :: EdgeKey,
        .target :: Node,
    };
    
    const EdgeMap = OrdMap.t[EdgeKey, Edge];
    const new_edge_map = () -> EdgeMap => (
        OrdMap.new_with_compare(compare_edge_key)
    );
    
    const Node = newtype {
        .terminal :: Option.t[SyntaxRule.t],
        .next :: EdgeMap,
    };
    
    const new_node = () -> Node => {
        .terminal = :None,
        .next = new_edge_map(),
    };
    
    const new = () -> SyntaxRuleset.t => {
        .root = new_node(),
    };
    
    const follow_edge = (node :: &mut Node, edge_key :: EdgeKey) -> &mut Node => (
        let mut edge = &mut node^.next
            |> OrdMap.get_or_init(
                edge_key,
                () => {
                    .key = edge_key,
                    .target = new_node(),
                }
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
                iter_cps_from(index + 1, list, .node, .consumer, .continuation);
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
                    let node = follow_edge(node, :Keyword keyword);
                    continuation(node);
                )
                | :Value value => (
                    let node = follow_edge(node, :Value);
                    continuation(node);
                )
                | :Group group => (
                    match group.quantifier with (
                        | :None => (
                            follow_parts(node, &group.parts, .continuation);
                        )
                        | :Optional => (
                            continuation(node);
                            follow_parts(node, &group.parts, .continuation);
                        )
                    )
                )
            );
        );
        iter_cps_from(0, parts, .node, .consumer = follow_part, .continuation);
    );
    
    const add = (self :: &mut SyntaxRuleset.t, rule :: SyntaxRule.t) => (
        follow_parts(
            &mut self^.root,
            &rule.parts,
            .continuation = node => (
                if node^.terminal is :Some existing_rule then (
                    Error.report(
                        rule.span,
                        "Conflicting syntax rules "
                        + escape_string(existing_rule.name)
                        + " and "
                        + escape_string(rule.name),
                    );
                );
                Log.debug("Added rule " + rule.name);
                node^.terminal = :Some rule;
            ),
        );
    );
    
    const print = (self :: &SyntaxRuleset.t) => (
        print_node(&self^.root);
    );
    
    const print_node = (node :: &Node) => (
        let output = @current Output;
        if node^.terminal is :Some rule then (
            ansi.with_mode(
                :Green,
                () => output.write(escape_string(rule.name)),
            );
            output.write("\n");
        );
        for &{ .key = _, .value = ref edge } in &node^.next |> OrdMap.iter do (
            match edge^.key with (
                | :Keyword keyword => (
                    ansi.with_mode(
                        :Magenta,
                        () => output.write(keyword),
                    );
                )
                | :Value => (
                    ansi.with_mode(
                        :Yellow,
                        () => output.write("<value>"),
                    );
                )
            );
            output.write(" {\n");
            output.inc_indentation();
            print_node(&edge^.target);
            output.dec_indentation();
            output.write("}\n");
        );
    );
);
