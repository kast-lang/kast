use std.prelude.*;

const HashMap = std.collections.HashMap;
const HashSet = struct (
    const HashSet = forall[T :: type] { HashMap.HashMap[T, ()] };
    const new = forall[T :: type] { 
        fn (()) -> HashSet[T] {
            HashMap.new[T, ()]()
        }
    };
    const insert = forall[T :: type] {
        fn (set :: &HashSet[T], value :: T) {
            HashMap.insert[T, ()](set, value, ());
        }
    };
    const contains = forall[T :: type] {
        fn (set :: &HashSet[T], value :: &T) -> bool {
            HashMap.get[T, ()] (set, value) is :Some _
        }
    };
    const size = forall[T :: type] {
        fn (set :: &HashSet[T]) -> int32 {
            HashMap.size[T, ()] set
        }
    }
);

const max = forall[T] {
    fn (a :: T, b :: T) -> T {
        if a > b then a else b
    }
};

const List = std.collections.List;

const VertexId = int32;
const Vertex = type (
    .edges :: list[VertexId],
);
const Tree = type (
    .vertices :: HashMap.HashMap[VertexId, Vertex],
);

let tree_size = fn (tree :: &Tree) -> int32 {
    HashMap.size &(tree^).vertices
};

const Edge = type (VertexId, VertexId);

let make_tree = fn(edges :: &list[Edge]) -> Tree {
    let mut tree :: Tree = (
        .vertices = HashMap.new (),
    );
    let insert_edge = (a :: VertexId, b :: VertexId) => (
        if HashMap.get (&tree.vertices, &a) is :None then (
            HashMap.insert (&tree.vertices, a, ( .edges = list[] ));
        );
        let :Some a = HashMap.get (&tree.vertices, &a);
        List.push (&(a^).edges, b);
    );
    for edge :: &Edge in List.iter edges {
        let (a :: VertexId, b :: VertexId) = edge^;
        insert_edge (a, b);
        insert_edge (b, a);
    };
    tree
};

const TreeData = type (
    .odd :: HashSet.HashSet[VertexId],
    .even :: HashSet.HashSet[VertexId],
);

let tree_data = fn(tree :: &Tree) -> TreeData {
    let mut result :: TreeData = ( .odd = HashSet.new(), .even = HashSet.new() );
    let root :: VertexId = 0;
    _ = rec (
        let traverse = fn(current_vertex_id :: VertexId, prev_vertex_id :: VertexId, even :: bool) {
            let result_store = if even then ( &result.even ) else ( &result.odd );
            HashSet.insert (result_store, current_vertex_id);
            let :Some current_vertex = HashMap.get (&(tree^).vertices, &current_vertex_id);
            for neighbor_id :: &VertexId in List.iter &(current_vertex^).edges {
                let neighbor_id = neighbor_id^;
                if neighbor_id != prev_vertex_id then (
                    traverse(neighbor_id, current_vertex_id, not even);
                );
            };
        };
        traverse(root, root, true);
    );
    result
};

let max_target_nodes = fn(.edges1 :: list[Edge], .edges2 :: list[Edge]) -> list[int32] {
    let tree1 = make_tree &edges1;
    let tree1_data = &tree1 |> tree_data;
    let tree2 = make_tree &edges2;
    let tree2_data = &tree2 |> tree_data;
    let max_tree2_added_targets = max(HashSet.size &tree2_data.odd, HashSet.size &tree2_data.even);
    # dbg max_tree2_added_targets;
    # for v in 0..tree_size tree1
    let mut vertex_id = 0;
    let mut result = list[];
    unwindable _loop (
        loop {
            if not (vertex_id < tree_size &tree1) then (
                unwind _loop ();
            );
            let current_targets_in_tree_1 = if HashSet.contains(&tree1_data.even, &vertex_id) then
                HashSet.size &tree1_data.even
            else
                HashSet.size &tree1_data.odd;
            List.push(&result, current_targets_in_tree_1 + max_tree2_added_targets);
            vertex_id += 1;
        };
    );
    result
};

let test = fn(.edges1 :: list[Edge], .edges2 :: list[Edge], .expected_result :: list[int32]) {
    let actual_result = max_target_nodes(.edges1, .edges2);
    if actual_result != expected_result then (
        print "expected_result:";
        dbg expected_result;
        print "actual_result:";
        dbg actual_result;
        panic "test fails";
    );
    # print "test passed";
};

test(
    .edges1 = list[(0,1),(0,2),(2,3),(2,4)],
    .edges2 = list[(0,1),(0,2),(0,3),(2,7),(1,4),(4,5),(4,6)],
    .expected_result = list[8,7,7,8,8],
);
test(
    .edges1 = list[(0,1),(0,2),(0,3),(0,4)],
    .edges2 = list[(0,1),(1,2),(2,3)],
    .expected_result = list[3,6,6,6,6],
);

let max_target_nodes = fn(.edges1 :: list[list[int32]], .edges2 :: list[list[int32]]) -> list[int32] {
    let convert = fn(edges :: list[list[int32]]) -> list[Edge] {
        let mut result = list[];
        for edge :: &list[int32] in List.iter &edges {
            if List.length edge != 2 then panic "wtf";
            let a, b = List.get(edge, 0), List.get(edge, 1);
            let edge2 :: Edge = a^, b^;
            List.push(&result, edge2);
        };
        result
    };
    let edges1 = edges1 |> convert;
    let edges2 = edges2 |> convert;
    max_target_nodes(.edges1, .edges2)
};
let js_code :: string = std.javascript.transpile max_target_nodes;
print "var f=";
print &js_code;

print "maxTargetNodes=({edges1,edges2})=>f({},{edges1,edges2})";

# print "console.log(maxTargetNodes({edges1:[[0,1],[0,2],[2,3],[2,4]],edges2:[[0,1],[0,2],[0,3],[2,7],[1,4],[4,5],[4,6]]}))";
# print "console.log(maxTargetNodes({edges1:[[0,1],[0,2],[0,3],[0,4]],edges2:[[0,1],[1,2],[2,3]]}))";
print "let input = require(`${process.cwd()}/examples/leetcode/3373.input.json`);";
print "console.log(maxTargetNodes(input))"
