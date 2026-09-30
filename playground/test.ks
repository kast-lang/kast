const Map = std.collections.OrdMap;

const Graph = (
    module:
    const VertexId = String;
    const Vertex = [T] newtype {
        .id :: VertexId,
        .data :: T,
        .out :: ArrayList.t[type (&mut Vertex[T])],
    };
    const t = [T] newtype {
        .vs :: Map.t[VertexId, Box[Vertex[T]]],
    };
    const new = [T] () -> t[T] => {
        .vs = Map.new()
    };
    const get_or_init_vertex = [T] (
        g :: &mut t[T],
        id :: VertexId,
        init :: () -> T,
    ) -> &mut Vertex[T] => (
        Map.get_or_init(
            &mut g^.vs,
            id,
            () => Box_new({
                .id,
                .data = init(),
                .out = ArrayList.new(),
            }),
        )^
    );
    const get_mut = [T] (g :: &mut t[T], id :: VertexId) -> &mut Vertex[T] => (
        get_or_init_vertex(g, id, () => panic("vertex not found"))
    );
    const get = [T] (g :: &t[T], id :: VertexId) -> &Vertex[T] => (
        &(Map.get(&g^.vs, id) |> Option.unwrap)^^
    );
    const print = [T] (g :: &t[T]) => (
        for &{ .key = id, .value = v } in Map.iter(&g^.vs) do (
            let mut s = id + ": ";
            let mut first = true;
            for &u in ArrayList.iter(&v^.out) do (
                if first then (
                    first = false;
                ) else (
                    s += ", "
                );
                
                s += u^.id;
            );
            
            std.io.print(s);
        );
    );
);

Graph.new[Int32]();
