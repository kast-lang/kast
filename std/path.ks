module:

const dirname = (path :: &str) -> &str => (
    let slash_idx = path |> String.last_index_of('/');
    String.substring(path, 0, slash_idx)
);
