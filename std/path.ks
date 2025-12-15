module:
const dirname :: String -> String = path => (
    let slash_idx = String.last_index_of ('/', path);
    String.substring (path, 0, slash_idx)
);
