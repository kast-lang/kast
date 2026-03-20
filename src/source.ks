use (import "../deps/uri/src/lib.ks").*;

module:

const Source = newtype {
    .contents :: String,
    .uri :: Uri,
};

impl Source as module = (
    module:

    const read_file = path -> Source => (
        let contents = std.fs.read_file(path);
        {
            .contents,
            .uri = Uri.new_path(path),
        }
    );
);