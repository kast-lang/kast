use (import "./output.ks").*;

module:

const Uri = newtype {
    .path :: String,
};

impl Uri as module = (
    module:

    const new_path = (path :: String) -> Uri => {
        .path,
    };

    const print = (self :: Uri) => (
        let output = @current Output;
        output.write(self.path);
    );
);