module:

const Uri = newtype {
    .path :: String,
};

impl Uri as module = (
    module:

    const new_path = (path :: String) -> Uri => {
        .path,
    };
);