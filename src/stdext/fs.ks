module:

const exists = (path :: String) -> Bool => (
    @native "fs.existsSync(\(path))"
);
