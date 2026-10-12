const f = () => (
    @native "#include <non-exitent-header.h>"
);

f();
