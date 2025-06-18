module:

const document = rec (
    const body = rec (
        const appendChild = (child) => (
            native "document.body.appendChild($(child))";
        );
    );
    const createElement = fn(name :: string) {
        native "document.createElement($(name))"
    };
);