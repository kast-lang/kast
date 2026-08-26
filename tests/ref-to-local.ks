const f = (value) => (
    let x = value;
    () => x
);

let x = f(123);
let x = x();

@native "printf(\"%d\\n\", \(x))";
