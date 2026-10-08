let x = 123;
let x = &x;

let f = (
    let f = () => x;
    f
);

f();
f();

