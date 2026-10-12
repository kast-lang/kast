let x = "hi";

let f = @call "C" () => (
    let y = x;
);

@native "\(@unsafe_copy f)";
