let f = x => x;
# f "123";
let generic = [T] (x :: T) => f(x);
# generic[_] "string";
# generic[_] 123
