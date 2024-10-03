syntax then -> 0 = _ ";" _;
syntax then -> 0 = _ ";";

syntax add <- 20 = lhs "+" rhs;
syntax sub <- 20 = lhs "-" rhs;
syntax mul <- 30 = lhs "*" rhs;
syntax call <- 50 = f args;

syntax scoped <- 1000 = "(" e ")";
