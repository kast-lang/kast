use std.*;

# TODO mark generator with yielding effect

let generator = fn(void) {
	print "yielding 1";
	yield "1";
	print "yielding 2";
	yield "2";
};

for value :: string in generator () {
	print &value;
};

print "now using generator as value";

# TODO types should be inferred

# let g = generator_value[Yield: string, Resume: void, Finish: void] generator;
# print "calling .next()";
# dbg <| g.next();
# print "calling .next()";
# dbg <| g.next();
# print "calling .next()";
# dbg <| g.next();
# dbg <| g.next(); # this one panics since generator is finished
