# TODO mark generator with yielding effect
let generator = fn(void) {
	print "yielding 1";
	yield "1";
	print "yielding 2";
	yield "2";
};

for value in generator () {
	print value;
}
