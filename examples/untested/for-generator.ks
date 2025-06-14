use std.*;

let generator = fn(()) {
	print "yielding 1";
	yield "1";
	print "yielding 2";
	yield "2";
	print "yielding stop";
	yield "stop";
	print "yielding 3";
	yield "3";
};

for value in generator() {
	print value;
  if value == "stop" then (
    break;
  );
};
