let show_current_context = fn (void) {
	print (current string)
};

with "123" (
	show_current_context()
);
with "456" (
	with "shadow" (
		show_current_context()
	)
);
