use std.*;

let show_current_context = () => (
	print &(current string);
);

(
    with "123";
    show_current_context();
);

(
    with "456";
	with "shadow";
    show_current_context();
);

show_current_context(); # This should not compile (no context available)
