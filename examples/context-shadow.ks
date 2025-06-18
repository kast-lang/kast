use std.*;

let show_current_context = () => (
	print &(current string);
);

(
    with "123" :: string;
    show_current_context();
);

(
    with "456" :: string;
    (
        with "shadow" :: string;
        show_current_context();
    );
    show_current_context();
);

show_current_context(); # TODO This should not compile (no context available)
