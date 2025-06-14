use std.*;

let commands = (
    .hellopgorley = fn(_args :: list[&string]) -> Option[string] {
        :Some "hello, pgorley"
    },
);

let is_whitespace = fn(c :: char) -> bool {
    c == ' '
};

let split_whitespace = fn(s :: &string) -> () with loop_context[string] {
    let cur :: Option[string] = :None;
    let finish = () => (
        if cur is :Some cur then (
            yield cur;
        );
        cur = :None;
    );
    for c :: char in chars s {
        if is_whitespace c then (
            finish();
        ) else (
            if cur is :None then ( cur = :Some "" );
            let :Some (ref cur) = cur;
            cur^ = push_char (cur^, c);
        )
    };
    finish();
};

let handle_chat = fn(s :: &string) -> Option[string] {
    for arg :: string in split_whitespace s {
        dbg arg;
    };
    :None
};

if handle_chat "!hellopgorley" is :Some reply then (
    print &reply;
);
