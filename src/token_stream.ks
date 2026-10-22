use (import "./token.ks").*;
use std.collections.OrdMap;

module:

const TokenStream = (
    module:

    const t = newtype {
        .index :: Int32,
        .prev :: Option.t[Token.t],
        .peeked :: Token.t,
        .next :: () -> Token.t,
        .next_recording_id :: Int32,
        .recordings :: OrdMap.t[Int32, ArrayList.t[Token.t]],
    };

    const Recording = newtype {
        .id :: Int32,
    };

    const start_recording = (self :: &mut TokenStream.t) -> Recording => (
        let id = self^.next_recording_id;
        self^.next_recording_id += 1;
        &mut self^.recordings |> OrdMap.add(id, ArrayList.new());
        { .id }
    );

    const finish_recording = (
        self :: &mut TokenStream.t,
        { .id } :: Recording,
    ) -> ArrayList.t[Token.t] => (
        match &mut self^.recordings |> OrdMap.remove(&id) with (
            | :Some tokens => tokens
            | :None => panic("Recording not found")
        )
    );

    const from_fn = (f :: () -> Token.t) -> TokenStream.t => {
        .index = 0,
        .prev = :None,
        .peeked = f(),
        .next = f,
        .next_recording_id = 0,
        .recordings = OrdMap.new(),
    };

    const prev = (self :: &TokenStream.t) -> Option.t[Token.t] => (
        self^.prev
    );

    const peek = (self :: &TokenStream.t) -> Token.t => (
        self^.peeked
    );

    const advance = (self :: &mut TokenStream.t) => (
        let token = self^.peeked;
        for &mut {
            .key = _,
            .value = ref mut tokens,
        } in &mut self^.recordings |> OrdMap.iter_mut do (
            tokens |> ArrayList.push_back(token);
        );
        self^.prev = :Some token;
        self^.index += 1;
        self^.peeked = (self^.next)();
    );
);
