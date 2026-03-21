use (import "./token.ks").*;

module:

const TokenStream = (
    module:

    const t = newtype {
        .index :: Int32,
        .peeked :: Token.t,
        .next :: () -> Token.t,
    };
    
    const from_fn = (f :: () -> Token.t) -> TokenStream.t => {
        .index = 0,
        .peeked = f(),
        .next = f,
    };
    
    const peek = (self :: &TokenStream.t) -> Token.t => (
        self^.peeked
    );
    
    const advance = (self :: &mut TokenStream.t) => (
        self^.index += 1;
        self^.peeked = (self^.next)();
    );
);
