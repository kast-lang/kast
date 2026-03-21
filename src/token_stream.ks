use (import "./token.ks").*;

module:

const TokenStream = (
    module:

    const t = newtype {
        .peeked :: Token.t,
        .next :: () -> Token.t,
    };
    
    const from_fn = (f :: () -> Token.t) -> TokenStream.t => {
        .peeked = f(),
        .next = f,
    };
    
    const peek = (self :: &TokenStream.t) -> Token.t => (
        self^.peeked
    );
    
    const advance = (self :: &mut TokenStream.t) => (
        self^.peeked = (self^.next)();
    );
);
