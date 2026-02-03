const Ast = @native "Ast";

impl Ast as module = (
    module:

    const number_literal :: Int32 -> Ast = @native "syntax.number_literal";
    const ident :: String -> Ast = @native "syntax.ident";
);