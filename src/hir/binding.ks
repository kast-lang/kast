const Binding = Rc.t[newtype {
    .id :: Id,
    .name :: RcStr,
    .ty :: Ty,
    .mutable :: Bool,
    .span :: Span,
}];
