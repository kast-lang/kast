const PatternShape = newtype (
    | :Binding Binding
);

const Pattern = Rc.t[newtype {
    .shape :: PatternShape,
    .span :: Span,
    .ty :: Ty,
}];
