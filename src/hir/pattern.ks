const PatternShape = newtype (
    | :Binding Binding
);

const Pattern = newtype {
    .shape :: PatternShape,
    .span :: Span,
    .ty :: Ty,
};