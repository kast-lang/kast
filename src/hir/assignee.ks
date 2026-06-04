const AssigneeShape = newtype (
    | :Binding Binding
    | :Let Pattern
);

const Assignee = newtype {
    .shape :: AssigneeShape,
    .span :: Span,
    .ty :: Ty,
};
