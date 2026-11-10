const AssigneeShape = newtype (
    | :Binding Binding
    | :Let Pattern
);

const Assignee = Rc.t[newtype {
    .shape :: AssigneeShape,
    .span :: Span,
    .ty :: Ty,
}];
