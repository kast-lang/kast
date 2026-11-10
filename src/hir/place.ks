const PlaceState = newtype (
    | :Uninitialized
    | :Occupied Value
    | :MovedOut
);

const Place = Rc.t[newtype {
    .id :: Id,
    .state :: PlaceState,
    .ty :: Ty,
}];

impl Place as module = (
    module:

    const uninitialized = (.ty :: Ty) -> Place => Rc.new({
        .id = Id.gen(),
        .state = :Uninitialized,
        .ty,
    });

    const init = (value :: Value) -> Place => Rc.new({
        .id = Id.gen(),
        .ty = value^.ty,
        .state = :Occupied value,
    });
)
