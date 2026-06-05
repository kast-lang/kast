const PlaceState = newtype (
    | :Uninitialized
    | :Occupied Value
    | :MovedOut
);

const Place = newtype {
    .id :: Id,
    .state :: PlaceState,
    .ty :: Ty,
};

impl Place as module = (
    module:

    const uninitialized = (.ty :: Ty) -> Place => {
        .id = Id.gen(),
        .state = :Uninitialized,
        .ty,
    };

    const init = (value :: Value) -> Place => {
        .id = Id.gen(),
        .ty = value.ty,
        .state = :Occupied value,
    };
)
