const reflection = (
    module:
    
    const Field = newtype {
        .name :: String,
        .ty :: TypeInfo,
    };
    
    const TypeInfo = newtype (
        | :Int32
        | :Tuple List.t[Field]
    );
    
    const get_info :: Type -> TypeInfo = @native "reflection.get_info";
);

@syntax "@field" 10 @wrap never = "@field" "(" obj "," " " field ")";
impl syntax (@field(obj, field)) = `(
    # TODO
    _
);

(
    module:
    const default_value = [T :: Type] T => with_return (
        match reflection.get_info(T) with (
            | :Int32 => 0
            | :Tuple fields => (
                let mut result = mem.unintialized(T);
                for field in List.iter(&fields) do (
                    @field(result, field.name) = default_value(field^.ty);
                );
                result
            )
        )
    );
)
