const MyList = [T1] newtype { .value :: T1 };

const create_my_list = [T2] (value :: T2) -> MyList[T2] => {
    .value
};

const Type = @native "Type";
const String :: Type = @native "String";

const foo = (:: MyList[Type]) (create_my_list(String));

let (:: Int32) x = 123;
