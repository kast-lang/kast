module:

const Compare = [T] type ((T, T) -> Ordering);

const Ordering = newtype (
    | :Less
    | :Equal
    | :Greater 
);

impl Ordering as module = (
    module:

    const is_less = (self :: Ordering) -> Bool => match self with (
        | :Less => true
        | _ => false
    );

    const is_less_or_equal = (self :: Ordering) -> Bool => match self with (
        | :Greater => false
        | _ => true
    );

    const is_equal = (self :: Ordering) -> Bool => match self with (
        | :Equal => true
        | _ => false
    );

    const is_not_equal = (self :: Ordering) -> Bool => match self with (
        | :Equal => false
        | _ => true
    );

    const is_greater_or_equal = (self :: Ordering) -> Bool => match self with (
        | :Less => false
        | _ => true
    );

    const is_greater = (self :: Ordering) -> Bool => match self with (
        | :Greater => true
        | _ => false
    );
);

const Ord = [Self] newtype {
    .compare :: (Self, Self) -> Ordering,
};

impl Bool as Ord = {
    .compare = (a, b) => @cfg (
        | (@native "==")(target.name, "interpreter") => (@native "cmp")(a, b)
        | (@native "==")(target.name, "c") => (
            if @native "\(a) < \(b)" then (
                :Less
            ) else if @native "\(a) > \(b)" then (
                :Greater
            ) else (
                :Equal
            )
        )
        | (@native "==")(target.name, "javascript") => (
            if @native "\(a) < \(b)" then (
                :Less
            ) else if @native "\(a) > \(b)" then (
                :Greater
            ) else (
                :Equal
            )
        )
    )
};

impl Int32 as Ord = {
    .compare = (a, b) => @cfg (
        | (@native "==")(target.name, "interpreter") => (@native "cmp")(a, b)
        | (@native "==")(target.name, "c") => (
            if @native "\(a) < \(b)" then (
                :Less
            ) else if @native "\(a) > \(b)" then (
                :Greater
            ) else (
                :Equal
            )
        )
        | (@native "==")(target.name, "javascript") => (
            if @native "\(a) < \(b)" then (
                :Less
            ) else if @native "\(a) > \(b)" then (
                :Greater
            ) else (
                :Equal
            )
        )
    )
};

impl Int64 as Ord = {
    .compare = (a, b) => @cfg (
        | (@native "==")(target.name, "interpreter") => (@native "cmp")(a, b)
        | (@native "==")(target.name, "c") => (
            if @native "\(a) < \(b)" then (
                :Less
            ) else if @native "\(a) > \(b)" then (
                :Greater
            ) else (
                :Equal
            )
        )
        | (@native "==")(target.name, "javascript") => (
            if @native "\(a) < \(b)" then (
                :Less
            ) else if @native "\(a) > \(b)" then (
                :Greater
            ) else (
                :Equal
            )
        )
    )
};

impl Float64 as Ord = {
    .compare = (a, b) => @cfg (
        | (@native "==")(target.name, "interpreter") => (@native "cmp")(a, b)
        | (@native "==")(target.name, "c") => (
            if @native "\(a) < \(b)" then (
                :Less
            ) else if @native "\(a) > \(b)" then (
                :Greater
            ) else (
                :Equal
            )
        )
        | (@native "==")(target.name, "javascript") => (
            if @native "\(a) < \(b)" then (
                :Less
            ) else if @native "\(a) > \(b)" then (
                :Greater
            ) else (
                :Equal
            )
        )
    )
};

impl Char as Ord = {
    .compare = (a, b) => @cfg (
        | (@native "==")(target.name, "interpreter") => (@native "cmp")(a, b)
        | (@native "==")(target.name, "c") => (
            if @native "\(a) < \(b)" then (
                :Less
            ) else if @native "\(a) > \(b)" then (
                :Greater
            ) else (
                :Equal
            )
        )
        | (@native "==")(target.name, "javascript") => (
            if @native "\(a) < \(b)" then (
                :Less
            ) else if @native "\(a) > \(b)" then (
                :Greater
            ) else (
                :Equal
            )
        )
    )
};

impl String as Ord = {
    .compare = (a, b) => @cfg (
        | (@native "==")(target.name, "interpreter") => (@native "cmp")(a, b)
        | (@native "==")(target.name, "c") => (
            let cmp :: Int32 = @native "String_cmp(\(a), \(b))";
            if @native "\(cmp) < 0" then (
                :Less
            ) else if @native "\(cmp) > 0" then (
                :Greater
            ) else (
                :Equal
            )
        )
        | (@native "==")(target.name, "javascript") => (
            if @native "\(a) < \(b)" then (
                :Less
            ) else if @native "\(a) > \(b)" then (
                :Greater
            ) else (
                :Equal
            )
        )
    )
};

const less = [T] (a :: T, b :: T) -> Bool => (
    (T as Ord).compare(a, b) |> Ordering.is_less
);
const less_or_equal = [T] (a :: T, b :: T) -> Bool => (
    (T as Ord).compare(a, b) |> Ordering.is_less_or_equal
);
const equal = [T] (a :: T, b :: T) -> Bool => (
    (T as Ord).compare(a, b) |> Ordering.is_equal
);
const not_equal = [T] (a :: T, b :: T) -> Bool => (
    (T as Ord).compare(a, b) |> Ordering.is_not_equal
);
const greater_or_equal = [T] (a :: T, b :: T) -> Bool => (
    (T as Ord).compare(a, b) |> Ordering.is_greater_or_equal
);
const greater = [T] (a :: T, b :: T) -> Bool => (
    (T as Ord).compare(a, b) |> Ordering.is_greater
);

const default_compare = [T] (a :: T, b :: T) -> Ordering => (
    if a < b then (
        :Less
    ) else if a == b then (
        :Equal
    ) else (
        :Greater
    )
);
