const StringBuilder = (
    module:

    const t = newtype {
        .result :: String,
    };

    const new = () -> StringBuilder.t => {
        .result = String.from_str(""),
    };

    const add_str = (self :: &mut StringBuilder.t, s :: &str) => (
        self |> add_String(String.from_str(s));
    );

    const add_String = (self :: &mut StringBuilder.t, s :: String) => (
        self^.result = String.concat_owned(self^.result, s);
    );

    const into_string = (self :: StringBuilder.t) -> String => (
        self.result
    );
);
