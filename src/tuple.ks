use (import "./output.ks").*;
use std.collections.OrdMap;
use std.Ast;

module:

const Tuple = (
    module:

    const t = [T] newtype {
        .unnamed :: ArrayList.t[T],
        .named :: OrdMap.t[String, T],
        .name_order :: ArrayList.t[String],
    };

    const new = [T] () -> Tuple.t[T] => {
        .unnamed = ArrayList.new(),
        .named = OrdMap.new(),
        .name_order = ArrayList.new(),
    };

    const Member = newtype (
        | :Index Int32
        | :Name String
    );

    const get = [T] (
        self :: &Tuple.t[T],
        member :: Member,
    ) -> &T => (
        match member with (
            | :Index i => get_unnamed(self, i)
            | :Name name => get_named(self, name)
        )
    );

    const get_unnamed = [T] (
        self :: &Tuple.t[T],
        idx :: Int32,
    ) -> &T => (
        if 0 <= idx and idx < ArrayList.length(&self^.unnamed) then (
            ArrayList.at(&self^.unnamed, idx)
        ) else (
            panic("tuple idx out of range")
        )
    );

    const get_named = [T] (
        self :: &Tuple.t[T],
        name :: String,
    ) -> &T => (
        match &self^.named |> OrdMap.get(name) with (
            | :Some value => value
            | :None => panic("tuple doesnt have field " + String.escape(name))
        )
    );

    const has_named = [T] (
        self :: &Tuple.t[T],
        name :: String,
    ) -> Bool => (
        match &self^.named |> OrdMap.get(name) with (
            | :Some _ => true
            | :None => false
        )
    );

    const get_opt = [T] (
        self :: &Tuple.t[T],
        member :: Member,
    ) -> Option.t[type (&T)] => (
        match member with (
            | :Index i => get_unnamed_opt(self, i)
            | :Name name => get_named_opt(self, name)
        )
    );

    const get_unnamed_opt = [T] (
        self :: &Tuple.t[T],
        idx :: Int32,
    ) -> Option.t[type (&T)] => (
        if idx < ArrayList.length(&self^.unnamed) then (
            :Some ArrayList.at(&self^.unnamed, idx)
        ) else (
            :None
        )
    );

    const get_named_opt = [T] (
        self :: &Tuple.t[T],
        name :: String,
    ) -> Option.t[type (&T)] => (
        &self^.named |> OrdMap.get(name)
    );

    const add = [T] (
        self :: &mut Tuple.t[T],
        name :: Option.t[String],
        value :: T,
    ) => (
        match name with (
            | :Some name => self |> add_named(name, value)
            | :None => self |> add_unnamed(value)
        );
    );

    const add_named = [T] (
        self :: &mut Tuple.t[T],
        name :: String,
        value :: T,
    ) => (
        &mut self^.named |> OrdMap.add(name, value);
        &mut self^.name_order |> ArrayList.push_back(name);
    );

    const add_unnamed = [T] (
        self :: &mut Tuple.t[T],
        value :: T,
    ) => (
        &mut self^.unnamed |> ArrayList.push_back(value);
    );

    const array_to_tuple = (a :: Ast, N :: Int32) -> Ast => @cfg (
        | target.name == "interpreter" => (
            if N == 0 then `(


            ) else if N == 1 then `(
                (&$a |> ArrayList.at(0))^
            ) else `(
                $(array_to_tuple(a, N - 1)), (&$a |> ArrayList.at(N - 1))^
            )
        )
        | true => panic("comptime only")
    );

    const unwrap_unnamed_impl = (self :: Ast, N :: Int32) -> Ast => @cfg (
        | target.name == "interpreter" => `(
            if &$self.name_order |> ArrayList.length != 0 then (
                panic("Has named fields");
            );
            let length = &$self.unnamed |> ArrayList.length;
            if length != N then (
                panic(
                    "Expected "
                    + to_string(N)
                    + "unnamed fields, got "
                    + to_string(length)
                );
            );
            { $(array_to_tuple(`($self.unnamed), N)) }
        )
        | true => panic("comptime only")
    );

    const unwrap_unnamed_2 = [T] (self :: Tuple.t[T]) -> { T, T } => (
        include_ast unwrap_unnamed_impl(`(self), 2)
    );

    const unwrap_unnamed_1 = [T] (self :: Tuple.t[T]) -> T => (
        let { inner } = include_ast unwrap_unnamed_impl(`(self), 1);
        inner
    );

    const print = [T] (
        self :: &Tuple.t[T],
        .open :: String,
        .delimiter :: String,
        .before_field_name :: String,
        .after_field_name :: String,
        .print_value :: &T -> (),
        .close :: String,
    ) => (
        let output = @current Output;
        output.write(open);
        output.write("\n");
        output.inc_indentation();
        for value in &self^.unnamed |> ArrayList.iter do (
            print_value(value);
            output.write(delimiter);
            output.write("\n");
        );
        for &name in &self^.name_order |> ArrayList.iter do (
            output.write(before_field_name);
            output.write(name);
            output.write(after_field_name);
            print_value(&self^.named |> OrdMap.get(name) |> Option.unwrap);
            output.write(delimiter);
            output.write("\n");
        );
        output.dec_indentation();
        output.write(close);
    );

    const iter = [T] (self :: &Tuple.t[T]) -> std.iter.Iterable[type { Member, &T }] => {
        .iter = consumer => (
            for { index, element } in &self^.unnamed |> ArrayList.iter |> std.iter.enumerate do (
                consumer({ :Index index, element });
            );
            for &name in &self^.name_order |> ArrayList.iter do (
                let element = &self^.named |> OrdMap.get(name) |> Option.unwrap;
                consumer({ :Name name, element });
            );
        ),
    };
);
