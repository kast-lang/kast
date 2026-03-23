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

    const get_named = [T] (
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
        .delimeter :: String,
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
            output.write(delimeter);
            output.write("\n");
        );
        for &name in &self^.name_order |> ArrayList.iter do (
            output.write(before_field_name);
            output.write(name);
            output.write(after_field_name);
            print_value(&self^.named |> OrdMap.get(name) |> Option.unwrap);
            output.write(delimeter);
            output.write("\n");
        );
        output.dec_indentation();
        output.write(close);
    );
);
