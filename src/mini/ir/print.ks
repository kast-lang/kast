use (import "../../span.ks").*;
use (import "../../output.ks").*;
use std.collections.OrdMap;
use std.collections.OrdSet;

use (import "./types.ks").*;

module:

const Print = (
    module:

    const fn_type = (self :: &FnType) => (
        let output = @current Output;
        output.write("(");
        for { i, arg_ty } in &self^.args |> ArrayList.iter |> std.iter.enumerate do (
            if i != 0 then (
                output.write(", ");
            );
            Print.type_name(arg_ty);
        );
        output.write(") -> ");
        Print.type_name(&self^.result);
    );

    const type_name = (self :: &Type) => (
        let output = @current Output;
        match self^ with (
            | :Any => output.write("Any")
            | :Unit => output.write("()")
            | :Bool => output.write("Bool")
            | :Int => output.write("Int")
            | :UInt => output.write("UInt")
            | :IntSpecific { .signed, .bits } => (
                if not signed then (
                    output.write("U");
                );
                output.write("Int");
                output.write(to_string(bits));
            )
            | :Float64 => output.write("Float64")
            | :Char => output.write("Char")
            | :Named name => output.write(name)
            | :Fn ref ty => Print.fn_type(ty)
            | :Ref ref referenced => (
                output.write("&");
                Print.type_name(referenced);
            )
            | :Native s => output.write(s)
            | :UnwindToken {
                .instantiated_token_ty = _,
                .result_ty = ref result_ty,
            } => (
                output.write("UnwindToken[");
                Print.type_name(result_ty);
                output.write("]");
            )
        )
    );

    const fn_type_as_ident = (self :: &FnType) => (
        let output = @current Output;
        output.write("Fn");
        for arg_ty in &self^.args |> ArrayList.iter do (
            output.write("_");
            Print.type_name_as_ident(arg_ty);
        );
        output.write("_");
        Print.type_name_as_ident(&self^.result);
        output.write("_nF");
    );

    const raw_string_as_ident = (s :: String) => (
        let output = @current Output;
        for c in String.iter(s) do (
            let c = if Char.is_ascii_alphanumeric(c) or c == '_' then (
                c
            ) else (
                '_'
            );
            output.write(to_string(c));
        );
    );

    const type_name_as_ident = (self :: &Type) => (
        let output = @current Output;
        match self^ with (
            | :Any => output.write("Any")
            | :Unit => output.write("Unit")
            | :Bool => output.write("Bool")
            | :Int => output.write("Int")
            | :UInt => output.write("UInt")
            | :IntSpecific _ => Print.type_name(self)
            | :Float64 => output.write("Float64")
            | :Char => output.write("Char")
            | :Named name => output.write(name)
            | :Fn ref ty => Print.fn_type_as_ident(ty)
            | :Ref ref referenced => (
                output.write("Ref_");
                Print.type_name_as_ident(referenced);
            )
            | :Native s => (
                output.write("Native_");
                Print.raw_string_as_ident(s);
            )
            | :UnwindToken {
                .instantiated_token_ty = ref instantiated_token_ty,
                .result_ty = _,
            } => (
                Print.type_name_as_ident(instantiated_token_ty);
            )
        )
    );

    const program = (self :: &Program) => (
        let output = @current Output;
        ansi.with_mode(
            :Bold,
            () => output.write("Types:\n"),
        );
        for &{ .key = name, .value = def } in &self^.types |> OrdMap.iter do (
            output.write("- ");
            output.write(name);
            output.write("\n");
        );
        ansi.with_mode(
            :Bold,
            () => output.write("Functions:\n"),
        );
        for &{ .key = name, .value = def } in &self^.fns |> OrdMap.iter do (
            output.write("- ");
            output.write(name);
            output.write("\n");
        );
    );
);
