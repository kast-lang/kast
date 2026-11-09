use (import "../../span.ks").*;
use (import "../../output.ks").*;
use (import "../../rc_str.ks").*;
use std.collections.OrdMap;
use std.collections.OrdSet;

use (import "./types.ks").*;

module:

const Print = (
    module:

    const fn_type = (self :: &FnType) => (
        if self^.call_convention is :Some ref s then (
            locked_output.write_str("@call ");
            locked_output.write_escaped(s);
            locked_output.write_str(" ");
        );
        locked_output.write_str("(");
        for { i, arg_ty } in &self^.args^ |> ArrayList.iter |> std.iter.enumerate do (
            if i != 0 then (
                locked_output.write_str(", ");
            );
            Print.type_name(arg_ty);
        );
        locked_output.write_str(") -> ");
        Print.type_name(&self^.result);
    );

    const type_name = (self :: &Type) => (
        if self^.alias_name is :Some ref name then (
            locked_output.write(name);
            locked_output.write_str(" (aka ");
        );
        match self^.shape^ with (
            | :Any => locked_output.write_str("Any")
            | :Unit => locked_output.write_str("()")
            | :Bool => locked_output.write_str("Bool")
            | :Int => locked_output.write_str("Int")
            | :UInt => locked_output.write_str("UInt")
            | :IntSpecific { .signed, .bits } => (
                if not signed then (
                    locked_output.write_str("U");
                );
                locked_output.write_str("Int");
                locked_output.write_str(&to_string(bits) |> as_str);
            )
            | :Float32 => locked_output.write_str("Float32")
            | :Float64 => locked_output.write_str("Float64")
            | :Char => locked_output.write_str("Char")
            | :Named ref name => locked_output.write(name)
            | :Fn ref ty => Print.fn_type(ty)
            | :Ref ref referenced => (
                locked_output.write_str("&");
                Print.type_name(referenced);
            )
            | :Native ref s => locked_output.write(s)
            | :UnwindToken {
                .repr = _,
                .result_ty = ref result_ty,
            } => (
                locked_output.write_str("UnwindToken[");
                Print.type_name(result_ty);
                locked_output.write_str("]");
            )
            | :DelimitedContinuationToken {
                .repr = _,
                .result_ty = ref result_ty,
            } => (
                locked_output.write_str("DelimitedContinuationToken[");
                Print.type_name(result_ty);
                locked_output.write_str("]");
            )
            | :Array {
                .repr = _,
                .element_ty = ref element_ty,
            } => (
                locked_output.write_str("Array[");
                Print.type_name(element_ty);
                locked_output.write_str("]");
            )
            | :ContextObject => locked_output.write_str("@Context")
        );
        if self^.alias_name is :Some _ then (
            locked_output.write_str(")");
        );
    );

    const fn_type_as_ident = (self :: &FnType) => (
        locked_output.write_str("Fn");
        for arg_ty in &self^.args^ |> ArrayList.iter do (
            locked_output.write_str("_");
            Print.type_name_as_ident(arg_ty);
        );
        locked_output.write_str("_");
        Print.type_name_as_ident(&self^.result);
        locked_output.write_str("_nF");
    );

    const raw_string_as_ident = (s :: &str) => (
        for c in String.iter(s) do (
            let c = if Char.is_ascii_alphanumeric(c) or c == '_' then (
                c
            ) else (
                '_'
            );
            locked_output.write_str(&to_string(c) |> as_str);
        );
    );

    const type_name_as_ident = (self :: &Type) => with_return (
        match type_repr(self)^.shape^ with (
            | :Any => locked_output.write_str("Any")
            | :Unit => locked_output.write_str("Unit")
            | :Bool => locked_output.write_str("Bool")
            | :Int => locked_output.write_str("Int")
            | :UInt => locked_output.write_str("UInt")
            | :IntSpecific _ => Print.type_name(self)
            | :Float32 => locked_output.write_str("Float32")
            | :Float64 => locked_output.write_str("Float64")
            | :Char => locked_output.write_str("Char")
            | :Named ref name => locked_output.write(name)
            | :Fn ref ty => Print.fn_type_as_ident(ty)
            | :Ref ref referenced => (
                locked_output.write_str("Ref_");
                Print.type_name_as_ident(referenced);
            )
            | :Native ref s => (
                locked_output.write_str("Native_");
                Print.raw_string_as_ident(s |> RcStr.as_str);
            )
            | :ContextObject => locked_output.write_str("Context")
        )
    );

    const program = (self :: &Program) => (
        ansi.with_mode(
            :Bold,
            () => locked_output.write_str("Types:\n"),
        );
        for &{ .key = ref name, .value = ref def } in &self^.types |> OrdMap.iter do (
            locked_output.write_str("- ");
            locked_output.write(name);
            locked_output.write_str("\n");
        );
        ansi.with_mode(
            :Bold,
            () => locked_output.write_str("Functions:\n"),
        );
        for &{ .key = ref name, .value = ref def } in &self^.fns |> OrdMap.iter do (
            locked_output.write_str("- ");
            locked_output.write(name);
            locked_output.write_str("\n");
        );
    );
);
