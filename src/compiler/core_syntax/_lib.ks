use (import "./_common.ks").*;

const super = @current_scope;

module:

const CoreSyntax = (
    module:

    const t = super.CoreSyntax;

    const Map = type (OrdMap.t[String, CoreSyntax.t]);

    const add = (map :: &mut Map, item :: t) => (
        map |> OrdMap.add(item.name, item);
    );

    const init = () -> CoreSyntax.Map => (
        let mut map = OrdMap.new();
        add(&mut map, import "./assign.ks");
        add(&mut map, import "./let.ks");
        add(&mut map, import "./type_ascribe.ks");
        map
    );
);
