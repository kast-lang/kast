use (import "src/id.ks").*;
use (import "src/tty.ks").*;
use std.collections.OrdMap;

const Tui = (
    module:

    const State = newtype {
        .click_handlers :: OrdMap.t[Id, ClickHandler],
        .root :: Id,
        .effects :: OrdMap.t[Id, EffectState],
        .dependent :: OrdMap.t[Id, ArrayList.t[Id]],
    };

    const EffectState = newtype {
        .redraw :: () -> (),
        .drawn :: Option.t[ArrayList.t[Drawn]],
        .click_handlers :: ArrayList.t[Id],
    };

    const Drawn = newtype (
        | :String String
        | :Effect Id
    );

    const ClickHandler = newtype {
        .start_pos :: Pos,
        .end_pos :: Pos,
        .on_click :: () -> (),
    };

    const Context = @context State;

    const run = (f :: () -> ()) => with_return (
        let root = Id.gen();
        with Context = {
            .root,
            .click_handlers = OrdMap.new_with_compare(Id.compare),
            .effects = OrdMap.new_with_compare(Id.compare),
            .dependent = OrdMap.new_with_compare(Id.compare),
        };
        with Effect = {
            .id = root,
        };
        tty.run(
            () => (
                tty.clear_screen();
                tty.move_cursor_to(1, 1);
                let redraw = () => (
                    (
                        &mut (@current Context).effects
                            |> OrdMap.get_mut(root)
                            |> Option.unwrap
                    )^.drawn = :Some ArrayList.new();
                    with Effect = { .id = root };
                    f();
                );
                &mut (@current Context).effects
                    |> OrdMap.add(
                        root,
                        {
                            .drawn = :None,
                            .redraw,
                            .click_handlers = ArrayList.new(),
                        }
                    );
                redraw();
                tty.flush();
                mainloop();
            ),
            .handle_ctrl_c = () => return,
        );
    );

    const Signal = [T] newtype {
        .get :: () -> T,
        .set :: T -> (),
    };

    const Effect = @context newtype {
        .id :: Id,
    };

    const add_drawn = (drawn :: Drawn) => (
        let effect_id = (@current Effect).id;
        let effect = &mut (@current Context).effects
            |> OrdMap.get_mut(effect_id)
            |> Option.unwrap;
        let :Some ref mut effect_drawn = effect^.drawn;
        effect_drawn |> ArrayList.push_back(drawn);
    );

    const signal = [T] (initial_value :: T) -> Signal[T] => (
        let mut value = initial_value;
        let id = Id.gen();
        {
            .get = () => (
                &mut (@current Context).dependent
                    |> OrdMap.get_or_init(id, ArrayList.new[_])
                    |> ArrayList.push_back((@current Effect).id);
                value
            ),
            .set = new_value => (
                value = new_value;
                if &(@current Context).dependent |> OrdMap.get(id) is :Some dependent then (
                    for &effect_id in dependent |> ArrayList.iter do (
                        if effect_id == (@current Context).root then (
                            continue;
                        );
                        let effect = &mut (@current Context).effects
                            |> OrdMap.get_mut(effect_id)
                            |> Option.unwrap;
                        effect^.drawn = :None;
                        for &handler_id in &effect^.click_handlers |> ArrayList.iter do (
                            &mut (@current Context).click_handlers |> OrdMap.remove(handler_id);
                        );
                        effect^.click_handlers = ArrayList.new();
                    );
                );
                redraw_all();
            ),
        }
    );

    const redraw_effect = (id :: Id) => (
        let effect = &mut (@current Context).effects
            |> OrdMap.get_mut(id)
            |> Option.unwrap;
        match effect^.drawn with (
            | :None => (
                effect^.redraw();
            )
            | :Some ref drawn => (
                for item in drawn |> ArrayList.iter do (
                    match item^ with (
                        | :String s => tty.write(s)
                        | :Effect id => redraw_effect(id)
                    )
                );
            )
        );
    );

    const redraw_all = () => (
        tty.clear_screen();
        tty.move_cursor_to(1, 1);
        let ctx = @current Context;
        redraw_effect(ctx.root);
        tty.flush();
    );

    const Pos = newtype {
        .row :: Int32,
        .col :: Int32,
    };

    const compare_pos = (a :: Pos, b :: Pos) -> std.cmp.Ordering => with_return (
        match std.cmp.default_compare(a.row, b.row) with (
            | :Equal => ()
            | other => return other
        );
        std.cmp.default_compare(a.col, b.col)
    );

    const current_pos = () -> Pos => (
        let { row, col } = tty.read_cursor_position();
        { .row, .col }
    );

    const set_area_click_handler = (
        start_pos :: Pos,
        end_pos :: Pos,
        on_click :: () -> (),
    ) => (
        let on_click = () => (
            on_click();
        );
        let id = Id.gen();
        &mut (@current Context).click_handlers
            |> OrdMap.add(id, { .start_pos, .end_pos, .on_click });
        let effect = &mut (@current Context).effects
            |> OrdMap.get_mut((@current Effect).id)
            |> Option.unwrap;
        &mut effect^.click_handlers |> ArrayList.push_back(id);
    );

    const createEffect = (f :: () -> ()) => (
        let id = Id.gen();
        add_drawn(:Effect id);
        let redraw = () => (
            (
                &mut (@current Context).effects
                    |> OrdMap.get_mut(id)
                    |> Option.unwrap
            )^.drawn = :Some ArrayList.new();
            with Effect = { .id };
            f();
        );
        &mut (@current Context).effects
            |> OrdMap.add(
                id,
                {
                    .drawn = :None,
                    .redraw,
                    .click_handlers = ArrayList.new(),
                }
            );
        redraw();
    );

    const button = (text :: () -> String, on_click :: () -> ()) => (
        createEffect(
            () => (
                let start_pos = current_pos();
                write(text());
                let end_pos = current_pos();
                set_area_click_handler(start_pos, end_pos, on_click);
            )
        );
    );

    const write = (text :: String) => (
        add_drawn(:String text);
        tty.write(text);
    );

    const write_signal = [T] (signal :: Signal[T]) => (
        createEffect(
            () => (
                write(to_string[T](signal.get()));
            )
        );
    );

    const mainloop = () => (
        loop (
            let input = tty.input();
            match input.shape with (
                | :Mouse { .row, .col, .event = :Press button } => (
                    let pos :: Pos = { .row, .col };
                    for &{ .key = _, .value = ref handler } in &(@current Context).click_handlers |> OrdMap.iter do (
                        if compare_pos(pos, handler^.start_pos) is :Less then (
                            continue;
                        );
                        if compare_pos(pos, handler^.end_pos) is :Less then (
                            handler^.on_click();
                        );
                    );
                )
                | _ => ()
            )
        );
    );
);

use Tui.*;

module:

const counter = () => (
    let value = signal(0);
    button(() => "[-]", () => value.set(value.get() - 1));
    write(" ");
    write_signal(value);
    write(" ");
    button(() => "[+]", () => value.set(value.get() + 1));
);

const foldable = (text :: String) => (
    let folded = signal(false);
    button(
        () => if folded.get() then "[+]" else "[-]",
        () => folded.set(not folded.get()),
    );
    write(" ");
    createEffect(
        () => (
            if not folded.get() then (
                write(text);
            );
        )
    );
);

with IdGenCtx = IdGen.new();

Tui.run(
    () => (
        counter();
        write("\n");
        foldable("Some text");
        write("\n");
    )
);
