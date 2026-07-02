use (import "src/id.ks").*;
use (import "src/tty.ks").*;
use std.collections.OrdMap;

const Tui = (
    module:

    const State = newtype {
        .hovered_clickable :: Option.t[Id],
        .root :: Id,
        .effects :: OrdMap.t[Id, EffectState],
        .dependent :: OrdMap.t[Id, ArrayList.t[Id]],
    };

    const EffectState = newtype {
        .redraw :: () -> (),
        .drawn :: Option.t[ArrayList.t[Drawn]],
        .start_pos :: Pos,
        .end_pos :: Pos,
        .on_click :: Option.t[type (() -> ())],
    };

    const Drawn = newtype (
        | :String String
        | :Effect Id
    );

    const Context = @context State;
    const Visible = @context Bool;

    const new_effect = (redraw :: () -> ()) -> EffectState => {
        .redraw,
        .drawn = :None,
        .start_pos = { .row = 0, .col = 0 },
        .end_pos = { .row = 0, .col = 0 },
        .on_click = :None,
    };

    const run = (f :: () -> ()) => with_return (
        let root = Id.gen();
        with Context = {
            .hovered_clickable = :None,
            .root,
            .effects = OrdMap.new_with_compare(Id.compare),
            .dependent = OrdMap.new_with_compare(Id.compare),
        };
        with Visible = true;
        with Effect = {
            .id = root,
        };
        tty.run(
            () => (
                tty.clear_screen();
                tty.move_cursor_to(1, 1);
                tty.set_cursor_visibility(false);
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
                    |> OrdMap.add(root, new_effect(redraw));
                redraw();
                redraw_all();
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
        let :Some ref mut effect_drawn = current_effect_state()^.drawn;
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
                        effect^.on_click = :None;
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
        effect^.start_pos = current_pos();
        if effect^.drawn is :None then (
            effect^.redraw();
        );
        let is_hovered = if (@current Context).hovered_clickable is :Some hovered then (
            hovered == id
        ) else false;
        if is_hovered then (
            tty.invert_colors(true);
        );
        for item in &effect^.drawn
            |> Option.as_ref
            |> Option.unwrap
            |> ArrayList.iter do (
            match item^ with (
                | :String s => tty.write(s)
                | :Effect id => redraw_effect(id)
            )
        );
        if is_hovered then (
            tty.invert_colors(false);
        );
        effect^.end_pos = current_pos();
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

    const create_effect = (f :: () -> ()) -> Id => (
        let id = Id.gen();
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
            |> OrdMap.add(id, new_effect(redraw));
        redraw();
        id
    );

    const create_and_draw_effect = (f :: () -> ()) => (
        add_drawn(:Effect create_effect(f));
    );

    const current_effect_state = () -> &mut EffectState => (
        let id = (@current Effect).id;
        &mut (@current Context).effects
            |> OrdMap.get_mut(id)
            |> Option.unwrap
    );

    const set_on_click = (f :: () -> ()) => (
        current_effect_state()^.on_click = :Some f;
    );

    const button = (text :: () -> String, on_click :: () -> ()) => (
        create_and_draw_effect(
            () => (
                write(text());
                set_on_click(on_click);
            )
        );
    );

    const write = (text :: String) => (
        add_drawn(:String text);
    );

    const write_signal = [T] (signal :: Signal[T]) => (
        create_and_draw_effect(
            () => (
                write(to_string[T](signal.get()));
            )
        );
    );

    const find_hovered_clickable = (effect_id :: Id, pos :: Pos) -> Option.t[Id] => with_return (
        let effect :: &mut EffectState = &mut (@current Context).effects
            |> OrdMap.get_mut(effect_id)
            |> Option.unwrap;
        if compare_pos(pos, effect^.start_pos) is :Less then (
            return :None;
        );
        if compare_pos(pos, effect^.end_pos) is :Less then () else (
            return :None;
        );
        if effect^.on_click is :Some f then (
            return :Some effect_id;
        );
        if effect^.drawn is :Some ref drawn then (
            for child in drawn |> ArrayList.iter do (
                match child^ with (
                    | :String _ => ()
                    | :Effect id => (
                        if find_hovered_clickable(id, pos) is :Some result then (
                            return :Some result;
                        );
                    )
                )
            );
        );
        :None
    );

    const mainloop = () => (
        let mut ctx = @current Context;
        loop (
            let input = tty.input();
            match input.shape with (
                | :Mouse { .row, .col, .event = :Press button } => (
                    if find_hovered_clickable(
                        ctx.root, { .row, .col }
                    ) is :Some id then (
                        let effect = &mut ctx.effects
                            |> OrdMap.get_mut(id)
                            |> Option.unwrap;
                        let :Some f = effect^.on_click;
                        f();
                    );
                )
                | :Mouse { .row, .col, .event = :Move } => (
                    let new_hovered = find_hovered_clickable(ctx.root, { .row, .col });
                    let changed = match { ctx.hovered_clickable, new_hovered } with (
                        | { :Some a, :Some b } => a != b
                        | { :None, :None } => false
                        | { :Some _, :None } => true
                        | { :None, :Some _ } => true
                    );
                    if changed then (
                        ctx.hovered_clickable = new_hovered;
                        redraw_all();
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

const foldable = (contents :: () -> ()) => (
    let folded = signal(false);
    button(
        () => if folded.get() then "[+]" else "[-]",
        () => folded.set(not folded.get()),
    );
    create_and_draw_effect(
        () => (
            if not folded.get() then (
                write(" ");
            );
        )
    );
    let contents = create_effect(contents);
    create_and_draw_effect(
        () => (
            if not folded.get() then (
                add_drawn(:Effect contents)
            );
        )
    );
);

with IdGenCtx = IdGen.new();

Tui.run(
    () => with_return (
        write("Hello\n");
        counter();
        write("\n");
        foldable(
            () => (
                write("Some text (");
                foldable(
                    () => write("inner foldable")
                );
                write(")");
            )
        );
        write("\n");
    )
);
