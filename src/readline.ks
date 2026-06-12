use (import "./tty.ks").*;
use (import "./history.ks").*;
use std.collections.OrdMap;

module:

const Readline = (
    module:

    const DEBUG = false;
    ## indices are in string encoding
    ## end is exclusive
    const Range = newtype {
        .start :: Int32,
        .end :: Int32,
    };

    const Context = @context newtype {
        .history :: History,
        .highlight :: String -> String,
        .tokenize :: String -> ArrayList.t[Range],
        .prompt :: String,
    };

    const read_line_impl = () -> String => (
        let mut ctx = @current Context;
        if tty.read_cursor_position().1 != 1 then (
            tty.write("\x1b[7m%\x1b[27m\n");
        );
        tty.write(ctx.prompt);
        tty.flush();
        let mut begin_pos = tty.read_cursor_position();
        let mut cursor_pos = begin_pos;
        let mut result = {
            .before_cursor = "",
            .after_cursor = "",
        };
        let mut selection_start = 0;
        let mut display_widths = OrdMap.new();
        let calculate_display_widths = s => (
            for c in s |> String.iter do (
                if &display_widths |> OrdMap.get(c) is :None then (
                    # we clear them afterwards anyway
                    let start = tty.read_cursor_position();
                    tty.write(to_string(c));
                    let end = tty.read_cursor_position();
                    &mut display_widths |> OrdMap.add(c, end.1 - start.1);
                    tty.reset_cursor_position();
                );
            );
        );
        let display_width = c => (
            (&display_widths |> OrdMap.get(c) |> Option.unwrap)^
        );
        let recalculate_pos = () => (
            # TODO copy
            cursor_pos = { begin_pos.0, begin_pos.1 };
            for c in result.before_cursor |> String.iter do (
                cursor_pos.1 += display_width(c);
            );
        );
        let reset_selection = () => (
            selection_start = String.length(result.before_cursor);
        );
        let delete_selection = () -> Bool => with_return (
            let current_pos = String.length(result.before_cursor);
            if current_pos == selection_start then (
                return false;
            );
            let s = result.before_cursor + result.after_cursor;
            let { start, end } = if current_pos > selection_start then (
                { selection_start, current_pos }
            ) else (
                { current_pos, selection_start }
            );
            result = {
                .before_cursor = s |> String.substring(0, start),
                .after_cursor = s |> String.substring_from(end),
            };
            true
        );
        let move_history = f => (
            let s = &mut ctx.history |> f;
            result = {
                .before_cursor = s,
                .after_cursor = "",
            };
            reset_selection();
            calculate_display_widths(s);
        );
        let redraw_and_place_cursor_on_new_line = () => (
            recalculate_pos();
            tty.move_cursor_to(...begin_pos);
            tty.clear_after_cursor();
            tty.write(ctx.highlight(result.before_cursor + result.after_cursor));
            tty.write("\n");
        );
        loop (
            let input = tty.input();
            match input.shape with (
                | :Enter => (
                    break;
                )
                | :Backspace => (
                    if not delete_selection() and result.before_cursor != "" then (
                        &mut result.before_cursor |> String.pop_back;
                    );
                    reset_selection();
                )
                | :Delete => (
                    if not delete_selection() and result.after_cursor != "" then (
                        let c = result.after_cursor |> String.at(0);
                        let i = Char.string_encoding_len(c);
                        result.after_cursor = result.after_cursor
                            |> String.substring(i, String.length(result.after_cursor) - i);
                    );
                    reset_selection();
                )
                | :ClearScreen => (
                    tty.clear_screen();
                    tty.move_cursor_to(1, 1);
                    tty.write(ctx.prompt);
                    begin_pos = tty.read_cursor_position();
                )
                | :Content content => (
                    result.before_cursor += content;
                    reset_selection();
                    calculate_display_widths(content);
                )
                | :ArrowUp => (
                    move_history(History.select_prev);
                )
                | :ArrowDown => (
                    move_history(History.select_next);
                )
                | :ArrowLeft => (
                    if result.before_cursor != "" then (
                        if (
                            input.modifiers |> tty.Modifiers.has_alt
                            or input.modifiers |> tty.Modifiers.has_ctrl
                        ) then (
                            let tokens = ctx.tokenize(result.before_cursor + result.after_cursor);
                            let mut target_pos = 0;
                            for token in tokens |> ArrayList.into_iter do (
                                if token.start < result.before_cursor |> String.length then (
                                    target_pos = token.start;
                                ) else (
                                    break;
                                );
                            );
                            let skipped = result.before_cursor
                                |> String.substring_from(target_pos);
                            result.after_cursor = skipped + result.after_cursor;
                            result.before_cursor = result.before_cursor
                                |> String.substring(0, target_pos);
                        ) else (
                            let c = &mut result.before_cursor |> String.pop_back;
                            result.after_cursor = to_string(c) + result.after_cursor;
                        );
                        if not input.modifiers |> tty.Modifiers.has_shift then (
                            reset_selection();
                        );
                    );
                )
                | :ArrowRight => (
                    if result.after_cursor != "" then (
                        if (
                            input.modifiers |> tty.Modifiers.has_alt
                            or input.modifiers |> tty.Modifiers.has_ctrl
                        ) then (
                            let tokens = ctx.tokenize(result.before_cursor + result.after_cursor);
                            let current_pos = result.before_cursor |> String.length;
                            let mut target_delta = String.length(result.after_cursor);
                            for token in tokens |> ArrayList.into_iter do (
                                if token.start > current_pos then (
                                    target_delta = token.start - current_pos;
                                    break;
                                );
                            );
                            let skipped = result.after_cursor
                                |> String.substring(0, target_delta);
                            result.before_cursor = result.before_cursor + skipped;
                            result.after_cursor = result.after_cursor
                                |> String.substring_from(target_delta);
                        ) else (
                            let c = result.after_cursor |> String.at(0);
                            let i = Char.string_encoding_len(c);
                            result.after_cursor = result.after_cursor
                                |> String.substring(i, String.length(result.after_cursor) - i);
                            result.before_cursor += to_string(c);
                        );
                        if not input.modifiers |> tty.Modifiers.has_shift then (
                            reset_selection();
                        );
                    );
                )
                | :Home => (
                    result = {
                        .before_cursor = "",
                        .after_cursor = result.before_cursor + result.after_cursor,
                    };
                    cursor_pos = begin_pos;
                    if not input.modifiers |> tty.Modifiers.has_shift then (
                        reset_selection();
                    );
                )
                | :End => (
                    result = {
                        .before_cursor = result.before_cursor + result.after_cursor,
                        .after_cursor = ""
                    };
                    if not input.modifiers |> tty.Modifiers.has_shift then (
                        reset_selection();
                    );
                )
                | _ => ()
            );
            redraw_and_place_cursor_on_new_line();
            (
                # highlight selection
                tty.save_cursor_position();
                let mut screen_pos = { begin_pos.0, begin_pos.1 };
                let current_pos = String.length(result.before_cursor);
                let s = result.before_cursor + result.after_cursor;
                let { start, end } = if current_pos > selection_start then (
                    { selection_start, current_pos }
                ) else (
                    { current_pos, selection_start }
                );
                for c in s |> String.substring(0, start) |> String.iter do (
                    screen_pos.1 += display_width(c);
                );
                tty.move_cursor_to(...screen_pos);
                tty.invert_colors();
                tty.write(s |> String.substring(start, end - start));
                tty.invert_colors();
                tty.reset_cursor_position();
            );
            if DEBUG then (
                # dbg.print writes directly to stderr so need to flush first
                tty.flush();
                dbg.print((@current tty.Context).last_read);
                dbg.print(input);
            );
            tty.move_cursor_to(...cursor_pos);
            tty.flush();
        );
        redraw_and_place_cursor_on_new_line();
        tty.flush();
        let result = result.before_cursor + result.after_cursor;
        &mut ctx.history |> History.push(result);
        result
    );

    const read_line = (
        .history :: History,
        .prompt :: String,
        .tokenize :: String -> ArrayList.t[Range],
        .highlight :: String -> String,
        .handle_ctrl_c :: () -> (),
    ) -> String => (
        tty.run(
            () => (
                tty.set_cursor_type(:BlinkingBar);
                with Context = {
                    .history,
                    .tokenize,
                    .highlight,
                    .prompt,
                };
                read_line_impl()
            ),
            .handle_ctrl_c,
        )
    );
);
# Repl.run(.highlight = s => s, .prompt = "> ");

