use (import "./stdext/_lib.ks").*;
use (import "./rc_str.ks").*;

module:

const History = newtype {
    .entries :: ArrayList.t[RcStr],
    .selected_index :: Int32,
    .save_push :: &str -> (),
};

impl History as module = (
    module:

    const new_file = (path :: &str) -> History => (
        let mut entries = ArrayList.new();
        if stdext.fs.exists(path) then (
            let contents = std.fs.read_file(path) |> RcStr.from_String;
            for line in &contents |> RcStr.lines do (
                if &line |> RcStr.as_str != "" then (
                    &mut entries |> ArrayList.push_back(line);
                );
            );
        );
        let mut f = stdext.fs.File.create_with_mode(path, { .append = true });
        {
            .selected_index = &entries |> ArrayList.length,
            .entries,
            .save_push = @move line => (
                &mut f |> stdext.fs.File.write(line);
                &mut f |> stdext.fs.File.write("\n");
                &mut f |> stdext.fs.File.flush();
            ),
        }
    );

    const push = (self :: &mut History, entry :: &str) => (
        let entries_len = &self^.entries |> ArrayList.length;
        if (
            entry != ""
            and (
                entries_len == 0
                or entry != &self^.entries |> ArrayList.at(entries_len - 1) |> RcStr.as_str
            )
        ) then (
            &mut self^.entries |> ArrayList.push_back(RcStr.allocate(entry));
            self^.save_push(entry);
        );
        self^.selected_index = &self^.entries |> ArrayList.length;
    );

    const currently_selected = (self :: &History) -> &str => (
        if self^.selected_index < &self^.entries |> ArrayList.length then (
            &self^.entries |> ArrayList.at(self^.selected_index) |> RcStr.as_str
        ) else (
            ""
        )
    );

    const select_prev = (self :: &mut History) -> &str => (
        if self^.selected_index > 0 then (
            self^.selected_index -= 1;
        );
        currently_selected(&self^)
    );
    const select_next = (self :: &mut History) -> &str => (
        if self^.selected_index + 1 <= &self^.entries |> ArrayList.length then (
            self^.selected_index += 1;
        );
        currently_selected(&self^)
    );
);
