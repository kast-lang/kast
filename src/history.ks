use (import "./stdext/_lib.ks").*;

module:

const History = newtype {
    .entries :: ArrayList.t[String],
    .selected_index :: Int32,
    .save_push :: String -> (),
};

impl History as module = (
    module:

    const new_file = (path :: String) -> History => (
        let mut entries = ArrayList.new();
        if stdext.fs.exists(path) then (
            let contents = std.fs.read_file(path);
            for line in contents |> String.lines do (
                &mut entries |> ArrayList.push_back(line);
            );
        );
        let f :: @opaque_type = @native "fs.openSync(\(path), 'a')";
        {
            .selected_index = &entries |> ArrayList.length,
            .entries,
            .save_push = line => (
                @native "fs.writeSync(\(f), \(line))";
                @native "fs.writeSync(\(f), '\\n')";
            ),
        }
    );

    const push = (self :: &mut History, entry :: String) => (
        &mut self^.entries |> ArrayList.push_back(entry);
        self^.selected_index = &self^.entries |> ArrayList.length;
        self^.save_push(entry);
    );

    const currently_selected = (self :: &History) -> String => (
        if self^.selected_index < &self^.entries |> ArrayList.length then (
            (&self^.entries |> ArrayList.at(self^.selected_index))^
        ) else (
            ""
        )
    );

    const select_prev = (self :: &mut History) -> String => (
        if self^.selected_index > 0 then (
            self^.selected_index -= 1;
        );
        currently_selected(&self^)
    );
    const select_next = (self :: &mut History) -> String => (
        if self^.selected_index + 1 <= &self^.entries |> ArrayList.length then (
            self^.selected_index += 1;
        );
        currently_selected(&self^)
    );
);
