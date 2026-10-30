use (import "src/output.ks").*;
use (import "src/log.ks").*;

with Stdout = new_std_output(
    std.io.stdout.write,
    .color = true,
);
@defer Output_drop(@current Stdout);
with Stderr = new_std_output(
    std.io.stdout.write,
    .color = true,
);
@defer Output_drop(@current Stderr);
with Output = (@current Stdout);

let name = "chat";

let ctx = @context;

let mut threads = ArrayList.new();
for i in 0..100 do (
    let tid = std.thread.spawn(@move () => (
        let mut ctx = ctx;
        let &@context = &mut ctx;
        for i in 0..100 do (
            Log.info!("Hello, \(name)");
        );
    ));
    &mut threads |> ArrayList.push_back(tid);
);

for thread in threads |> ArrayList.into_iter do (
    std.thread.join(thread);
);
