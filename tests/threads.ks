use std.rc.Rc;
use std.sync.Mutex;

let mutex = Mutex.new(());
@defer Mutex.drop(mutex);

const println = args => `(
    (
        let guard = Mutex.lock(&mutex);
        @defer Mutex.unlock(guard);
        std.io.println!($args);
    )
);

let mut threads = ArrayList.new();

for (i :: Int32) in 0..10 do (
    let thread = std.thread.spawn(@move () => (
        println!("Started thread #\(i)");
        for (j :: Int32) in 0..10 do (
            let ms = std.random.gen_range(.min = 100, .max = 500);
            # std.thread.sleep_ns(ms * 1000000);
            println!("Hello #\(j) from thread #\(i)");
        );
    ));
    &mut threads |> ArrayList.push_back(thread);
);

println!("All threads started");
for thread in threads |> ArrayList.into_iter do (
    std.thread.join(thread);
);
println!("The End");
