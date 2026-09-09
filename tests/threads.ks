let mutex = std.sync.Mutex.new(());

let print = (s :: String) => (
    let mutex = std.sync.Mutex.lock(&mutex);
    std.io.print(s);
    std.sync.Mutex.unlock(mutex);
);

for (i :: Int32) in 0..10 do (
    std.thread.spawn(() => with_return (
        print("Started thread #" + to_string(i));
        for (j :: Int32) in 0..10 do (
            let ms = std.random.gen_range(.min = 100, .max = 500);
            std.thread.sleep_ns(ms * 1000000);
            print("Hello #" + to_string(j) + " from thread #" + to_string(i));
        );
    ));
);

print("All threads started");
std.thread.sleep_ns(5000000000);
print("The End");
