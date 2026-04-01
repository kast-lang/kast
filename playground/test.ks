const f :: (Int32, .named :: String) -> () = (...args) => dbg.print(args);
f(123, .named = "hello");
