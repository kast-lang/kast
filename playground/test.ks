use std.rc.Rc;

println!("0");
unwindable top (
    println!("1");
    let x :: Rc.t[Int32] = unwindable foo (
        println!("2");
        unwind top ();
        println!("3");
        Rc.new(123)
    );
    println!("4");
);
println!("5");
