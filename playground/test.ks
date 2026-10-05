let s = unwindable block (
    unwind block String.from_str("hi");
    panic("unreachable")
);

println!("\(s)");
