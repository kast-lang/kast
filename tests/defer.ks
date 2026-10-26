println!("outer in");
@defer (
    @defer println!("deferred in defer");
    println!("outer out");
);
(
    println!("in");
    @defer println!("out");
    @defer println!("out2");
    println!("end");
);
println!("outer end");
