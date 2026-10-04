let main = () with io => (
    println!("Welcome to the Guessing Number Game :-)");
    let picked :: Int32 = std.random.gen_range(.min = 1, .max = 10);
    println!("The number has been picked!");
    # dbg.print (.picked);
    let mut first = true;
    loop (
        let prompt = if first then "Guess: " else "Guess again: ";
        first = false;
        let guess = &input(prompt) |> String.as_str |> String.parse;
        if picked < guess then (
            println!("Less!")
        ) else if picked > guess then (
            println!("Greater!")
        ) else (
            println!("You guessed!");
            break;
        );
    );
);
main();
