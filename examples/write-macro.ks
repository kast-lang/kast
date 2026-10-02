# Usage of the macro
let mut output :: String = String.from_str("");
std.fmt.write!(&mut output, "Hello, {}! Here's a random number: {}", "World", 67 :: Int32);
print(&output |> String.as_str);

let name = "you";
print(&format!("Hi, {}", name) |> String.as_str);
