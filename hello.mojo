@fieldwise_init
struct Person(Copyable, Movable):
    var age: UInt16
    var name: String

fn main() raises:
    var person = Person(10000, "kuviman")
    var description: String = Optional(
        String("age: {}, name: {}").format(
            person.age, person.name)
    ).or_else("parse error")
    print(description)
