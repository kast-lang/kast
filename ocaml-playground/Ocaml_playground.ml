type ('a, 'b) pair = {
  first : 'a;
  second : 'b;
}

type ('a, 'b, 'c) triple = ('a, ('b, 'c) pair) pair

let f : (int, int, int) triple -> _ = fun x -> x
