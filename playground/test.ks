let { mut a, mut b } = { 1, 2 };
{ a, ..., b } = { 6, 7, 8, 9 };

print(to_string(a) + to_string(b));
