f = x -> {class x, if class x === Sequence then #x};
f()
f(3)
f(3,4)
f(3,4,5)
f = x -> (
     x = sequence x;
     {class x, #x});
f()
f(3)
f(3,4)
f(3,4,5)
((x) -> x) 3
1 : 3
((x) -> x) oo
