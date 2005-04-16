f = method(Associative => true)
f(String,String) := (i,j) -> "(" | i | ")," | j;
f("a","b","c","d")
