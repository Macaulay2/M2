Free = new Type of HashTable
p = new Free from { "x" => 2, "y" => 3, "cat" => 5 }
q = new Free from { "x" => 100, "y" => 200, "dog" => 7 }
Free + Free := (p,q) -> merge(p,q,plus);
p+q
