x = new HashTable from {1 => a, 2 => b, 3 => c}
y = applyPairs(x, (k,v) -> (v,k))
x#2
y#b
