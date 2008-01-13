x = new HashTable from {a=>1, b=>2}
applyValues(x, value -> 1000*value)
applyKeys(x, key -> {key})
applyPairs(x, (key,value) -> (value,key))
x = new HashTable from {a=>1, b=>2}
scanValues(x, print)
scanKeys(x, print)
scanPairs(x, print)
y = new HashTable from {b=>200, c=>300}
merge(x, y, plus)
combine(x,y,identity,times,plus)
