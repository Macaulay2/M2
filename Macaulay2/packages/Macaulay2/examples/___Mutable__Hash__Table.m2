x = new MutableHashTable
scan(0 .. 30, i -> x#i = i^2)
x # 20
x #? 40
