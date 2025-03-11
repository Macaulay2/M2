X = new SelfInitializingType of HashTable

-- NewFromMethod
new X from  ZZ        := (T,x)     -> T {a => x, b => 0, c => 0}
new X from (ZZ,ZZ)    := (T,x,y)   -> T {a => x, b => y, c => 0}
new X from (ZZ,ZZ,ZZ) := (T,x,y,z) -> T {a => x, b => y, c => z}

x = X {a => 1, b => 2, c => 3}
assert(x.a == 1 and x.b == 2 and x.c == 3)
x = X 1
assert(x.a == 1 and x.b == 0 and x.c == 0)
x = X(1, 2)
assert(x.a == 1 and x.b == 2 and x.c == 0)
x = X(1, 2, 3)
assert(x.a == 1 and x.b == 2 and x.c == 3)

-- TODO: add tests for NewMethod, NewOfMethod, and NewOfFromMethod
