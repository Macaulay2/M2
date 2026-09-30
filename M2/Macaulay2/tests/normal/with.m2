-- context management

-- files
f = openOut temporaryFileName()
with f do assert isOpen f
assert not isOpen f

-- mutexes
m = new Mutex
with m do assert try tryLock m then false else true
assert try tryLock m then true else false
unlock m

-- polynomial rings
R = QQ[x]
x = 5
assert Equation(with R do x, R_0)
assert Equation(x, 5)

-- inexact fields
prec = defaultPrecision
assert Equation(with RR_100 do defaultPrecision, 100)
assert Equation(defaultPrecision, prec)

-- custom types
X = new Type of HashTable
X.EnterMethod = x -> y = 1
X.ExitMethod = x -> y = x + 1
y = 0
with new X do assert Equation(y, 1)
assert Equation(y, 2)

-- ensure that exit method still runs even w/ an error
y = 3
try with new X do 1/0
assert Equation(y, 2)
