v = vector {1, 2}
w = vector {3, 4}
assert(v⊗w == vector {3, 4, 6, 8})

Vector·Vector := (v, w) -> ((transpose v#0) * w#0)_(0, 0)
assert(v·w == 11)

-- should get "invalid symbol" error
assert try getSymbol "⟎⟎" then false else true

assert((a⇒b) === (a => b))

-- used to raise recursion error (M2/bugs/dan/1-recursion-in-printing)
assert(vector vector {1} == vector(ZZ, 1))
