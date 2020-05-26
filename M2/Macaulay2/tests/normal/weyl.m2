-- Tests of standard ideal/matrix operations but in the Weyl algebra, to check 
-- consistency

-- no test yet
assert(true)

end

restart
needsPackage "Dmodules"
W = makeWA(QQ[x,y])
W * Matrix := (f,M) -> error "cannot multiply matrices on left"
W * Ideal := (f,M) -> error "cannot multiply ideal on left"
prevMult = lookup(symbol*, W, W)
W * W := (f,g) -> prevMult(g,f)
x * dx
describe W
I = ideal(x*dx)
f = dx
f * I -- want to disallow this for Weyl algebras
  -- it is: if I = (g1, ..., gr), is f*I = (f*g1, ..., f*gr)
  -- but: this has no good meaning
-- instead, want:
W = QQ[a..d]
a * ideal(b,c)

I * f
f * (gens I)
matrix{{f}} * (gens I) -- this is what we want gens(I*f) to be.
(gens I) * f
f * (gens I)
gens (f*I)


(gens I) * matrix{{f}}  -- these are OK
matrix{{f}} * (gens I)

(gens I) ** matrix{{f}}  -- want to disallow tensor product in this case, as it doesn't really mean what
                         -- we usually want it to mean.
matrix{{f}} ** (gens I)
