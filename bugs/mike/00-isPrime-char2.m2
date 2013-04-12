-- From Starx <jstarx@gmail.com>, sent to M2 google group, 10 April 2013.

A = ZZ/2[u, v, w, x, y, z]
I = ideal(x*y + z^2, u*v + w^2, u*y + v*x)

assert not isPrime(I)
assert((v*z + w*y)^2 % I == 0)
assert((v*z + w*y) % I != 0)

end
-- MES playing with this bug.
debug Core
rawIrredCharacteristicSeries
C = rawCharSeries raw gens I
netList C

end

The following returns true:

A = ZZ/2[u, v, w, x, y, z]
I = ideal(x*y + z^2, u*v + w^2, u*y + v*x)
isPrime(I)

but the ideal I here is not prime, indeed

(v*z + w*y)^2 % I == 0

returns true yet

(v*z + w*y) % I == 0

returns false.

-Jim

