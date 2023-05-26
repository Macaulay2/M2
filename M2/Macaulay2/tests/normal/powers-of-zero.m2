-- git issue 1856: powers of 0 were inconsistent.
-- e.g. 0^0 is sometimes 1, sometimes 0 (inconsistent choice of convention)
--      0^n is sometimes non-zero, for n > 0 (bug)
-- These are fixed in April 2022, these checks make sure it is fixed correctly.
checkPowersOfZero = (n, F) -> (
    vals := (0..n) / (i -> 0_F^i);
    vals == prepend(1_F, n:0_F)
    )
previousPrime = (n) -> if n < 2 then null else (while not isPrime n do n = n-1; n)

assert checkPowersOfZero(30, ZZ/5)
assert checkPowersOfZero(30, ZZ/32003)
assert checkPowersOfZero(30, ZZ/1048583) -- nextPrime 2^20
if version#"pointer size" > 4 then (
    assert checkPowersOfZero(30, ZZ/(nextPrime 2^40)); -- nextPrime 2^40
    assert checkPowersOfZero(30, ZZ/(nextPrime 2^60)); -- nextPrime 2^60
    assert checkPowersOfZero(30, ZZ/(nextPrime 2^63))) -- nextPrime 2^63
assert checkPowersOfZero(30, GF 5)

assert checkPowersOfZero(30, GF 5)
assert checkPowersOfZero(30, GF 25)
assert checkPowersOfZero(30, GF(3^10))
assert checkPowersOfZero(30, GF(3^50))

assert checkPowersOfZero(30, GF(5, Strategy => "Flint"))
assert checkPowersOfZero(30, GF(5, Strategy => "FlintBig"))
assert checkPowersOfZero(30, GF(25, Strategy => "Flint"))
assert checkPowersOfZero(30, GF(25, Strategy => "FlintBig"))
assert checkPowersOfZero(30, GF(3^10, Strategy => "Flint"))
assert checkPowersOfZero(30, GF(3^50, Strategy => "FlintBig"))

assert checkPowersOfZero(30, GF(2, Strategy => "Flint"))
assert checkPowersOfZero(30, GF(2, Strategy => "FlintBig"))
assert checkPowersOfZero(30, GF(2^2, Strategy => "Flint"))
assert checkPowersOfZero(30, GF(2^2, Strategy => "FlintBig"))
assert checkPowersOfZero(30, GF(2^10, Strategy => "Flint"))
assert checkPowersOfZero(30, GF(2^80, Strategy => "FlintBig"))

-- Now we check the non-exported ZZ/p, GF types.  Note: Givaro GF are not considered here, as they
--  have been removed on a branch that will soon get pulled into the development branch (April 2022).
-- (ZZ/p: Aring, Old, Ffpack).  Status: Aring, Old are likely to be removed.  Ffpack might also be changed?
-- GF: Old, New (both are old though!) and require not large. Status: both of these to be removed.
debug Core
assert checkPowersOfZero(30, ZZp(5, Strategy => "Ffpack"))
assert checkPowersOfZero(30, ZZp(5, Strategy => "Aring"))
assert checkPowersOfZero(30, ZZp(5, Strategy => "Old"))

assert checkPowersOfZero(30, ZZp((previousPrime 2^15), Strategy => "Ffpack"))
assert checkPowersOfZero(30, ZZp((previousPrime 2^15), Strategy => "Aring"))
assert checkPowersOfZero(30, ZZp((previousPrime 2^15), Strategy => "Old"))

assert checkPowersOfZero(30, GF(5, Strategy => "Old")) -- ZZp.cpp ("Old")
assert checkPowersOfZero(30, GF(25, Strategy => "Old"))
assert checkPowersOfZero(30, GF(5^5, Strategy => "Old")) -- GF.cpp ("Old")
assert checkPowersOfZero(30, GF(5^10, Strategy => "Old")) -- FlintBig
assert checkPowersOfZero(30, GF(5^12, Strategy => "Old")) -- FlintBig
assert checkPowersOfZero(30, GF(5^15, Strategy => "Old")) -- FlintBig

assert checkPowersOfZero(30, GF(2, Strategy => "Old")) -- ZZp.cpp ("Old")
assert checkPowersOfZero(30, GF(8, Strategy => "Old"))
assert checkPowersOfZero(30, GF(2^5, Strategy => "Old")) -- GF.cpp ("Old")
assert checkPowersOfZero(30, GF(2^10, Strategy => "Old")) -- FlintBig
assert checkPowersOfZero(30, GF(2^12, Strategy => "Old")) -- FlintBig
assert checkPowersOfZero(30, GF(2^15, Strategy => "Old")) -- FlintBig

assert checkPowersOfZero(30, GF(5, Strategy => "New")) -- ZZp.cpp ("New")
assert checkPowersOfZero(30, GF(25, Strategy => "New"))
assert checkPowersOfZero(30, GF(5^5, Strategy => "New")) -- GF.cpp ("New")
assert checkPowersOfZero(30, GF(5^10, Strategy => "New")) -- FlintBig
assert checkPowersOfZero(30, GF(5^12, Strategy => "New")) -- FlintBig
assert checkPowersOfZero(30, GF(5^15, Strategy => "New")) -- FlintBig

assert checkPowersOfZero(30, GF(2, Strategy => "New"))
assert checkPowersOfZero(30, GF(8, Strategy => "New"))
assert checkPowersOfZero(30, GF(2^5, Strategy => "New"))
assert checkPowersOfZero(30, GF(2^10, Strategy => "New"))

assert checkPowersOfZero(30, GF(2^12, Strategy => "New")) -- FlintBig
elapsedTime assert checkPowersOfZero(30, GF(2^15, Strategy => "New", SizeLimit => 2^15)) -- takes awhile

assert checkPowersOfZero(30, ZZ)
assert checkPowersOfZero(30, QQ)
assert checkPowersOfZero(30, RR_53)
assert checkPowersOfZero(30, RR_100)
assert checkPowersOfZero(30, RR_1000)
assert checkPowersOfZero(30, CC_53)
assert checkPowersOfZero(30, CC_100)
assert checkPowersOfZero(30, CC_1000)

assert checkPowersOfZero(30, ZZ/101[a..d])
assert checkPowersOfZero(30, ZZ/101[a..d]/(a^2-b^3, b^2-c^3))
assert checkPowersOfZero(30, frac(ZZ/101[a..d]))
assert checkPowersOfZero(30, frac(ZZ[a..d]))
assert checkPowersOfZero(30, frac(QQ[a..d]))
assert checkPowersOfZero(30, frac(QQ[a..d]/(a^2-b^3)))

assert checkPowersOfZero(30, frac(QQ[a]))
assert checkPowersOfZero(30, frac(QQ[a,b,c]/(a^2+b^3+c^4)))

