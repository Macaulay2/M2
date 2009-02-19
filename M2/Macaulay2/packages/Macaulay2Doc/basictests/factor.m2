assert = x -> if not x then error "assertion failed"

v = Pari$factorint (12 * 2^32 + 3)
if v === ((2, 3, 5, 13, 41, 61, 1321), (2, 1, 2, 1, 1, 1, 1)) then error "pari/gmp integer conversion: 32-bit reversal"
assert( v == ((3, 5, 137, 953, 26317), (1, 1, 1, 1, 1)) )

v = Pari$factorint (1 + 4 * 2^64)
if v == ((2, 5, 5581, 8681, 49477, 384773), (2, 1, 1, 1, 1, 1)) then error "pari/gmp integer conversion: 64-bit reversal"
assert( v == ((5, 13, 397, 2113, 312709, 4327489), (1, 1, 1, 1, 1, 1)) )
