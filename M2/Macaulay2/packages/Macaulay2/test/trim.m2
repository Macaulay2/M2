-- the question is whether 'trim' should do something for inhomogeneous modules.
-- if it doesn't do enough, this test will break
gbTrace = 3
R = ZZ/7[y, x, MonomialOrder=>Lex];
I = ideal (y^3*x^2 + 2*y^2*x + 3*x*y,  3*y^2 + x*y - 3*y);
J' = saturate(I, ideal(y), MinimalGenerators => false)	    -- tell saturate not to trim
J = saturate(I, ideal(y), MinimalGenerators => true)	    -- tell saturate to trim
assert ( set flatten entries gens J === set {y-2*x-1, x^4+x^3+3*x^2+3*x} )

--

R = ZZ/32003[a..j]
I = ideal random(R^1, R^{-2,-2,-2,-2,-2,-2,-2});
trim I  -- fixed: this fails because it takes a long time...  It should stop after mingens are known to be computed:

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test trim.out"
-- End:
