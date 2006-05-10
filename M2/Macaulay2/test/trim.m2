-- the question is whether 'trim' should do something for inhomogeneous modules.
-- if it doesn't do enough, this test will break

R = ZZ/7[y, x, MonomialOrder=>Lex];
I = ideal (y^3*x^2 + 2*y^2*x + 3*x*y,  3*y^2 + x*y - 3*y);
J = saturate(I, ideal(y))
assert ( set flatten entries gens J === set {y-2*x-1, x^4+x^3+3*x^2+3*x} )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test trim.out"
-- End:
