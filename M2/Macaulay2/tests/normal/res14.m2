-- Testing fast nonminimal resolutions
elapsedTime for kk in {-* TODO: QQ, *- ZZ/2, ZZ/32003 -* TODO: ZZ/32831 *-} do (
    R = kk[x,y,z];
    I = ideal(x^2-y*z, x^4);
    C = res I;
    -- FIXME: assert(betti C == betti res(ideal I_*, Strategy => FastNonminimal));
    assert(betti C == betti res(ideal I_*, Strategy => Nonminimal));
	assert(betti C == betti res(ideal gens gb ideal I_*, Strategy => NonminimalWithGB));
	assert(betti C == betti res(ideal I_*, Strategy => Nonminimal))
	);

elapsedTime for kk in {QQ, ZZ/32831} do (
    kk = QQ;
    R = kk[x,y,z];
    I = ideal(x^2-y*z, x^4);
    C = res I;
    assert try ( res(ideal I_*, Strategy => Nonminimal); false ) else true;
    assert try ( res(ideal I_*, Strategy => NonminimalWithGB); false ) else true;
	);

-- Here is a simple test for skew commuting vars
gbTrace=4
R = ZZ/101[a..d, SkewCommutative=>true]
I = ideal(a-d)
C = res(I, Strategy => Nonminimal, LengthLimit => 2)
C0 = res(ideal I_*, LengthLimit => 2)
assert(C.dd_2 _(0,0) == a-d) -- failed with first change from res-gausser to VectorArithmetic.
assert(C0.dd_2 _(0,0) == a-d) -- ok
