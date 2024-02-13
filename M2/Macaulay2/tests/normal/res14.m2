-- Testing fast nonminimal resolutions
elapsedTime for kk in {-* TODO: QQ, *- ZZ/2, ZZ/32003 -* TODO: ZZ/32831 *-} do (
    R = kk[x,y,z];
    I = ideal(x^2-y*z, x^4);
    C = res I;
    -- FIXME: assert(betti C == betti res(ideal I_*, Strategy => FastNonminimal));
    assert(betti C == betti res(ideal I_*, FastNonminimal => true));
    assert(betti C == betti res(ideal I_*, FastNonminimal => true, Strategy => Engine));
    for strategy in {4, 4.1, -* FIXME: 5, 5.1 *- } do (
	assert(betti C == betti res(ideal I_*, Strategy => strategy));
	assert(betti C == betti res(ideal I_*, FastNonminimal => true, Strategy => strategy));
	);
    )

elapsedTime for kk in {QQ, ZZ/32831} do (
    R = kk[x,y,z];
    I = ideal(x^2-y*z, x^4);
    C = res I;
    assert try ( res(ideal I_*, Strategy => FastNonminimal); false ) else true;
    assert try ( res(ideal I_*, FastNonminimal => true, Strategy => Engine); false ) else true;
    for strategy in {4, 5 -* experimentally enabled: 4.1, 5.1 *- } do (
	assert try ( res(ideal I_*, Strategy => strategy); false ) else true;
	assert try ( res(ideal I_*, FastNonminimal => true, Strategy => strategy); false ) else true;
	);
    )

-- Here is a simple test for skew commuting vars
gbTrace=4
R = ZZ/101[a..d, SkewCommutative=>true]
I = ideal(a-d)
C = res(I, FastNonminimal => true, LengthLimit => 2)
C0 = res(ideal I_*, LengthLimit => 2)
assert(C.dd_2 _(0,0) == a-d) -- failed with first change from res-gausser to VectorArithmetic.
assert(C0.dd_2 _(0,0) == a-d) -- ok
