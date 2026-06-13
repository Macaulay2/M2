-- test 0
TEST ///
	X = complexProjectiveSpace 1
	assert(X.QTMCharacteristicMatrix == matrix{{1, -1}})
	assert(betti X == {1, 0, 1})
	assert(bettiQTM X == {1, 0, 1}) -- deprecated
///

-- test 1
TEST ///
	X = complexProjectiveSpace 2
	assert(X.QTMCharacteristicMatrix == matrix{{1, 0, -1}, {0, 1, -1}})
	assert(betti X == {1, 0, 1, 0, 1})
	assert(bettiQTM X == {1, 0, 1, 0, 1}) -- deprecated
///

-- test 2
TEST ///
	X = complexProjectiveSpace 3
	assert(X.QTMCharacteristicMatrix == matrix{{1, 0, 0, -1}, {0, 1, 0, -1}, {0, 0, 1, -1}})
	assert(betti X == {1, 0, 1, 0, 1, 0, 1})
	assert(bettiQTM X == {1, 0, 1, 0, 1, 0, 1}) -- deprecated
///

-- test 3
TEST ///
	X = realProjectiveSpace 1
	assert(X.QTMCharacteristicMatrix == sub(matrix{{1, 1}}, ZZ/2))
	assert(betti X == {1, 1})
	assert(bettiSmallCover X == {1, 1}) -- deprecated
///

-- test 4
TEST ///
	X = realProjectiveSpace 2
	assert(X.QTMCharacteristicMatrix == sub(matrix{{1, 0, 1}, {0, 1, 1}}, ZZ/2))
	assert(betti X == {1, 0, 0})
	assert(bettiSmallCover X == {1, 0, 0}) -- deprecated
///

-- test 5
TEST ///
	X = realProjectiveSpace 3
	assert(X.QTMCharacteristicMatrix == sub(matrix{{1, 0, 0, 1}, {0, 1, 0, 1}, {0, 0, 1, 1}}, ZZ/2))
	assert(betti X == {1, 0, 0, 1})
	assert(bettiSmallCover X == {1, 0, 0, 1}) -- deprecated
///

-- test 6
TEST ///
	needsPackage "SimplicialComplexes"
	R = QQ[a..d]
	K = simplicialComplex {a*b, b*c, c*d, d*a}
	lambda = matrix{{1, 0, 1, 0}, {0, 1, 0, 1}}
	X = smallCover(K, lambda) -- 2-torus
	assert(betti X == {1, 2, 1})
	assert(bettiSmallCover X == {1, 2, 1}) -- deprecated
///

-- test 7
TEST ///
	needsPackage "SimplicialComplexes"
	R = QQ[a..d]
	K = simplicialComplex {a*b, b*c, c*d, d*a}
	lambda = matrix{{1, 1, 0, 1}, {0, 1, 1, 1}}
	X = smallCover(K, lambda) -- klein-bottle
	assert(betti X == {1, 1, 0})
	assert(bettiSmallCover X == {1, 1, 0}) -- deprecated
///

-- test 8
TEST ///
	needsPackage "SimplicialComplexes"
	R = QQ[a,b]
	K = simplicialComplex {a, b}
	Z = momentAngleComplex K
	assert(betti Z == {1, 0, 0, 1, 0})
	assert(bettiMAC Z == {1, 0, 0, 1, 0}) -- deprecated
	assert(euler Z == 0)
	assert(eulerMAC Z == 0) -- deprecated
///

-- test 9
TEST ///
	needsPackage "SimplicialComplexes"
	R = QQ[a,b]
	K = simplicialComplex {a*b}
	Z = momentAngleComplex K
	assert(betti Z == {1, 0, 0, 0, 0})
	assert(bettiMAC Z == {1, 0, 0, 0, 0}) -- deprecated
	assert(isFreeModule equivariantCohomology Z)
	assert(euler Z == 1)
	assert(eulerMAC Z == 1) -- deprecated
///

-- test 10
TEST ///
	needsPackage "NormalToricVarieties"
	rayList = {{1, 0}, {0, 1}, {-1, -1}}
	coneList = {{0, 1}, {2}}
	X = normalToricVariety (rayList, coneList) -- CP^2 with two points removed
	assert(betti X == {1, 0, 1, 1, 0})
///

-- test 11
TEST ///
	X = AFPVariety (2, 1)
	assert(betti X == {1, 0, 2, 1, 0})
///

-- test 12 (tests for AFPVariety, takes a long time, 10+ seconds)
TEST ///
	-- functions to compute the syzygy order
	-- written by Matthias Franz, 21/05/2014

	-- compute the Auslander-Bridger dual of a module
	auslanderBridgerDual = M -> coker transpose presentation M

	-- compute the order of syzygy of a module (over a polynomial ring)
	syzygyOrderModule = M -> (
		R := ring M;
		N := auslanderBridgerDual M;
		s := 0;
		for i from 1 to dim R
		when Ext^i(N,R) == 0
		do s = i;
		s
	)

	X = AFPVariety (2, 1)
	assert(betti X == {1, 0, 2, 1, 0})
	assert(syzygyOrderModule equivariantCohomology X == 1)
	assert(sum betti X != length maxCones fan X) -- not equivariantly formal

	X = AFPVariety (3, 2)
	assert(syzygyOrderModule equivariantCohomology X == 2)
	assert(sum betti X != length maxCones fan X) -- not equivariantly formal

	X = AFPVariety (5, 3)
	assert(syzygyOrderModule equivariantCohomology X == 3)
	assert(sum betti X != length maxCones fan X) -- not equivariantly formal

	X = AFPVariety (5, 4)
	assert(syzygyOrderModule equivariantCohomology X == 4)
	assert(sum betti X != length maxCones fan X) -- not equivariantly formal

	X = AFPVariety (7, 4)
	assert(syzygyOrderModule equivariantCohomology X == 4) -- takes a long time
	assert(sum betti X != length maxCones fan X) -- not equivariantly formal, takes a long time
///

-- test 13
TEST ///
	needsPackage "SimplicialComplexes"
	R = ZZ[a..d]
	K = simplicialComplex {a*b, b*c, c*d, d*a}
	lambda = matrix {{1, 0, -1, 0}, {0, 1, 0, -1}}
	X = quasiToricManifold(K, lambda) -- CP1 x CP1
	assert(bettiQTM X == {1, 0, 2, 0, 1})
	I = monomialIdeal X.QTMSimplicialComplex
	J = ideal entries (X.QTMCharacteristicMatrix * vector(gens ring I))
	assert(betti(I+J) == betti ideal cohomologyRing X)
///

-- test 14
TEST ///
	needsPackage "SimplicialComplexes"
	X = hessenbergVariety 2
	assert(length((faces X.QTMSimplicialComplex)#1) == 6)
///

-- test 15
TEST ///
	assert(isValidChar ((realProjectiveSpace 2).QTMSimplicialComplex,
		(realProjectiveSpace 2).QTMCharacteristicMatrix))
	assert(isValidChar ((realProjectiveSpace 3).QTMSimplicialComplex,
		(realProjectiveSpace 3).QTMCharacteristicMatrix))
	assert(isValidChar ((realProjectiveSpace 4).QTMSimplicialComplex,
		(realProjectiveSpace 4).QTMCharacteristicMatrix))

	assert(isValidChar ((complexProjectiveSpace 2).QTMSimplicialComplex,
		(complexProjectiveSpace 2).QTMCharacteristicMatrix))
	assert(isValidChar ((complexProjectiveSpace 3).QTMSimplicialComplex,
		(complexProjectiveSpace 3).QTMCharacteristicMatrix))
	assert(isValidChar ((complexProjectiveSpace 4).QTMSimplicialComplex,
		(complexProjectiveSpace 4).QTMCharacteristicMatrix))

	assert(isValidChar ((hessenbergVariety 1).QTMSimplicialComplex,
		(hessenbergVariety 1).QTMCharacteristicMatrix))
	assert(isValidChar ((hessenbergVariety 2).QTMSimplicialComplex,
		(hessenbergVariety 2).QTMCharacteristicMatrix))
	assert(isValidChar ((hessenbergVariety 3).QTMSimplicialComplex,
		(hessenbergVariety 3).QTMCharacteristicMatrix))
	assert(isValidChar ((hessenbergVariety 4).QTMSimplicialComplex,
		(hessenbergVariety 4).QTMCharacteristicMatrix))

	needsPackage "SimplicialComplexes"
	R = ZZ[a..c]
	assert(isValidChar (simplicialComplex {a*b, b*c, c*a},
		matrix {{1, 1, 1}, {0, 0, 1}}) == false)
///

-- test 16
TEST ///
	X = complexProjectiveSpace 3
	d = ((entries vars ambient cohomologyRing X)_0)_3
	assert(chern X == {1, 4*d, 6*d^2, 4*d^3})
///

-- test 17
TEST ///
	X = realProjectiveSpace 2
	c = ((entries vars ambient cohomologyRing X)_0)_2
	assert(stiefelWhitney X == {1, c, c^2})
	
	X = realProjectiveSpace 3
	assert(stiefelWhitney X == {1, 0, 0, 0})
///
