TEST ///
---basic test arising from ring documentation
S = ZZ/101[a,b,c,d];
            C = simplicialModule freeResolution coker vars S
            ring C
            assert(ring C === S)
            ring id_C
            assert(ring id_C === S)
///


TEST ///
--test for topdeg from doc
S = ZZ/101[a..c];
            C = simplicialModule(freeResolution coker vars S, 8)
            assert(topDegree C == 8)
            C' = simplicialModule(freeResolution coker vars S, 6)
	    assert(topDegree C' == 6)

	    S = ZZ/101[a..d]
            C0 = simplicialModule( S^2, 6, Degeneracy => true)
            f = dd^C0
            assert(source f == target f)
            f == 0
            isWellDefined C0
            C0 == 0
            assert(topDegree C0 == 6)
	    C5 = simplicialModule(S^0, 8, Degeneracy => true)
            assert(C5 == 0)
            assert(dd^C5 == 0)
	    assert(ss^C5 == 0)
            C5_0
///


TEST ///
R = QQ[a..d];
            I = ideal(a*d-b*c, b^2-a*c, c^2-b*d);
            C = simplicialModule(freeResolution(R^1/I), 4, Degeneracy => true)
            dd^C
            C.dd
	    ss^C
	    C.ss
            assert(dd^C === C.dd)
            assert(source dd^C === C)
            assert(target dd^C === C)
            assert(degree dd^C === -1)
	    assert(source ss^C === C)
            assert(target ss^C === C)
            assert(degree ss^C === 1)
	    dd^C_(2,0)
            assert(source dd^C_2 === C_2)
            assert(target dd^C_2 === C_1)
///


TEST ///
--direct sum testing
S = ZZ/101[a,b,c];
      C1 = simplicialModule(freeResolution coker vars S, 5, Degeneracy => true)
      C2 = simplicialModule(complex (ideal(a,b,c)) , 5, Degeneracy => true)
      D = C1 ++ C2
      assert isWellDefined D_[0]
      assert isCommutative D_[0]
      assert isWellDefined D_[1]
      assert isCommutative D_[1]
      assert(D^[0] * D_[0] == 1)
      assert(D^[1] * D_[1] == 1)
      assert(D^[0] * D_[1] == 0)
      assert(D^[1] * D_[0] == 0)
      assert(D_[0] * D^[0] + D_[1] * D^[1] == 1)
///

TEST ///
S = ZZ/101[a..d]
            moduleHash = hashTable { 0 => S^1,
		                     1 => S^1++S^2,
				     2 => S^1++S^2++S^2++S^1}
	    faceHash = hashTable {(1,0) => matrix {{1, a, b}},
		                  (1,1) => matrix {{1_S, 0, 0}},
				  (2,0) => matrix {{1, a, b, 0, 0, 0},
				                  {0, 0, 0, 1, 0, -b},
						  {0, 0, 0, 0, 1, a}},
				  (2,1) => matrix {{1_S, 0, 0, 0, 0, 0},
				                   {0, 1, 0, 1, 0,0},
				                   {0, 0, 1, 0, 1, 0}},
				  (2,2) => matrix {{1_S, 0, 0, 0, 0, 0},
				                   {0, 1, 0, 0, 0, 0},
						   {0, 0, 1, 0, 0, 0}}}
	    degenHash = hashTable {(0,0) => matrix {{1_S}, {0}, {0}},
		                   (1,0) => matrix {{1_S, 0, 0},
				                    {0, 0, 0},
						    {0, 0, 0},
						    {0, 1, 0},
						    {0, 0, 1},
						    {0, 0, 0}},
				   (1,1) => matrix {{1_S, 0, 0},
				                    {0, 1, 0},
						    {0, 0, 1},
						    {0, 0, 0},
						    {0, 0, 0},
						    {0, 0, 0}}}
	    T = simplicialModule(moduleHash, faceHash, degenHash, 2)
	    assert(T.?module)
	    assert T.?dd
	    assert T.?ss
	    T' = simplicialModule(moduleHash, faceHash, 2)
	   H1 = hashTable {0 => S^1, 1 => S^1, 2 => S^1}
	    H2 = hashTable {(1,0) => map(S^1, S^1, 0),
		            (1,1) => map(S^1, S^1, 0),
			    (2,0) => map(S^1, S^1, 0),
			    (2,1) => map(S^1, S^1, 0),
			    (2,2) => map(S^1, S^1, 0)}
	    H3 = hashTable {(0,0) => map(S^1, S^1, 0),
		            (1,0) => map(S^1, S^1, 0),
			    (1,1) => map(S^1, S^1, 0)}
	    U = simplicialModule(H1,H2,H3,2)
	    assert U.?dd
	    assert U.?ss
	    assert not isWellDefined U
///

TEST ///
S = ZZ/101[a..d]
            C0 = simplicialModule( S^2, 6, Degeneracy => true)
            f = dd^C0
            source f, target f
            f == 0
            assert isWellDefined C0
            C0 == 0
            assert(topDegree C0 ==6)
	    C2 = simplicialModule( S, 5, Degeneracy => true)
            I = ideal(a^2-b, c^3)
            C3 = simplicialModule( I, 7, Degeneracy => true)
            C4 = simplicialModule( (S/I), 8, Degeneracy => true)
	    assert isSimplicialModule C2
	    assert isSimplicialModule C3
	    assert isSimplicialModule C4
	    C5 = simplicialModule(S^0, 8, Degeneracy => true)
            assert(C5 == 0)
            assert (dd^C5 == 0)
	    assert(ss^C5 == 0)
            C5_0
///

TEST ///
S = ZZ/101[a..c]
      C = simplicialModule(freeResolution coker vars S, 4, Degeneracy => true)
      assert C.?complex
      assert isWellDefined C.complex
      assert(C_(1,0) == C.complex_0)
      assert(C_(1,1) == C.complex_1)
      assert(C_(2,1) == C.complex_1 ++ C.complex_1)
      tC = C ** C
      assert not tC.?complex
      tC_2
///


TEST ///
S = ZZ/101[a..c]
      C = simplicialModule(K = freeResolution coker vars S, 4)
      D = image id_C;
      assert(not(C === D))
      assert(C == D)
      E = simplicialModule(complex for i from 1 to 3 list 0*dd^K_i, 4)
      assert isWellDefined dd^E
      assert not(C == E)
      assert not(E == 0)
      f = id_C
      D = coker f
      assert(D == 0)
      C0 = simplicialModule( S^0, 5, Degeneracy => true)
      C1 = simplicialModule(complex(S^0, Base => 2), 5, Degeneracy => true)
      topDegree C0 == topDegree C1
      assert(C0 == C1)
      assert(C0 == 0)
      assert(C1 == 0)
///


TEST ///
R = QQ[a..d];
            I = ideal(a*d-b*c, b^2-a*c, c^2-b*d);
            C = simplicialModule(freeResolution(R^1/I), 4, Degeneracy => true)
	    isWellDefined C
            dd^C
            C.dd
	    ss^C
	    C.ss
            assert(dd^C === C.dd)
            assert(source dd^C === C)
            assert(target dd^C === C)
            assert(degree dd^C === -1)
	    assert(source ss^C === C)
            assert(target ss^C === C)
            assert(degree ss^C === 1)
            dd^C_(2,0)
            assert(source dd^C_2 === C_2)
            assert(target dd^C_2 === C_1)
///


TEST ///
S = ZZ/101[a,b,c];
      C1 = simplicialModule(freeResolution coker vars S, 5, Degeneracy => true)
      C2 = simplicialModule(complex (ideal(a,b,c)) , 5, Degeneracy => true)
      D = C1 ++ C2
      D_[0]
      assert isCommutative D_[0]
      D_[1]
      assert(D^[0] * D_[0] == 1)
      assert(D^[1] * D_[1] == 1)
      assert(D^[0] * D_[1] == 0)
      assert(D^[1] * D_[0] == 0)
      assert(D_[0] * D^[0] + D_[1] * D^[1] == 1)
      E = (chicken => C1) ++ (nuggets => C2)
      E_[chicken]
      E_[nuggets]
      assert(E^[chicken] * E_[chicken] == 1)
      assert(E^[nuggets] * E_[nuggets] == 1)
      assert(E^[chicken] * E_[nuggets] == 0)
      assert(E^[nuggets] * E_[chicken] == 0)
      assert(E_[chicken] * E^[chicken] + E_[nuggets] * E^[nuggets] == 1)
      F = directSum(C1, C2, simplicialModule(complex(S^2, Base => 1), 5, Degeneracy => true))
      prune (F^[0,1])
      assert isSimplicialMorphism oo
      prune (F_[0,2])
      assert isSimplicialMorphism oo
///


TEST ///
S = ZZ/101[a,b,c];
      C1 = simplicialModule freeResolution coker vars S
      C2 = simplicialModule(complex (ideal(a,b,c)), 3)
      D = C1 ++ C2
      L = components D
      assert(L_0 === C1)
      assert(L_1 === C2)
      E = (peanut => C1) ++ (butter => C2)
      components E
///


TEST ///
S = ZZ/101[a..c]
      Ca = simplicialModule(complex {matrix{{a}}}, 3)
      Cb = simplicialModule(complex {matrix{{b}}}, 3)
      Cc = simplicialModule(complex {matrix{{c}}}, 3)
      Cab = Cb ** Ca
      dd^Cab
      (prune normalize Cab).dd
      assert isWellDefined Cab
      Cabc = Cc ** Cab
      Cc ** Cb ** Ca
      dd^(nC = prune normalize Cabc)
      assert isWellDefined nC
      Cabc ** (S^1/(a,b,c));
      assert isWellDefined oo
      S^2 ** Cabc
      assert isWellDefined oo
///

TEST ///
R = QQ[a,b,c];
            I = ideal(a*b, a*c, b*c)
            C = simplicialModule(freeResolution I, 3, Degeneracy => true)
            D = truncate(3,C)
            assert isWellDefined D
	    assert(C == truncate(0, C))
	    A = ZZ/101[x_0, x_1, y_0, y_1, y_2, Degrees => {2:{1,0}, 3:{0,1}}];
            I = intersect(ideal(x_0, x_1), ideal(y_0, y_1, y_2))
            C = simplicialModule(freeResolution I, 3, Degeneracy => true)
            D1 = prune truncate({{1,1}}, C)
            D2 = truncate({{1,0}}, C)
            D3 = truncate({{0,1}}, C)
            D4 = truncate({{1,0},{0,1}}, C);
            D5 = truncate({{2,2}}, C);
            assert all({D1,D2,D3,D4,D5}, isWellDefined)
///


TEST ///
R = QQ[x,y,z]
            S = QQ[s,t]
            phi = map(S, R, {s, s+t, t})
            I = ideal(x^3, x^2*y, x*y^4, y*z^5)
            C = simplicialModule(freeResolution I, 3, Degeneracy => true)
            D = phi C
            assert isWellDefined D
            dd^D
            R = ZZ/101[a..d]
            S = ZZ/101[s,t]
            phi = map(S, R, {s^4, s^3*t, s*t^3, t^4}, DegreeMap => i -> 4*i)
            C = simplicialModule(freeResolution coker vars R, 4, Degeneracy => true)
            D = phi C
            assert isWellDefined D
///

TEST ///
R = QQ[x,y,z];
            S = QQ[s,t];
            phi = map(S, R, {s, s+t, t})
            I = ideal(x^3, x^2*y, x*y^4, y*z^5)
            C = simplicialModule(freeResolution I, 3, Degeneracy => true)
            D = phi ** C
            assert isWellDefined D
            assert isWellDefined dd^D
	    assert isWellDefined ss^D
	    A = R/(x^2+y^2+z^2);
            C ** A
            assert(map(A,R) ** C == C ** A)
	    use R
            I = ideal(x*y, x*z, y*z);
            J = I + ideal(x^2, y^2);
            g = inducedMap(module J, module I)
            assert isWellDefined g
            C = simplicialModule(complex {g}, 3, Degeneracy => true)
            D1 = phi C
            assert isWellDefined D1
            D2 = phi ** C
            assert isWellDefined D2
            prune D1
            prune D2
	    R = ZZ/101[a..d];
            S = ZZ/101[s,t];
            f = map(S, R, {s^4, s^3*t, s*t^3, t^4}, DegreeMap => i -> 4*i)
            C = simplicialModule(freeResolution coker vars R, 3, Degeneracy => true)
            D = f ** C
            D == f C
            assert isWellDefined D
///


TEST ///
S = ZZ/101[a,b,c,d,e];
            I = ideal(a,b) * ideal(c,d,e)
            F = simplicialModule((dual freeResolution I)[-4], 2, Degeneracy => true)
            C = HH F
            D = prune C
            g = D.cache.pruningMap
            assert isWellDefined g
            assert isSimplicialMorphism g
            assert (target g == C)
            assert (source g == D)
            g^-1
            assert(g*g^-1 == 1 and g^-1*g == 1)
            S = ZZ/101[a,b,c];
            I = ideal(a^2,b^2,c^2);
            J = I + ideal(a*b*c);
            FI = simplicialModule(freeResolution I, Degeneracy => true)
            FJ = simplicialModule(freeResolution J, Degeneracy => true)
            f = randomSimplicialMap(FJ, FI ** S^{-1}, Cycle => true)
            C = image f
            D = prune C
            g = D.cache.pruningMap
            assert isWellDefined g
            assert isSimplicialMorphism g
            assert (target g == C)
            assert (source g == D)
            g^-1
            assert(g*g^-1 == 1 and g^-1*g == 1)
	    h = prune f
            assert(source h === prune source f)
            assert(target h === prune target f)
///


TEST ///
R = ZZ/101[a,b,c];
            C = simplicialModule(F = freeResolution coker matrix{{a^2-b^2,b^3-c^3,c^4}}, Degeneracy => true)
            D = simplicialModule(G = freeResolution coker vars R, Degeneracy => true)
            H = hashTable { 0 => map(D_0, C_0, 1),
                1 => map(D_1, C_1, {{1, 0, 0, 0}, {0, a, 0, 0}, {0, -b, b^2, 0}, {0, 0, -c^2, c^3}}),
                2 => map(D_2, C_2, {{1, 0, 0, 0, 0, 0, 0, 0, 0, 0},
			             {0, a, 0, 0, 0, 0,0, 0, 0, 0},
				     {0, -b, b^2, 0, 0, 0, 0, 0, 0, 0},
				     {0, 0,-c^2, c^3, 0, 0, 0, 0, 0, 0},
				     {0, 0, 0, 0, a, 0, 0, 0, 0,0},
                                     {0, 0, 0, 0, -b, b^2, 0, 0, 0, 0},
                                     {0, 0, 0, 0, 0,-c^2, c^3, 0, 0, 0},
                                     {0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0},
                                     {0, 0, 0, 0, 0, 0, 0, -a*c^2, a*c^3, 0},
				     {0, 0, 0, 0, 0, 0, 0, b*c^2, -b*c^3, b^2*c^3}}),
                3 => map(D_3, C_3, {{1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0},
			            {0, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0},
				    {0, -b, b^2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0},
				    {0, 0, -c^2, c^3, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, a, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, -b, b^2, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0,-c^2, c^3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0,0, 0, 0, 0, 0, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0,0, 0, 0, 0, 0, 0, -b, b^2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0},
				    {0, 0, 0, 0, 0, 0, 0, 0, -c^2, c^3, 0, 0, 0, 0, 0, 0,0, 0, 0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0,0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-a*c^2, a*c^3, 0, 0, 0, 0, 0, 0, 0,0},
				    {0, 0, 0, 0, 0, 0,0, 0, 0, 0, b*c^2, -b*c^3, b^2*c^3, 0, 0, 0, 0, 0, 0, 0},
                                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0, 0, 0,0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -a*c^2,a*c^3, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, b*c^2, -b*c^3, b^2*c^3, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0, 0},
				    {0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -a*c^2, a*c^3,0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, b*c^2, -b*c^3, b^2*c^3, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2*c^3}})
			    }
            f = map(D, C, H)
            assert isWellDefined f
            assert isHomogeneous f
            assert(degree f == 0)
            assert isSimplicialMorphism f
	    h = map(C, C, hashTable {})
	    assert(h == 0)
///


TEST ///
R = QQ[a,b,c]
            C = simplicialModule(freeResolution coker vars R, Degeneracy => true)
            D = simplicialModule(freeResolution coker matrix{{a^2, b^2, c^2}}, Degeneracy => true)
            f = map(D, C, 0)
            assert isWellDefined f
            assert isSimplicialMorphism f
            g = map(C, C, 0, Degree => 13)
            assert isWellDefined g
            assert(degree g == 13)
            assert not isSimplicialMorphism g
            assert isCommutative g
            assert isHomogeneous g
            assert(source g == C)
            assert(target g == C)
            assert(map(C, C, 1) === id_C)
///


TEST ///
R = ZZ/101[a,b,c];
            C = simplicialModule(freeResolution coker vars R, Degeneracy => true)
            f = map(forgetComplex C, forgetComplex C, id_C)
            assert isWellDefined f
            assert(degree f == 0)
            assert isCommutative f
            assert isSimplicialMorphism f
            normalize f --notice how the normalization is not already pruned
	    normalize id_C
	    prune normalize f == normalize id_C
///


TEST ///
R = ZZ/101[x,y]/(x^3, y^3)
            C = simplicialModule(freeResolution(coker vars R, LengthLimit=>6), 6, Degeneracy => true)
            f = id_C;
            assert isWellDefined f
            assert isSimplicialMorphism f
///

TEST ///
R = ZZ/101[a,b,c];
            C = simplicialModule(F = freeResolution coker matrix{{a^2-b^2,b^3-c^3,c^4}}, Degeneracy => true)
            D = simplicialModule(G = freeResolution coker vars R, Degeneracy => true)
            H = hashTable { 0 => map(D_0, C_0, 1),
                1 => map(D_1, C_1, {{1, 0, 0, 0}, {0, a, 0, 0}, {0, -b, b^2, 0}, {0, 0, -c^2, c^3}}),
                2 => map(D_2, C_2, {{1, 0, 0, 0, 0, 0, 0, 0, 0, 0},
			             {0, a, 0, 0, 0, 0,0, 0, 0, 0},
				     {0, -b, b^2, 0, 0, 0, 0, 0, 0, 0},
				     {0, 0,-c^2, c^3, 0, 0, 0, 0, 0, 0},
				     {0, 0, 0, 0, a, 0, 0, 0, 0,0},
                                     {0, 0, 0, 0, -b, b^2, 0, 0, 0, 0},
                                     {0, 0, 0, 0, 0,-c^2, c^3, 0, 0, 0},
                                     {0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0},
                                     {0, 0, 0, 0, 0, 0, 0, -a*c^2, a*c^3, 0},
				     {0, 0, 0, 0, 0, 0, 0, b*c^2, -b*c^3, b^2*c^3}}),
                3 => map(D_3, C_3, {{1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0},
			            {0, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0},
				    {0, -b, b^2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0},
				    {0, 0, -c^2, c^3, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, a, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, -b, b^2, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0,-c^2, c^3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0, 0,0, 0, 0, 0, 0, a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				    {0,0, 0, 0, 0, 0, 0, -b, b^2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0},
				    {0, 0, 0, 0, 0, 0, 0, 0, -c^2, c^3, 0, 0, 0, 0, 0, 0,0, 0, 0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0,0, 0, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-a*c^2, a*c^3, 0, 0, 0, 0, 0, 0, 0,0},
				    {0, 0, 0, 0, 0, 0,0, 0, 0, 0, b*c^2, -b*c^3, b^2*c^3, 0, 0, 0, 0, 0, 0, 0},
                                    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0, 0, 0,0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -a*c^2,a*c^3, 0, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, b*c^2, -b*c^3, b^2*c^3, 0, 0, 0, 0},
				    {0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2, 0, 0, 0},
				    {0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -a*c^2, a*c^3,0, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, b*c^2, -b*c^3, b^2*c^3, 0},
				    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, a*b^2*c^3}})
			    }
            f = map(D, C, H)
            assert isWellDefined f
            assert isHomogeneous f
            assert(degree f == 0)
            assert isSimplicialMorphism f
	    g = randomSimplicialMap(D,C); --outputs are large
	    normalize g
            assert isWellDefined g
            assert not isCommutative g
	    h = randomSimplicialMap(D,C, Cycle => true);
	    normalize h
            assert isWellDefined h
            assert isSimplicialMorphism h
///


TEST ///
R = ZZ/101[a..d]
            I = ideal(a^2, b^2, c^2)
            J = I + ideal(a*b*c)
            FI = simplicialModule(freeResolution I, Degeneracy => true)
            FJ = simplicialModule(freeResolution J, Degeneracy => true)
            f = randomSimplicialMap(FJ, FI, Cycle=>true)
            source f
            assert isWellDefined f
            assert isSimplicialMorphism f
            assert(source f == FI)
            assert(target f == FJ)
	    kk = coker vars R
            F = simplicialModule(freeResolution kk, Degeneracy => true)
            assert(source dd^F == F)
            assert(target dd^F == F)
            assert(degree dd^F == -1)
///

TEST ///
R = ZZ/101[a..d];
            I = ideal(a^2, b^2, c^2)
            FI = simplicialModule(freeResolution I, Degeneracy => true)
            assert(degree dd^FI == -1)
	    assert(degree ss^FI == 1)
///


TEST ///
S = ZZ/101[a,b,c,d];
            I = minors(2, matrix{{a,b,c},{b,c,d}})
            C = simplicialModule(freeResolution (S^1/I), Degeneracy => true)
            assert isHomogeneous dd^C
            f = randomSimplicialMap(C, C, Degree => -1)
            assert isHomogeneous f
            f = randomSimplicialMap(C, C, InternalDegree => 2)
	    phi = map(S, S, {1,b,c,d})
            D = phi C
            dd^D
            assert not isHomogeneous dd^D
///


TEST ///
S = ZZ/101[a..c];
            C = simplicialModule(freeResolution coker matrix{{a^2, b^2, c^2}}, Degeneracy => true)
            f = dd^C
            f^2
            assert(source f == target f)
            assert(degree f == -1)
            assert(degree f^2 == -2)
	    g = randomSimplicialMap(C, C)
            assert isWellDefined g^2
            assert isWellDefined g^3
	    assert not(f^0 == id_(C[1]))
            assert(g^0 == id_C)
	    h = randomSimplicialMap(C, C)
            assert isWellDefined h^-1
            assert(h * h^-1 == id_C)
            assert isWellDefined h^-4
            assert(h^-4 * h^4 == id_C)
///


TEST ///
S = ZZ/101[a..c]
       C = simplicialModule(freeResolution coker vars S, Degeneracy => true)
       f = id_C
       assert(f == 1)
       assert(0 * id_C == 0)
       g = randomSimplicialMap(C, C)
       h = inducedMap(coker g, target g)
       assert(h == 0)
       g = randomSimplicialMap(C, C, InternalDegree=>1, Cycle=>true)
       h = inducedMap(coker g, target g)
       assert(h != 1)
       D = prune image g
       p = D.cache.pruningMap
       p == 1
       assert(coker p == 0 and ker p == 0)
       assert(prune p == 1)
///


TEST ///
S = ZZ/101[a,b,c];
      C = simplicialModule(freeResolution coker vars S, Degeneracy => true)
      D = C ** C
      isWellDefined D
      f1 = prune randomSimplicialMap(D, C, Cycle => true, InternalDegree => 1);
      g1 = prune normalize f1
      assert(isCommutative g1)
      assert(isCommutative f1)
      assert(degree f1 == 0)
      f2 = randomSimplicialMap(D, C, Cycle => true);
      assert isCommutative f2
      assert(degree f2 == 0)
      assert isSimplicialMorphism f2
///

TEST ///
S = ZZ/101[a,b,c];
      C = simplicialModule(freeResolution coker vars S, Degeneracy => true)
      D = C ** C
      f1 = randomSimplicialMap(D, C, Cycle => true, InternalDegree => 1);
      assert isSimplicialMorphism f1
      assert(degree f1 == 0)
///

TEST ///
R = ZZ/101[a..d];
            I = ideal(c^2-b*d, b*c-a*d, b^2-a*c)
            J = ideal(I_0, I_1)
            C = simplicialModule(koszulComplex vars R, Degeneracy => true)
            f = map(R^1/I, R^1/J, 1)
            C ** f;
	    assert isSimplicialMorphism oo
            f ** C;
	    assert isSimplicialMorphism oo
            f' = random(R^2, R^{-1, -1, -1})
            C ** f';
            f' ** C;
            assert isWellDefined(C ** f')
            assert isWellDefined(f' ** C)
	    f'' = random(source f', R^{-2,-2})
            assert((C ** f') * (C ** f'') == C ** (f' * f''))
            assert(C ** id_(R^{-1,-2,-3}) == id_(C ** R^{-1,-2,-3}))
///

TEST ///
S = ZZ/101[a..c]
            C = simplicialModule(freeResolution coker vars S, Degeneracy => true)
            D = simplicialModule((freeResolution coker matrix{{a^2,a*b,b^3}})[-1], Degeneracy => true)
            f = randomSimplicialMap(D,C)
            E = simplicialModule((dual C.complex)[-3], Degeneracy => true)
            F = simplicialModule((dual D.complex)[-3], 3, Degeneracy => true)
            g = randomSimplicialMap(F,E)
            h = f ** g;
            assert isWellDefined h
            assert(prune source h == C ** E)
            assert(prune target h == D ** F)
	    fE = f ** E;
            assert(fE == f ** id_E)
            k = coker vars S
            gk = g ** k;
	    D' = simplicialModule((freeResolution coker matrix{{a^2,a*b,c^3}})[-1], 3, Degeneracy => true)
            f' = randomSimplicialMap(D', D)
            assert((f' * f) ** g == (f' ** g) * (f ** id_E))
            assert((f' * f) ** g == (f' ** id_F) * (f ** g))
            F' = simplicialModule(dual (freeResolution coker matrix{{a^2,a*b,a*c,b^3}})[-3], Degeneracy => true)
            g' = randomSimplicialMap(F', F)
            assert(f ** (g' * g) == (f ** g') * (id_C ** g))
            assert(f ** (g' * g) == (id_D ** g') * (f ** g))
///


TEST ///
R = QQ[a,b,c];
            C = simplicialModule(freeResolution ideal(a*b, a*c, b*c), 3, Degeneracy => true)
            D = simplicialModule((freeResolution ideal(a*b, a*c, b*c, a^2-b^2))[-1], 3, Degeneracy => true)
            f = randomSimplicialMap(D,C, Cycle => true)
            g = truncate(3,f);
            assert isWellDefined g
            assert (source g == truncate(3, source f))
            assert (target g == truncate(3, target f))
	    assert(f == truncate(0, f))
	    A = ZZ/101[x_0, x_1, y_0, y_1, y_2, Degrees => {2:{1,0}, 3:{0,1}}];
            I = intersect(ideal(x_0, x_1), ideal(y_0, y_1, y_2))
            C = simplicialModule(freeResolution I, 4, Degeneracy => true)
            J = intersect(ideal(x_0^2, x_1^2), ideal(y_0^2, y_1^2, y_2^2))
            D = simplicialModule(freeResolution J, 4, Degeneracy => true)
            f = simplicialModule(extend(C.complex, D.complex, id_(A^1)), 2)
            g1 = prune truncate({{1,1}}, f);
	    g1_0
	    g1_1
            g2 = truncate({{1,0}}, f);
	    g2_1
            g3 = truncate({{0,1}}, f);
            g4 = truncate({{1,0},{0,1}}, f);
	    g4_1
            g5 = truncate({{2,2}}, f);
            assert all({g1,g2,g3,g4,g5}, isWellDefined)
///

TEST ///
R = QQ[a,b,c,d];
            S = QQ[s,t];
            phi = map(S, R, {s, s+t, t, s-t})
            I = ideal(a*b, b*c, c*d)
            J = I + ideal(a^2, b^2, c^2, d^2)
            CI = simplicialModule(freeResolution I, 4, Degeneracy => true)
            CJ = simplicialModule(freeResolution J, Degeneracy => true)
            f = simplicialModule(extend(CJ.complex, CI.complex, map(CJ_0, CI_0, 1)), Degeneracy => true)
            assert isWellDefined f
            g = phi ** f
            assert isWellDefined g
            assert isWellDefined dd^(source g)
	    assert isWellDefined ss^(source g)
            dd^(target g);
            simplicialModule prune HH normalize g
	    assert isSimplicialMorphism oo
///

TEST ///
needsPackage "Truncations"
            kk = ZZ/32003
            R = kk[a,b,c]
            F = simplicialModule(freeResolution (ideal gens R)^2, 2, Degeneracy => true)
            C1 = truncate(3, F);
	    prune normalize C1
            C2 = truncate(4, F);
	    prune normalize C2
            assert isWellDefined C1
            assert isWellDefined C2
            f = inducedMap(C1, C2);
	    prune normalize f
            assert isWellDefined f
            f1 = inducedMap(F, C1)
            f2 = inducedMap(F, C2)
            assert isWellDefined f1
            assert isWellDefined f2
            assert(f2 == f1 * f)
///


TEST ///
R = ZZ/101[a..d];
            C = simplicialModule(freeResolution coker matrix{{a*b, a*c^2, b*c*d^3, a^3}}, Degeneracy => true)
            D = simplicialModule(freeResolution coker matrix{{a*b, a*c^2, b*c*d^3, a^3, a*c*d}}, 3, Degeneracy => true)
            f = randomSimplicialMap(D, C, Cycle => true);
	    prune normalize f
            g = randomSimplicialMap(D, C, Boundary => true);
	    prune normalize g
	    f+g;
	    assert isSimplicialMorphism oo
            f-g;
	    assert isSimplicialMorphism oo
            -f;
            3*f;
            0*f
            a*f;
            assert(0*f == 0)
            assert(1*f == f)
            assert((-1)*f == -f)
            assert(-(f-g) == g-f)
            assert((a+b)*f == a*f + b*f)
            assert(a*(f+g) == a*f + a*g)
            assert isSimplicialMorphism (f+g)
	    h = randomSimplicialMap(C, C);
	    prune normalize h
            prune normalize(h+1)
            assert(h+1 == h + id_C)
            assert(h+a == h + a*id_C)
            assert(1-h == id_C - h)
            assert(b-c*h == -c*h + b*id_C)
            assert(b-h*c == -h*c + id_C*b)
///


TEST ///
R = ZZ/101[a..d];
            C1 = simplicialModule((freeResolution coker matrix{{a,b,c}})[1], Degeneracy => true)
            C2 = simplicialModule(freeResolution coker matrix{{a*b,a*c,b*c}}, Degeneracy => true)
            D = simplicialModule(freeResolution coker matrix{{a^2,b^2,c*d}}, Degeneracy => true)
            f = randomSimplicialMap(D, C1)
            g = randomSimplicialMap(D, C2)
	    h = f|g
            assert isWellDefined h
            assert(source h === source f ++ source g)
            assert(target h === target f)
///

TEST ///
R = ZZ/101[a..d];
            D1 = simplicialModule((freeResolution coker matrix{{a,b,c}})[1], Degeneracy => true)
            D2 = simplicialModule(freeResolution coker matrix{{a*b,a*c,b*c}}, Degeneracy => true)
            C = simplicialModule(freeResolution coker matrix{{a^2,b^2,c*d}}, Degeneracy => true)
            f = randomSimplicialMap(D1, C)
            g = randomSimplicialMap(D2, C)
	    h = f||g
            assert isWellDefined h
            assert(target h === target f ++ target g)
            assert(source h === source f)
///

TEST ///
R = ZZ/101[a..d];
            C1 = simplicialModule((freeResolution coker matrix{{a,b,c}})[1], Degeneracy => true)
            C2 = simplicialModule(freeResolution coker matrix{{a*b,a*c,b*c}}, Degeneracy => true)
            D1 = simplicialModule((freeResolution coker matrix{{a,b,c}}),2,  Degeneracy => true)
            D2 = simplicialModule(freeResolution coker matrix{{a^2, b^2, c^2}}[-1], 2, Degeneracy => true)
            f = randomSimplicialMap(D1, C1, Cycle => true)
            g = randomSimplicialMap(D2, C2, Cycle => true)
	    h = f ++ g
            assert isWellDefined h
	    assert isSimplicialMorphism h
	    directSum(f, g, f[2])
            h2 = directSum(peanut => f, butter => g, jelly => f[2])
	    indices h2
            h2_[butter,jelly]
            assert(source oo == C2 ++ C1[2])
	    assert(h_[0]^[0] == f)
            assert(h_[1]^[1] == g)
            assert(h_[0]^[1] == 0)
            assert(h_[1]^[0] == 0)
            assert(h_[0] == h * (C1 ++ C2)_[0])
            assert(h_[1] == h * (C1 ++ C2)_[1])
            assert(h^[0] == (D1 ++ D2)^[0] * h)
            assert(h^[1] == (D1 ++ D2)^[1] * h)
///

TEST ///
S = ZZ/101[a,b,c,d];
      C = simplicialModule(freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d),3, Degeneracy => true)
      D = simplicialModule(freeResolution ideal(a,b,c), Degeneracy => true)
      f = randomSimplicialMap(D, C, Cycle => true, InternalDegree => 0)
      assert isCommutative f
      assert isWellDefined prune image f
      prune normalize oo
      i = inducedMap(forgetComplex target f, image f)
      isSimplicialMorphism i
      normalize i
      g1 = inducedMap(target f, image f)
      assert(ker g1 == 0)
      assert(image g1 == image f)
///


TEST ///
S = ZZ/101[a,b,c,d];
      C = simplicialModule(freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d), 3, Degeneracy => true)
      D = simplicialModule(freeResolution ideal(a,b,c), Degeneracy => true)
      f = randomSimplicialMap(D, C, Cycle => true, InternalDegree => 0)
      assert isCommutative f
      g1 = inducedMap(coimage f, source f)
      assert(coimage g1 == coimage f)
      assert(coker g1 == 0)
///

TEST ///
S = ZZ/101[a,b,c,d];
      C = simplicialModule(freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d), 3, Degeneracy => true)
      D = simplicialModule(freeResolution ideal(a,b,c), Degeneracy => true)
      f = randomSimplicialMap(D, C, Boundary => true, InternalDegree => 0)
      assert isCommutative f
      assert isWellDefined prune ker f
      h1 = inducedMap(source f, ker f)
      assert(ker f == image h1)
      assert(ker h1 == 0)
///


TEST ///
S = ZZ/101[a,b,c,d];
      C = simplicialModule(freeResolution ideal(b^2-a*c, b*c-a*d, c^2-b*d), 3, Degeneracy => true)
      D = simplicialModule(freeResolution ideal(a,b,c), Degeneracy => true)
      f = randomSimplicialMap(D, C, Cycle => true, InternalDegree => 0)
      assert isCommutative f
      assert isWellDefined prune coker f
      prune normalize oo
      prune HH coker f
      g1 = inducedMap(coker f, target f)
      assert(coker f == image g1)
      assert(coker g1 == 0)
///


TEST ///
R = ZZ/101[a..d];
            C1 = simplicialModule((freeResolution coker matrix{{a,b,c}})[1], 3, Degeneracy => true)
            C2 = simplicialModule(freeResolution coker matrix{{a*b,a*c,b*c}}, 3, Degeneracy => true)
            D1 = simplicialModule((freeResolution coker matrix{{a,b,c}}), Degeneracy => true)
            D2 = simplicialModule(freeResolution coker matrix{{a^2, b^2, c^2}}[-1], 3, Degeneracy => true)
            f = randomSimplicialMap(D1, C1, Cycle => true)
	    assert isCommutative f
            g = randomSimplicialMap(D2, C2, Cycle => true)
	    assert isCommutative g
	    h = f ++ g;
	    assert(h_[0] == h * (C1 ++ C2)_[0])
            assert(h_[1] == h * (C1 ++ C2)_[1])
            assert(h^[0] == (D1 ++ D2)^[0] * h)
            assert(h^[1] == (D1 ++ D2)^[1] * h)
	    assert(h_[0]^[0] == f)
            assert(h_[1]^[1] == g)
            assert(h_[0]^[1] == 0)
            assert(h_[1]^[0] == 0)
	    h = (chicken => f) ++ (nuggets => g);
	    indices h
            assert(h_[chicken]^[chicken] == f)
            assert(h_[nuggets]^[nuggets] == g)
///


TEST ///
S = ZZ/101[a..c]
            C = simplicialModule(freeResolution coker matrix{{a*b, a*c, b*c}}, 3, Degeneracy => true)
            D = simplicialModule(freeResolution coker vars S, Degeneracy => true)
            f = randomSimplicialMap(D,C)
            assert isWellDefined f
            assert not isCommutative f
	    g = randomSimplicialMap(D,C, Cycle => true)
            assert isWellDefined g
            assert isCommutative g
            assert isSimplicialMorphism g
	    h = randomSimplicialMap(D,C, Boundary => true)
            assert isWellDefined h
            assert isCommutative h
            assert isSimplicialMorphism h
            assert isNullHomotopic normalize h
            nullHomotopy normalize h
	    p = randomSimplicialMap(D, C, Cycle => true, Degree => -1)
            assert isWellDefined p
            assert isCommutative p
            assert isSimplicialMorphism p
	    q = randomSimplicialMap(D, C, Boundary => true, InternalDegree => 2);
            assert isCommutative q
            assert isSimplicialMorphism q
            assert(source q == C)
            assert(target q == D)
            assert isNullHomotopic normalize q
///

TEST ///
R = ZZ/101[a,b,c];
            B = simplicialModule(freeResolution coker matrix{{a^2*b, a*b*c, c^3}},  Degeneracy => true)
            C = simplicialModule(freeResolution coker vars R, 2, Degeneracy => true)
            h = randomSimplicialMap(C, B, Cycle => true)
            f = inducedMap(C, image h)
	    assert isCommutative f
            g = inducedMap(coker h, C)
	    assert isCommutative g
            assert isShortExactSequence(g,f)
	    I = ideal(a^3, b^3, c^3)
            J = I + ideal(a*b*c)
            K = I : ideal(a*b*c)
            SES = complex{
                map(comodule J, comodule I, 1),
                map(comodule I, (comodule K) ** R^{-3}, {{a*b*c}})
                }
            assert isWellDefined SES
            assert isShortExactSequence(dd^SES_1, dd^SES_2)
            (g,f) = (horseshoeResolution SES)/simplicialModule;
            assert isShortExactSequence(g,f)
///


TEST ///
S = ZZ/101[a..d]
      D = simplicialModule(S^1, 5)
      D[-1]
      oo.dd
      D[-2]
      C = simplicialModule(freeResolution coker vars S, Degeneracy => true)
      dd^C_(3,0)
      ss^C_(2,0)
      D = C[1]
      assert isWellDefined D
      assert isWellDefined dd^D_(2,0)
      ss^D_(1,0)
      C2 = simplicialModule(freeResolution (S^1/(a^2, b^2, c^2, d^2)), Degeneracy => true)
      C3 = simplicialModule(freeResolution (S^1/(a^2, b^3, c^4, d^5)), Degeneracy => true)
      f2 = simplicialModule(extend(C.complex, C2.complex, map(C_0, C2_0, 1)));
      f3 = simplicialModule(extend(C2.complex, C3.complex, map(C2_0, C3_0, 1)));
      assert isWellDefined (f2[-1])
      assert isSimplicialMorphism (f2[-1])
      assert isSimplicialMorphism (f3[-2])
///


TEST ///
S = ZZ/101[a,b,c];
      C1 = simplicialModule(freeResolution coker vars S, Degeneracy => true)
      D1 = C1 ++ simplicialModule(complex(S^13)[-2], 3, Degeneracy => true)
      assert isWellDefined D1
      assert(D1.?ss) --knows to cache degeneracy maps if inputs have degeneracy maps
      C2 = simplicialModule(ideal(a,b,c), 3, Degeneracy => true)
      C1 ++ C2
      assert isWellDefined(C1 ++ C2)
      C3 = directSum(C1,C2,C2[-2])
      assert isWellDefined C3
      C4 = directSum(first => C1, second => C2)
      assert isCommutative C4_[first] -- inclusion map C1 --> C4
      assert isCommutative C4^[first] -- projection map C4 --> C1
      assert(C4^[first] * C4_[first] == 1)
      assert(C4^[second] * C4_[second] == 1)
      assert(C4^[first] * C4_[second] == 0)
      assert(C4^[second] * C4_[first] == 0)
      assert(C4_[first] * C4^[first] + C4_[second] * C4^[second] == 1)
      assert isShortExactSequence(C4^[first], C4_[second])
      assert isShortExactSequence(C4^[second], C4_[first])
///


TEST ///
Q = ZZ/101[a..d]
	    K = koszulComplex vars Q
	    S = simplicialModule(K, Degeneracy => true)
	    nK = naiveNorm(S)
	    assert isWellDefined nK
	    prune HH nK
///

TEST ///
Q = QQ[x_1..x_3];
  K = koszulComplex vars Q;
  assert isWellDefined simplicialModule(K, 4) --no degeneracy here
  S = simplicialModule(K,4, Degeneracy => true)
  S.dd**Q^1
  assert isWellDefined oo
  assert isWellDefined S
  Sdegen = simplicialModule(K,4,Degeneracy => true)
  S.dd
  assert Sdegen.?ss --this is the version that actually has degeneracy maps cached
  simplicialModule(K, 5)
  C = complex(Q^1, Base => 1)
  simplicialModule(C, 3)
  assert isWellDefined oo
  simplicialModule(complex(Q^2, Base => 2), 6, Degeneracy => true)
  assert isWellDefined oo --nice, now we can be confident the objects we are messing with actually make any sense
  assert oo.?ss
  assert isSimplicialMorphism id_S
  phi = exteriorInclusion(S);
  assert isWellDefined phi
  assert isCommutative phi
  assert isWellDefined prune phi
  assert isCommutative prune phi
  assert isWellDefined prune coker phi
  K = koszulComplex {x_1,x_2}
  Sn = simplicialModule(K, 4, Degeneracy => true)
  phi = exteriorInclusion(K)
  assert isCommutative phi
  assert isWellDefined phi
  assert isCommutative prune phi
  assert isWellDefined prune phi
  sphi = exteriorInclusion(Sn);
  psphi = prune sphi;
  assert isWellDefined (source psphi).cache.pruningMap
  assert isCommutative (source psphi).cache.pruningMap 
  assert isSimplicialMorphism (source psphi).cache.pruningMap  --pruningMaps are well-defined morphisms
  S = simplicialModule(K,6)
  

  p = randomComplexMap(K, K, Degree => 1, InternalDegree => 1)
  sp = simplicialModule p
  assert isWellDefined sp
  assert not isCommutative sp
  normalize(sp, CheckComplex => false)
  prune oo  ---reobtain p
  p' = randomComplexMap(K, K, Degree => -1)
  sp' = simplicialModule p'
  assert isWellDefined sp'
  assert not isCommutative sp'
  normalize(sp', CheckComplex => false)
  prune oo --reobtain p'
  p = randomComplexMap(K, K, Degree => 1, Cycle => true, InternalDegree => 2)
  sp = simplicialModule p
  assert isWellDefined sp
  assert isCommutative sp
  normalize(sp, CheckComplex => false)
  prune oo  ---reobtain p
  diffs = simplicialModule K.dd
  isWellDefined diffs
  assert isCommutative diffs

  assert(image(id_S) == S)
  assert(simplicialModule(id_K, 6) == id_S)
  assert isSimplicialMorphism id_S
  simplicialModule(complex(Q^0), 6, Degeneracy => true) -- should be able to input 0 and get a well-defined output
  assert isWellDefined oo
  phi = randomSimplicialMap(S, S, Cycle => true, InternalDegree => 1)
  assert isWellDefined phi
  assert isCommutative phi

  bS = basis(0, forgetComplex S)
  prune bS.dd
  assert isWellDefined bS
  assert isWellDefined prune bS
  bid = basis(0, id_(forgetComplex S))
  assert isWellDefined bid
  assert isCommutative bid
  assert isWellDefined prune bid
  assert isCommutative prune bid

  fS = forgetComplex S
  assert not fS.?ss

  bS = basis(1, forgetComplex S)
  assert isWellDefined bS
  prune bS.dd
  assert isWellDefined oo
  --prune bS.ss
  --assert isWellDefined oo
  normalize bS
  prune oo
  prune basis(1, K)

  bS = basis(3, forgetComplex S)
  assert isWellDefined bS
  prune bS.dd;
  assert isWellDefined oo
  --prune bS.ss;
  --assert isWellDefined oo
  normalize bS
  prune oo
  prune basis(3, koszulComplex vars Q)

  tS = truncate(1, S)
  assert isWellDefined tS
  prune tS
  assert isWellDefined prune tS
  tS = truncate(1, fS)
  assert isWellDefined tS
  prune tS
  assert isWellDefined prune tS
  assert isCommutative truncate(2, id_S)
  assert isCommutative truncate(2, id_fS)

  K = koszulComplex {x_1,x_2}
  assert isWellDefined (Sc = schurMap({1,1}, K))
  prune extPower(2, K)
  assert isWellDefined prune schurMap({2,1}, K, TopDegree => 4)
///


TEST ///
Q = ZZ/2[a,b]
	    K = koszulComplex vars Q;
	    F = freeResolution( (ideal vars Q)^2)
	    phi = extend(K, F, id_(K_0))
            f = elapsedTime prune schurMap({2}, phi)
	    assert isCommutative f
	    assert isWellDefined f
	    prune HH source f
	    prune HH target f
	    prune HH f
///


-- simplicialTensor of two length-1 complexes.  A lightweight revival of a test
-- block that was silenced for performance; its heavier extPower/wedgeProduct
-- half at top degree 6 is already covered by the test block immediately below.
TEST ///
Q = ZZ/101[x_1,x_2];
    K1 = complex {matrix{{x_1}}};
    K2 = complex {matrix{{x_2}}};
    T1 = K1 ** K2
    T2 = prune simplicialTensor({K1, K2})
    assert isWellDefined T2
    -- the (Complex,Complex) dispatch agrees with the (List) dispatch
    assert(simplicialTensor(K1, K2) == simplicialTensor({K1, K2}))
    -- the simplicial tensor product is homotopy equivalent to the classical one
    phi1 = extend(T1, T2, id_(T1_0))
    phi2 = extend(T2, T1, id_(T1_0))
    assert(phi1 * phi2 == id_T1)
    assert isNullHomotopic(phi2 * phi1 - id_T2)
///

TEST ///
R = ZZ/101[x_1..x_3];
	    K = koszulComplex vars R
	    S = simplicialModule(K,10, Degeneracy => true)
	    keys S
	    assert(K == normalize S)
	    Kn = normalize(S, CheckComplex => false)
	    Kn.dd
	    assert not(K == Kn)
	    assert(K == prune Kn)
	    Q = ZZ/3[a,b];
	    K = koszulComplex vars Q
	    SK = simplicialModule(K,4) --want top degree 6 since the resulting complexes should have length 6
	    w21K = extPower(2, SK) ** SK
	    w3K = extPower(3, SK)
	    H = hashTable for i from 0 to 6 list i => dual wedgeProduct(2,1, dual SK_i);
	    inclusion = map(w21K, w3K, H);
	    assert isWellDefined inclusion
	    assert isCommutative inclusion
	    Q = ZZ/2[a,b,c]
	    K = koszulComplex vars Q
	    phi = elapsedTime exteriorInclusion(K,3); --specify top degree 3
	    assert isWellDefined phi
	    assert isCommutative phi
///

-- Tests for bugs fixed in the polishing session.
-- These exercise edge cases around non-free modules, quotient rings,
-- and consistency of face map source/target in the Dold-Kan construction.

-- Test: isWellDefined and isSimplicialModule over a quotient ring
-- Exercises the mapsEq/mapIsId helpers that were added to handle
-- non-free modules where matrix(A)*matrix(B) != matrix(A*B).
TEST ///
    R = ZZ/101[x,y]/(x^3, y^3);
    C = simplicialModule(freeResolution(coker vars R, LengthLimit=>5), 5, Degeneracy => true);
    assert isWellDefined C
    assert isSimplicialModule C
    f = id_C;
    assert isWellDefined f
    assert isSimplicialMorphism f
    assert isCommutative f
    assert(f == 1)
///

-- Test: prune on a simplicial module that retains .complex
-- This verifies the forgetComplex fix at the start of prune.
TEST ///
    R = QQ[a,b,c];
    I = ideal(a*b, a*c, b*c);
    C = simplicialModule(freeResolution I, 3, Degeneracy => true);
    assert C.?complex
    D = prune C;
    assert isWellDefined D
    g = D.cache.pruningMap;
    assert isWellDefined g
    assert isSimplicialMorphism g
    assert(g * g^-1 == 1)
    assert(g^-1 * g == 1)
///

-- Test: forgetComplex produces a well-defined simplicial module
-- and preserves the normalization.
TEST ///
    R = ZZ/101[a,b,c];
    K = koszulComplex vars R;
    S = simplicialModule(K, 4, Degeneracy => true);
    assert S.?complex
    fS = forgetComplex S;
    assert(not fS.?complex)
    assert isWellDefined fS
    assert isSimplicialModule fS
    assert(K == prune normalize fS)
///

-- Test: faceMap0Direct source/target consistency at topDegree >= 3
-- Verifies that face maps at all levels have consistent source/target
-- for simplicial modules built from complexes of length >= 3.
TEST ///
    R = ZZ/101[a,b,c];
    C = simplicialModule(freeResolution coker vars R, 3);
    assert isWellDefined C
    -- Also test with the horseshoe resolution (produces length-3 complexes)
    I = ideal(a^3, b^3, c^3);
    J = I + ideal(a*b*c);
    K = I : ideal(a*b*c);
    SES = complex{
        map(comodule J, comodule I, 1),
        map(comodule I, (comodule K) ** R^{-3}, {{a*b*c}})
        };
    (g0,f0) = horseshoeResolution SES;
    sg = simplicialModule g0;
    sf = simplicialModule f0;
    assert isWellDefined(source sg)
    assert isWellDefined(source sf)
    assert isWellDefined(target sg)
    assert isWellDefined(target sf)
///

-- Test: isShortExactSequence with image/kernel having
-- different generator representations (the degree-by-degree fix).
TEST ///
    R = ZZ/101[a,b,c];
    C = simplicialModule(freeResolution coker vars R, 2, Degeneracy => true);
    B = simplicialModule(freeResolution coker matrix{{a^2*b, a*b*c, c^3}}, Degeneracy => true);
    h = randomSimplicialMap(C, B, Cycle => true);
    f = inducedMap(C, image h);
    g = inducedMap(coker h, C);
    assert isShortExactSequence(g, f)
    -- Verify the components individually
    assert(g*f == 0)
    assert(kernel f == 0)
    assert(coker g == 0)
///

-- Test: normalize on a direct sum built via forgetComplex
-- Exercises the forgetComplex map source/target rewriting.
TEST ///
    R = ZZ/101[a,b,c];
    K = koszulComplex vars R;
    S = simplicialModule(K, 3, Degeneracy => true);
    fS = forgetComplex S;
    D = fS ++ fS;
    assert isWellDefined D
    N = prune normalize D;
    assert isWellDefined N
    assert(N == K ++ K)
///

-- Test: truncate on a simplicial module over a multigraded ring
-- with the complex early-return path.
TEST ///
    A = ZZ/101[x_0, x_1, y_0, y_1, y_2, Degrees => {2:{1,0}, 3:{0,1}}];
    I = intersect(ideal(x_0, x_1), ideal(y_0, y_1, y_2));
    C = simplicialModule(freeResolution I, 3, Degeneracy => true);
    D = truncate({{1,1}}, C);
    assert isWellDefined D
    assert(C == truncate({{0,0}}, C))
///

-- Test: composition of simplicial module maps where source/target
-- involve non-free modules (exercises the direct composition fix).
TEST ///
    R = ZZ/101[a,b,c];
    C = simplicialModule(freeResolution coker vars R, 2, Degeneracy => true);
    f = id_C;
    g = id_C;
    h = f * g;
    assert(h == 1)
    assert isWellDefined h
    -- Also test composition of random cycle maps
    D = simplicialModule(freeResolution coker matrix{{a^2,b^2,c^2}}, 2, Degeneracy => true);
    f1 = randomSimplicialMap(D, C, Cycle => true);
    f2 = randomSimplicialMap(C, D, Cycle => true);
    h1 = f1 * f2;
    assert isWellDefined h1
    assert isCommutative h1
///

-- Test: RingMap applied to a non-free complex simplicial module
-- Exercises the tensor(phi, S.complex) fix.
TEST ///
    R = ZZ/101[x,y,z];
    S = ZZ/101[a,b,c];
    phi = map(S, R, {a,b,c});
    I = ideal(x*y, x*z, y*z);
    J = I + ideal(x^2, y^2);
    g = inducedMap(module J, module I);
    C = simplicialModule(complex {g}, 3, Degeneracy => true);
    D = phi C;
    assert isWellDefined D
///

TEST ///
-- tensorLES: the long exact sequence of homology from the canonical
-- short exact sequence 0 -> wedge^2 C -> C ** C -> Sym^2 C -> 0
Q = ZZ/2[a,b]
K = koszulComplex vars Q
les = tensorLES(K, 4)
assert isWellDefined les
assert instance(les, Complex)
-- a long exact sequence is exact, so it has no homology
assert(prune HH les == 0)
///

TEST ///
-- symmetricQuotient: the surjection C ** C -> Sym^2 C
Q = ZZ/2[a,b]
K = koszulComplex vars Q
phi = prune symmetricQuotient(K, 4)
assert isWellDefined phi
assert isCommutative phi
-- symmetricQuotient is a surjection onto the second symmetric power
assert(prune coker phi == 0)
-- 0 -> wedge^2 K -> K ** K -> Sym^2 K -> 0 is a short exact sequence
assert isShortExactSequence(symmetricQuotient(K, 4), exteriorInclusion(K, 4))
-- the Module dispatch
assert isWellDefined symmetricQuotient(Q^2)
///

