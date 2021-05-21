invariantsSn = (n) -> (
    -- ring of invariants of S_n
    x := getSymbol "x";
    R := ZZ/101[x_0 .. x_(n-1)]; 
    map(R^1, n, (j,i) -> sum apply(toList(x_0 .. x_(n-1)), x->x^(i+1))))

genericminors = (minorsize,rowsize,colsize) -> (
    -- k by k minors of a generic m by n matrix
    matdim := rowsize * colsize - 1;
    x := getSymbol "x";
    R := ZZ/101[x_0 .. x_matdim];
    gens minors(minorsize,genericMatrix(R,x_0,rowsize,colsize)))

--------------------------------------------
-- test top-level and engine strategies on trivial example
TEST ///
R=QQ[x,y]
S=matrix{{x+y^2,x^2+2*x*y}}
subalgebraBasis(S,PrintLevel=>1)--top level
subalgebraBasis(S,PrintLevel=>1,Strategy=>Engine)
R=QQ[x,y,MonomialOrder=>Lex]
S=matrix{{x+y^2,x^2+2*x*y}}
subalgebraBasis(S,PrintLevel=>1)--top level
subalgebraBasis(S,PrintLevel=>1,Strategy=>Engine)
///

TEST ///
R=QQ[x,y,MonomialOrder=>Lex]
M=matrix{{x+y,x*y,x*y^2}}
assert(subalgebraBasis(M,Limit=>3)==M)
assert(subalgebraBasis(M,Limit=>3,Strategy=>Engine)==M)
///

TEST ///
-- simple inhomog example
kk = ZZ/101
R = kk[a,b,c]
F = matrix{{a+b+c-1, a^2+b^2+c^2-a, a^3+b^3+c^3-b}}
ans = matrix {{a+b+c-1, a*b+a*c+b*c+50*b+50*c, a*b*c+50*b^2+50*b*c+50*c^2-9*b+25*c}}
assert(
     time sagbi(F,Limit=>3)
     ==
     ans)
assert(
     time sagbi(F,Limit=>3,Strategy=>Engine)
     ==
     ans)
///
--------------------------------------------
TEST ///
-- invariants of S3
invariantsSn = (n) -> (
    -- ring of invariants of S_n
    R = ZZ/101[x_0 .. x_(n-1)]; 
    map(R^1, n, (j,i) -> sum apply(toList(x_0 .. x_(n-1)), x->x^(i+1))))

F = invariantsSn 3
ans = matrix {{x_0+x_1+x_2, x_0*x_1+x_0*x_2+x_1*x_2, x_0*x_1*x_2}}
assert(
     time sagbi(F,Limit=>10)
     ==
     ans)
assert(
     time sagbi(F,Limit=>10,Strategy=>Engine)
     ==
     ans)
///
--------------------------------------------
TEST ///
-- invariants of S4
invariantsSn = (n) -> (
    -- ring of invariants of S_n
    R = ZZ/101[x_0 .. x_(n-1)]; 
    map(R^1, n, (j,i) -> sum apply(toList(x_0 .. x_(n-1)), x->x^(i+1))))

F = invariantsSn 4
ans = matrix {{x_0+x_1+x_2+x_3, 
	  x_0*x_1+x_0*x_2+x_1*x_2+x_0*x_3+x_1*x_3+x_2*x_3, 
	  x_0*x_1*x_2+x_0*x_1*x_3+x_0*x_2*x_3+x_1*x_2*x_3, 
	  x_0*x_1*x_2*x_3}}
assert(
     time sagbi(F,Limit=>10)
     ==
     ans)
assert(
     time sagbi(F,Limit=>10,Strategy=>Engine)
     ==
     ans)
///
--------------------------------------------
TEST ///
--generic minors(2,2,10)
genericminors = (minorsize,rowsize,colsize) -> (
    -- k by k minors of a generic m by n matrix
    matdim := rowsize * colsize - 1;
    R = ZZ/101[x_0 .. x_matdim];
    gens minors(minorsize,genericMatrix(R,x_0,rowsize,colsize)))

F = genericminors(2,2,10)
ans = matrix {{x_17*x_18-x_16*x_19,
        x_15*x_18-x_14*x_19,
        x_13*x_18-x_12*x_19,
        x_11*x_18-x_10*x_19,
        x_9*x_18-x_8*x_19,
        x_7*x_18-x_6*x_19,
        x_5*x_18-x_4*x_19,
        x_3*x_18-x_2*x_19,
        x_1*x_18-x_0*x_19,
        x_15*x_16-x_14*x_17,
        x_13*x_16-x_12*x_17,
        x_11*x_16-x_10*x_17,
        x_9*x_16-x_8*x_17,
        x_7*x_16-x_6*x_17,
        x_5*x_16-x_4*x_17,
        x_3*x_16-x_2*x_17,
        x_1*x_16-x_0*x_17,
        x_13*x_14-x_12*x_15,
        x_11*x_14-x_10*x_15,
        x_9*x_14-x_8*x_15,
        x_7*x_14-x_6*x_15,
        x_5*x_14-x_4*x_15,
        x_3*x_14-x_2*x_15,
        x_1*x_14-x_0*x_15,
        x_11*x_12-x_10*x_13,
        x_9*x_12-x_8*x_13,
        x_7*x_12-x_6*x_13,
        x_5*x_12-x_4*x_13,
        x_3*x_12-x_2*x_13,
        x_1*x_12-x_0*x_13,
        x_9*x_10-x_8*x_11,
        x_7*x_10-x_6*x_11,
        x_5*x_10-x_4*x_11,
        x_3*x_10-x_2*x_11,
        x_1*x_10-x_0*x_11,
        x_7*x_8-x_6*x_9,
        x_5*x_8-x_4*x_9,
        x_3*x_8-x_2*x_9,
        x_1*x_8-x_0*x_9,
        x_5*x_6-x_4*x_7,
        x_3*x_6-x_2*x_7,
        x_1*x_6-x_0*x_7,
        x_3*x_4-x_2*x_5,
        x_1*x_4-x_0*x_5,
        x_1*x_2-x_0*x_3}}
assert(
     time sagbi(F,Limit=>100)
     ==
     ans)
assert(
     time sagbi(F,Limit=>100,Strategy=>Engine)
     ==
     ans)
///
--------------------------------------------
TEST ///
--generic minors(2,3,3)
genericminors = (minorsize,rowsize,colsize) -> (
    -- k by k minors of a generic m by n matrix
    matdim := rowsize * colsize - 1;
    R = ZZ/101[x_0 .. x_matdim];
    gens minors(minorsize,genericMatrix(R,x_0,rowsize,colsize)))

F = genericminors(2,3,3)
ans = matrix {{x_5*x_7-x_4*x_8,
        x_2*x_7-x_1*x_8,
        x_5*x_6-x_3*x_8,
        x_4*x_6-x_3*x_7,
        x_2*x_6-x_0*x_8,
        x_1*x_6-x_0*x_7,
        x_2*x_4-x_1*x_5,
        x_2*x_3-x_0*x_5,
        x_1*x_3-x_0*x_4,
        x_2*x_4*x_6*x_8-x_1*x_5*x_6*x_8-x_2*x_3*x_7*x_8+x_0*x_5*x_7*x_8+x_1*x_3*x_8^2-x_0*x_4*x_8^2,
        x_0*x_2*x_4*x_6-x_0*x_1*x_5*x_6-x_0*x_2*x_3*x_7+x_0^2*x_5*x_7+x_0*x_1*x_3*x_8-x_0^2*x_4*x_8}}
assert(
     time sagbi(F,Limit=>100)
     ==
     ans)
assert(
     time sagbi(F,Limit=>100,Strategy=>Engine)
     ==
     ans)
///
--------------------------------------------
TEST ///
--generic minors(2,3,4)
genericminors = (minorsize,rowsize,colsize) -> (
    -- k by k minors of a generic m by n matrix
    matdim := rowsize * colsize - 1;
    R = ZZ/101[x_0 .. x_matdim];
    gens minors(minorsize,genericMatrix(R,x_0,rowsize,colsize)))

F = genericminors(2,3,4)
ans = matrix {{x_8*x_10-x_7*x_11, x_5*x_10-x_4*x_11, x_2*x_10-x_1*x_11, x_8*x_9-x_6*x_11, x_7*x_9-x_6*x_10, x_5*x_9-x_3*x_11, x_4*x_9-x_3*x_10, x_2*x_9-x_0*x_11, x_1*x_9-x_0*x_10, x_5*x_7-x_4*x_8, x_2*x_7-x_1*x_8, x_5*x_6-x_3*x_8, x_4*x_6-x_3*x_7, x_2*x_6-x_0*x_8, x_1*x_6-x_0*x_7, x_2*x_4-x_1*x_5, x_2*x_3-x_0*x_5, x_1*x_3-x_0*x_4, x_5*x_7*x_9*x_11-x_4*x_8*x_9*x_11-x_5*x_6*x_10*x_11+x_3*x_8*x_10*x_11+x_4*x_6*x_11^2-x_3*x_7*x_11^2, x_2*x_7*x_9*x_11-x_1*x_8*x_9*x_11-x_2*x_6*x_10*x_11+x_0*x_8*x_10*x_11+x_1*x_6*x_11^2-x_0*x_7*x_11^2, x_2*x_4*x_9*x_11-x_1*x_5*x_9*x_11-x_2*x_3*x_10*x_11+x_0*x_5*x_10*x_11+x_1*x_3*x_11^2-x_0*x_4*x_11^2, x_2*x_4*x_6*x_11-x_1*x_5*x_6*x_11-x_2*x_3*x_7*x_11+x_0*x_5*x_7*x_11+x_1*x_3*x_8*x_11-x_0*x_4*x_8*x_11, x_3*x_5*x_7*x_9-x_3*x_4*x_8*x_9-x_3*x_5*x_6*x_10+x_3^2*x_8*x_10+x_3*x_4*x_6*x_11-x_3^2*x_7*x_11, x_0*x_5*x_7*x_9-x_0*x_4*x_8*x_9-x_0*x_5*x_6*x_10+x_0*x_3*x_8*x_10+x_0*x_4*x_6*x_11-x_0*x_3*x_7*x_11, x_0*x_2*x_7*x_9-x_0*x_1*x_8*x_9-x_0*x_2*x_6*x_10+x_0^2*x_8*x_10+x_0*x_1*x_6*x_11-x_0^2*x_7*x_11, x_0*x_2*x_4*x_9-x_0*x_1*x_5*x_9-x_0*x_2*x_3*x_10+x_0^2*x_5*x_10+x_0*x_1*x_3*x_11-x_0^2*x_4*x_11, x_2*x_4*x_6*x_8-x_1*x_5*x_6*x_8-x_2*x_3*x_7*x_8+x_0*x_5*x_7*x_8+x_1*x_3*x_8^2-x_0*x_4*x_8^2, x_0*x_2*x_4*x_6-x_0*x_1*x_5*x_6-x_0*x_2*x_3*x_7+x_0^2*x_5*x_7+x_0*x_1*x_3*x_8-x_0^2*x_4*x_8}}
assert(
     time sagbi(F,Limit=>100)
     ==
     ans)
assert(
     time sagbi(F,Limit=>100,Strategy=>Engine)
     ==
     ans)
///
---------------------------------------------
-- Commented out: takes too long right now --
---------------------------------------------
///
--generic minors(2,3,5)
genericminors = (minorsize,rowsize,colsize) -> (
    -- k by k minors of a generic m by n matrix
    matdim := rowsize * colsize - 1;
    R = ZZ/101[x_0 .. x_matdim];
    gens minors(minorsize,genericMatrix(R,x_0,rowsize,colsize)))

F = genericminors(2,3,5)
ans = matrix {{x_11*x_13-x_10*x_14, x_8*x_13-x_7*x_14, x_5*x_13-x_4*x_14, x_2*x_13-x_1*x_14, x_11*x_12-x_9*x_14, x_10*x_12-x_9*x_13, x_8*x_12-x_6*x_14, x_7*x_12-x_6*x_13, x_5*x_12-x_3*x_14, x_4*x_12-x_3*x_13, x_2*x_12-x_0*x_14, x_1*x_12-x_0*x_13, x_8*x_10-x_7*x_11, x_5*x_10-x_4*x_11, x_2*x_10-x_1*x_11, x_8*x_9-x_6*x_11, x_7*x_9-x_6*x_10, x_5*x_9-x_3*x_11, x_4*x_9-x_3*x_10, x_2*x_9-x_0*x_11, x_1*x_9-x_0*x_10, x_5*x_7-x_4*x_8, x_2*x_7-x_1*x_8, x_5*x_6-x_3*x_8, x_4*x_6-x_3*x_7, x_2*x_6-x_0*x_8, x_1*x_6-x_0*x_7, x_2*x_4-x_1*x_5, x_2*x_3-x_0*x_5, x_1*x_3-x_0*x_4, x_8*x_10*x_12*x_14-x_7*x_11*x_12*x_14-x_8*x_9*x_13*x_14+x_6*x_11*x_13*x_14+x_7*x_9*x_14^2-x_6*x_10*x_14^2, x_5*x_10*x_12*x_14-x_4*x_11*x_12*x_14-x_5*x_9*x_13*x_14+x_3*x_11*x_13*x_14+x_4*x_9*x_14^2-x_3*x_10*x_14^2, x_2*x_10*x_12*x_14-x_1*x_11*x_12*x_14-x_2*x_9*x_13*x_14+x_0*x_11*x_13*x_14+x_1*x_9*x_14^2-x_0*x_10*x_14^2, x_5*x_7*x_12*x_14-x_4*x_8*x_12*x_14-x_5*x_6*x_13*x_14+x_3*x_8*x_13*x_14+x_4*x_6*x_14^2-x_3*x_7*x_14^2, x_2*x_7*x_12*x_14-x_1*x_8*x_12*x_14-x_2*x_6*x_13*x_14+x_0*x_8*x_13*x_14+x_1*x_6*x_14^2-x_0*x_7*x_14^2, x_2*x_4*x_12*x_14-x_1*x_5*x_12*x_14-x_2*x_3*x_13*x_14+x_0*x_5*x_13*x_14+x_1*x_3*x_14^2-x_0*x_4*x_14^2, x_5*x_7*x_9*x_14-x_4*x_8*x_9*x_14-x_5*x_6*x_10*x_14+x_3*x_8*x_10*x_14+x_4*x_6*x_11*x_14-x_3*x_7*x_11*x_14, x_2*x_7*x_9*x_14-x_1*x_8*x_9*x_14-x_2*x_6*x_10*x_14+x_0*x_8*x_10*x_14+x_1*x_6*x_11*x_14-x_0*x_7*x_11*x_14, x_2*x_4*x_9*x_14-x_1*x_5*x_9*x_14-x_2*x_3*x_10*x_14+x_0*x_5*x_10*x_14+x_1*x_3*x_11*x_14-x_0*x_4*x_11*x_14, x_2*x_4*x_6*x_14-x_1*x_5*x_6*x_14-x_2*x_3*x_7*x_14+x_0*x_5*x_7*x_14+x_1*x_3*x_8*x_14-x_0*x_4*x_8*x_14, x_6*x_8*x_10*x_12-x_6*x_7*x_11*x_12-x_6*x_8*x_9*x_13+x_6^2*x_11*x_13+x_6*x_7*x_9*x_14-x_6^2*x_10*x_14, x_3*x_8*x_10*x_12-x_3*x_7*x_11*x_12-x_3*x_8*x_9*x_13+x_3*x_6*x_11*x_13+x_3*x_7*x_9*x_14-x_3*x_6*x_10*x_14, x_0*x_8*x_10*x_12-x_0*x_7*x_11*x_12-x_0*x_8*x_9*x_13+x_0*x_6*x_11*x_13+x_0*x_7*x_9*x_14-x_0*x_6*x_10*x_14, x_3*x_5*x_10*x_12-x_3*x_4*x_11*x_12-x_3*x_5*x_9*x_13+x_3^2*x_11*x_13+x_3*x_4*x_9*x_14-x_3^2*x_10*x_14, x_0*x_5*x_10*x_12-x_0*x_4*x_11*x_12-x_0*x_5*x_9*x_13+x_0*x_3*x_11*x_13+x_0*x_4*x_9*x_14-x_0*x_3*x_10*x_14, x_0*x_2*x_10*x_12-x_0*x_1*x_11*x_12-x_0*x_2*x_9*x_13+x_0^2*x_11*x_13+x_0*x_1*x_9*x_14-x_0^2*x_10*x_14, x_3*x_5*x_7*x_12-x_3*x_4*x_8*x_12-x_3*x_5*x_6*x_13+x_3^2*x_8*x_13+x_3*x_4*x_6*x_14-x_3^2*x_7*x_14, x_0*x_5*x_7*x_12-x_0*x_4*x_8*x_12-x_0*x_5*x_6*x_13+x_0*x_3*x_8*x_13+x_0*x_4*x_6*x_14-x_0*x_3*x_7*x_14, x_0*x_2*x_7*x_12-x_0*x_1*x_8*x_12-x_0*x_2*x_6*x_13+x_0^2*x_8*x_13+x_0*x_1*x_6*x_14-x_0^2*x_7*x_14, x_0*x_2*x_4*x_12-x_0*x_1*x_5*x_12-x_0*x_2*x_3*x_13+x_0^2*x_5*x_13+x_0*x_1*x_3*x_14-x_0^2*x_4*x_14, x_5*x_7*x_9*x_11-x_4*x_8*x_9*x_11-x_5*x_6*x_10*x_11+x_3*x_8*x_10*x_11+x_4*x_6*x_11^2-x_3*x_7*x_11^2, x_2*x_7*x_9*x_11-x_1*x_8*x_9*x_11-x_2*x_6*x_10*x_11+x_0*x_8*x_10*x_11+x_1*x_6*x_11^2-x_0*x_7*x_11^2, x_2*x_4*x_9*x_11-x_1*x_5*x_9*x_11-x_2*x_3*x_10*x_11+x_0*x_5*x_10*x_11+x_1*x_3*x_11^2-x_0*x_4*x_11^2, x_2*x_4*x_6*x_11-x_1*x_5*x_6*x_11-x_2*x_3*x_7*x_11+x_0*x_5*x_7*x_11+x_1*x_3*x_8*x_11-x_0*x_4*x_8*x_11, x_3*x_5*x_7*x_9-x_3*x_4*x_8*x_9-x_3*x_5*x_6*x_10+x_3^2*x_8*x_10+x_3*x_4*x_6*x_11-x_3^2*x_7*x_11, x_0*x_5*x_7*x_9-x_0*x_4*x_8*x_9-x_0*x_5*x_6*x_10+x_0*x_3*x_8*x_10+x_0*x_4*x_6*x_11-x_0*x_3*x_7*x_11, x_0*x_2*x_7*x_9-x_0*x_1*x_8*x_9-x_0*x_2*x_6*x_10+x_0^2*x_8*x_10+x_0*x_1*x_6*x_11-x_0^2*x_7*x_11, x_0*x_2*x_4*x_9-x_0*x_1*x_5*x_9-x_0*x_2*x_3*x_10+x_0^2*x_5*x_10+x_0*x_1*x_3*x_11-x_0^2*x_4*x_11, x_2*x_4*x_6*x_8-x_1*x_5*x_6*x_8-x_2*x_3*x_7*x_8+x_0*x_5*x_7*x_8+x_1*x_3*x_8^2-x_0*x_4*x_8^2, x_0*x_2*x_4*x_6-x_0*x_1*x_5*x_6-x_0*x_2*x_3*x_7+x_0^2*x_5*x_7+x_0*x_1*x_3*x_8-x_0^2*x_4*x_8}}
assert(
     time sagbi(F,Limit=>100,PrintLevel=>1)
     ==
     ans)
assert(
     time sagbi(F,Limit=>100,Strategy=>Engine,PrintLevel=>1)
     ==
     ans)
///
--------------------------------------------
TEST ///
-- 'symmetric' quadratic artin ideal in 2x3 variables
kk = ZZ/101
R = kk[symbol a..symbol f]
F = mingens ((ideal(a,b,c))^2 + (ideal(d,e,f))^2 + (ideal(a+d,b+e,c+f))^2)
ans = matrix {{f^2, e*f, d*f, c*f, e^2, d*e, c*e+b*f, b*e, d^2, c*d+a*f, b*d+a*e, a*d, c^2, b*c, a*c, b^2, a*b, a^2, b*f^3, a*f^3, a*e*f^2, a*e^2*f, b^3*f, a*b^2*f, a^2*b*f, a^3*f, a*e^3, a^3*e}}
assert(
     time sagbi(F,Limit=>100,PrintLevel=>1)
     ==
     ans)
assert(
     time sagbi(F,Limit=>100,Strategy=>Engine, PrintLevel=>1)
     ==
     ans)
///
--------------------------------------------
TEST ///
--example with both finite and infinite sagbi bases (infinite one)
kk = ZZ/101
R = kk[symbol x,symbol y]   -- x>y gives infinite, y>x gives finite
F = matrix{{x, x*y-y^2, x*y^2}}
ans = matrix {{x, x*y-y^2, x*y^2, x*y^3+50*y^4, x*y^4, x*y^5-34*y^6, x*y^6, x*y^7+25*y^8, x*y^8, x*y^9+20*y^10, x*y^10, x*y^11-17*y^12, x*y^12, x*y^13-29*y^14, x*y^14, x*y^15-38*y^16, x*y^16, x*y^17-45*y^18, x*y^18, x*y^19+10*y^20, x*y^20, x*y^21-46*y^22, x*y^22, x*y^23+42*y^24, x*y^24, x*y^25+31*y^26, x*y^26, x*y^27+36*y^28, x*y^28, x*y^29-27*y^30}}
assert(
     time sagbi(F,Limit=>30,PrintLevel=>1)
     ==
     ans)
assert(
     time sagbi(F,Limit=>30,Strategy=>Engine)
     ==
     ans)

--     time sagbi(F,Limit=>100,PrintLevel=>1)
///
--------------------------------------------
TEST ///
--example with both finite and infinite sagbi bases (finite one)
kk = ZZ/101
R = kk[symbol y,symbol x]   -- x>y gives infinite, y>x gives finite
F = matrix{{x, x*y-y^2, x*y^2}}
ans = matrix {{x, y^2-y*x, y*x^2}}
assert(
     time sagbi(F,Limit=>1000)
     ==
     ans)
assert(
     time sagbi(F,Limit=>1000,Strategy=>Engine)
     ==
     ans)
///
--------------------------------------------
TEST ///
--same example, with generic change of coordinates
kk = ZZ/101
R = kk[symbol x,symbol y]   -- Change of coordinates (i.e. random term order)
F = matrix{{x, x*y-y^2, x*y^2}}
--G = random(R^1, R^(elements(2:-1)))
G = matrix {{43*x+49*y, -37*x-39*y}}
Coordchange = map(R, R, G)
ans = matrix {{x-20*y, x*y+35*y^2, x*y^2-20*y^3, x*y^3-43*y^4, x*y^4-20*y^5, x*y^5+32*y^6, x*y^6-20*y^7, x*y^7+19*y^8, x*y^8-20*y^9, x*y^9-9*y^10, x*y^10-20*y^11, x*y^11+6*y^12, x*y^12-20*y^13, x*y^13-41*y^14, x*y^14-20*y^15, x*y^15+50*y^16, x*y^16-20*y^17, x*y^17+31*y^18, x*y^18-20*y^19, x*y^19+36*y^20, x*y^20-20*y^21, x*y^21-15*y^22, x*y^22-20*y^23, x*y^23-7*y^24, x*y^24-20*y^25, x*y^25-8*y^26, x*y^26-20*y^27, x*y^27+20*y^28, x*y^28-20*y^29, x*y^29-50*y^30}}
F = Coordchange F
assert(
     time sagbi(F,Limit=>30)
     ==
     ans)
assert(
     time sagbi(F,Limit=>30, Strategy=>Engine)
     ==
     ans)
///
--------------------------------------------
TEST ///
-- invariants of A3, infinite sagbi bases, at least for lex order
-- it is infinite for all term orders.

--invariants of A3, to degree 15
kk = ZZ/101
R = kk[a,b,c]
ans = matrix {{a+b+c, a*b+a*c+b*c, a*b*c, a*b^2+a^2*c+b*c^2, a*b^3+a^3*c+b*c^3, a*b^4+a^4*c+b*c^4, a*b^5+a^5*c+b*c^5, a*b^6+a^6*c+b*c^6, a*b^7+a^7*c+b*c^7, a*b^8+a^8*c+b*c^8, a*b^9+a^9*c+b*c^9, a*b^10+a^10*c+b*c^10, a*b^11+a^11*c+b*c^11, a*b^12+a^12*c+b*c^12, a*b^13+a^13*c+b*c^13, a*b^14+a^14*c+b*c^14}}
F = matrix{{a+b+c, a*b+b*c+c*a, a*b*c, a^2*b+b^2*c+c^2*a}}
assert(
     time sagbi(F,Limit=>15)
     ==
     ans)
assert(
     time sagbi(F,Limit=>15, Strategy=>Engine)
     ==
     ans)
///
--------------------------------------------
TEST ///
--invariants of A3, to degree 30
kk = ZZ/101
R = kk[a,b,c]
ans = matrix {{a+b+c, a*b+a*c+b*c, a*b*c, a*b^2+a^2*c+b*c^2, a*b^3+a^3*c+b*c^3, a*b^4+a^4*c+b*c^4, a*b^5+a^5*c+b*c^5, a*b^6+a^6*c+b*c^6, a*b^7+a^7*c+b*c^7, a*b^8+a^8*c+b*c^8, a*b^9+a^9*c+b*c^9, a*b^10+a^10*c+b*c^10, a*b^11+a^11*c+b*c^11, a*b^12+a^12*c+b*c^12, a*b^13+a^13*c+b*c^13, a*b^14+a^14*c+b*c^14, a*b^15+a^15*c+b*c^15, a*b^16+a^16*c+b*c^16, a*b^17+a^17*c+b*c^17, a*b^18+a^18*c+b*c^18, a*b^19+a^19*c+b*c^19, a*b^20+a^20*c+b*c^20, a*b^21+a^21*c+b*c^21, a*b^22+a^22*c+b*c^22, a*b^23+a^23*c+b*c^23, a*b^24+a^24*c+b*c^24, a*b^25+a^25*c+b*c^25, a*b^26+a^26*c+b*c^26, a*b^27+a^27*c+b*c^27, a*b^28+a^28*c+b*c^28, a*b^29+a^29*c+b*c^29}}
F = matrix{{a+b+c, a*b+b*c+c*a, a*b*c, a^2*b+b^2*c+c^2*a}}
assert(
     time sagbi(F,Limit=>30)
     ==
     ans)
assert(
     time sagbi(F,Limit=>30,Strategy=>Engine)
     ==
     ans)
///
--------------------------------------------
TEST ///
--Invariants of A^1, with a nilpotent action on A^5
x = symbol x
kk = ZZ/101
R = kk[t,x_1..x_5, MonomialOrder=>Lex, Degrees=>{1,5,4,3,2,1}]
ans = matrix {{x_5, t*x_5+x_4, t^2*x_5+2*t*x_4+2*x_3, x_3*x_5+50*x_4^2, t^3*x_5+3*t^2*x_4+6*t*x_3+6*x_2, t*x_3*x_5+50*t*x_4^2-49*x_2*x_5+50*x_3*x_4, t^4*x_5+4*t^3*x_4+12*t^2*x_3+24*t*x_2+24*x_1, x_2*x_5^2-x_3*x_4*x_5+34*x_4^3, x_1*x_5-x_2*x_4-50*x_3^2, t^2*x_3*x_5+50*t^2*x_4^2+3*t*x_2*x_5-t*x_3*x_4+3*x_2*x_4-2*x_3^2, t*x_2*x_5^2-t*x_3*x_4*x_5+34*t*x_4^3+x_2*x_4*x_5-35*x_3^2*x_5+34*x_3*x_4^2, t^3*x_3*x_5+50*t^3*x_4^2-46*t^2*x_2*x_5+49*t^2*x_3*x_4+6*t*x_1*x_5+3*t*x_2*x_4-3*t*x_3^2+6*x_1*x_4-3*x_2*x_3, t^2*x_2*x_5^2-t^2*x_3*x_4*x_5+34*t^2*x_4^3+2*t*x_2*x_4*x_5+31*t*x_3^2*x_5-33*t*x_3*x_4^2-2*x_2*x_3*x_5+2*x_2*x_4^2+33*x_3^2*x_4, t^4*x_3*x_5+50*t^4*x_4^2+6*t^3*x_2*x_5-2*t^3*x_3*x_4+12*t^2*x_1*x_5+6*t^2*x_2*x_4-6*t^2*x_3^2+24*t*x_1*x_4-12*t*x_2*x_3+24*x_1*x_3-18*x_2^2, x_1*x_3*x_5+50*x_1*x_4^2-26*x_2^2*x_5-50*x_2*x_3*x_4-17*x_3^3, t^3*x_2*x_5^2-t^3*x_3*x_4*x_5+34*t^3*x_4^3+3*t^2*x_2*x_4*x_5-4*t^2*x_3^2*x_5+t^2*x_3*x_4^2-6*t*x_2*x_3*x_5+6*t*x_2*x_4^2-2*t*x_3^2*x_4-6*x_2^2*x_5+6*x_2*x_3*x_4+31*x_3^3, x_2^2*x_5^2-2*x_2*x_3*x_4*x_5-33*x_2*x_4^3-44*x_3^3*x_5-34*x_3^2*x_4^2, t^4*x_2*x_5^2-t^4*x_3*x_4*x_5+34*t^4*x_4^3+4*t^3*x_2*x_4*x_5-39*t^3*x_3^2*x_5+35*t^3*x_3*x_4^2-12*t^2*x_2*x_3*x_5+12*t^2*x_2*x_4^2-4*t^2*x_3^2*x_4-16*t*x_1*x_3*x_5+8*t*x_1*x_4^2-12*t*x_2^2*x_5+16*t*x_2*x_3*x_4-8*t*x_3^3-24*x_1*x_2*x_5+8*x_1*x_3*x_4+12*x_2^2*x_4-8*x_2*x_3^2, t*x_2^2*x_5^2-2*t*x_2*x_3*x_4*x_5-33*t*x_2*x_4^3-44*t*x_3^3*x_5-34*t*x_3^2*x_4^2+2*x_1*x_2*x_5^2-2*x_1*x_3*x_4*x_5-33*x_1*x_4^3-x_2^2*x_4*x_5+x_2*x_3^2*x_5-45*x_3^3*x_4, t^5*x_2*x_5^2-t^5*x_3*x_4*x_5+34*t^5*x_4^3+5*t^4*x_2*x_4*x_5+27*t^4*x_3^2*x_5-32*t^4*x_3*x_4^2-20*t^3*x_2*x_3*x_5+20*t^3*x_2*x_4^2+27*t^3*x_3^2*x_4-40*t^2*x_1*x_3*x_5+20*t^2*x_1*x_4^2-30*t^2*x_2^2*x_5+40*t^2*x_2*x_3*x_4-20*t^2*x_3^3-19*t*x_1*x_2*x_5+40*t*x_1*x_3*x_4-41*t*x_2^2*x_4-40*t*x_2*x_3^2+5*x_1^2*x_5-29*x_1*x_2*x_4-16*x_1*x_3^2-12*x_2^2*x_3, t^2*x_2^2*x_5^2-2*t^2*x_2*x_3*x_4*x_5-33*t^2*x_2*x_4^3-44*t^2*x_3^3*x_5-34*t^2*x_3^2*x_4^2+4*t*x_1*x_2*x_5^2-4*t*x_1*x_3*x_4*x_5+35*t*x_1*x_4^3-2*t*x_2^2*x_4*x_5+2*t*x_2*x_3^2*x_5+11*t*x_3^3*x_4+4*x_1*x_2*x_4*x_5-39*x_1*x_3^2*x_5+35*x_1*x_3*x_4^2+2*x_2^2*x_3*x_5-4*x_2^2*x_4^2+37*x_2*x_3^2*x_4+44*x_3^4, t^6*x_2*x_5^2-t^6*x_3*x_4*x_5+34*t^6*x_4^3+4*t^5*x_1*x_5^2+2*t^5*x_2*x_4*x_5-6*t^5*x_3^2*x_5+2*t^5*x_3*x_4^2+20*t^4*x_1*x_4*x_5-30*t^4*x_2*x_3*x_5+10*t^4*x_2*x_4^2+40*t^3*x_1*x_4^2+41*t^3*x_2^2*x_5-19*t^2*x_1*x_2*x_5+19*t^2*x_1*x_3*x_4+41*t^2*x_2^2*x_4+5*t*x_1^2*x_5-48*t*x_1*x_2*x_4+43*t*x_1*x_3^2+29*t*x_2^2*x_3+5*x_1^2*x_4+43*x_1*x_2*x_3+29*x_2^3, t^3*x_2^2*x_5^2-2*t^3*x_2*x_3*x_4*x_5-33*t^3*x_2*x_4^3-44*t^3*x_3^3*x_5-34*t^3*x_3^2*x_4^2+6*t^2*x_1*x_2*x_5^2-6*t^2*x_1*x_3*x_4*x_5+2*t^2*x_1*x_4^3-3*t^2*x_2^2*x_4*x_5+3*t^2*x_2*x_3^2*x_5-34*t^2*x_3^3*x_4+8*t*x_1^2*x_5^2-4*t*x_1*x_2*x_4*x_5-8*t*x_1*x_3^2*x_5+4*t*x_1*x_3*x_4^2+6*t*x_2^2*x_3*x_5-4*t*x_2^2*x_4^2+2*t*x_2*x_3^2*x_4+33*t*x_3^4+8*x_1^2*x_4*x_5-12*x_1*x_2*x_3*x_5-4*x_1*x_2*x_4^2+4*x_1*x_3^2*x_4+6*x_2^3*x_5+33*x_2*x_3^3, t^4*x_2^2*x_5^2-2*t^4*x_2*x_3*x_4*x_5-33*t^4*x_2*x_4^3-44*t^4*x_3^3*x_5-34*t^4*x_3^2*x_4^2+8*t^3*x_1*x_2*x_5^2-8*t^3*x_1*x_3*x_4*x_5-31*t^3*x_1*x_4^3-4*t^3*x_2^2*x_4*x_5+4*t^3*x_2*x_3^2*x_5+22*t^3*x_3^3*x_4+16*t^2*x_1^2*x_5^2-8*t^2*x_1*x_2*x_4*x_5-16*t^2*x_1*x_3^2*x_5+8*t^2*x_1*x_3*x_4^2+12*t^2*x_2^2*x_3*x_5-8*t^2*x_2^2*x_4^2+4*t^2*x_2*x_3^2*x_4-35*t^2*x_3^4+32*t*x_1^2*x_4*x_5-48*t*x_1*x_2*x_3*x_5-16*t*x_1*x_2*x_4^2+16*t*x_1*x_3^2*x_4+24*t*x_2^3*x_5+31*t*x_2*x_3^3+16*x_1^2*x_4^2-48*x_1*x_2*x_3*x_4-46*x_1*x_3^3+24*x_2^3*x_4-12*x_2^2*x_3^2}}
F = matrix{{x_5, 
	  t*x_5+x_4, 
	  t^2*x_5+2*t*x_4+2*x_3, 
	  t^3*x_5+3*t^2*x_4+6*t*x_3+6*x_2,
	  t^4*x_5+4*t^3*x_4+12*t^2*x_3+24*t*x_2+24*x_1}}
assert(
    time sagbi(F,Limit=>30)
    ==
    ans)
assert(
    time sagbi(F,Limit=>30,Strategy=>Engine)
    ==
    ans)
///
--------------------------------------------
TEST ///
--Invariants of A^1, with a nilpotent action on A^3
x = symbol x;
t = symbol t;
kk = ZZ/101
--R = kk[t,x_1,x_2,x_3, MonomialOrder=>Lex];
R = kk[t,x_1..x_3, MonomialOrder=>Lex, Degrees=>{1,3,2,1}]
ans = matrix {{x_3, t*x_3+x_2, t^2*x_3+2*t*x_2+2*x_1, x_1*x_3+50*x_2^2}}
F = matrix{{x_3, 
	  t*x_3+x_2, 
	  t^2*x_3+2*t*x_2+2*x_1}} 
assert(
     time sagbi(F,Limit=>200)
     ==
     ans)
assert(
     time sagbi(F,Limit=>200,Strategy=>Engine)
     ==
     ans)
///
--------------------------------------------
TEST ///
--invariants of SL_2 on V + V + Sym^2(V)
u = symbol u;
v = symbol v;
s = symbol s;
kk = ZZ/101
R = kk[u_1,u_2,v_1,v_2,s_0,s_1,s_2];
ans = matrix {{s_1^2-4*s_0*s_2, u_2*v_1-u_1*v_2, v_2^2*s_0-v_1*v_2*s_1+v_1^2*s_2, u_2*v_2*s_0+50*u_2*v_1*s_1+50*u_1*v_2*s_1+u_1*v_1*s_2, u_2^2*s_0-u_1*u_2*s_1+u_1^2*s_2}}
F = matrix{{u_2*v_1-u_1*v_2,
	  s_1^2-4*s_0*s_2,
	  s_0*u_2^2+s_2*u_1^2-s_1*u_1*u_2,
	  s_0*v_2^2+s_2*v_1^2-s_1*v_1*v_2,
	  2*s_0*u_2*v_2+2*s_2*u_1*v_1-s_1*(u_2*v_1+u_1*v_2)}}
assert(
     time sagbi(F,Limit=>30)
     ==
     ans)
assert(
     time sagbi(F,Limit=>30,Strategy=>Engine)
     ==
     ans)
///
--------------------------------------------
end
--------------------------------------------
TEST ///
-- What is the answer supposed to be??  Which ring to use?
--Invariants of A^1, with a nilpotent action on A^4
x = symbol x
kk = ZZ/101
R = kk[t,x_1 .. x_4, MonomialOrder => Lex]
R = kk[t,x_1..x_4, MonomialOrder=>Lex, Degrees=>{1,4,3,2,1}]
R = kk[t,x_1..x_4, MonomialOrder=>ProductOrder{1,4}, Degrees=>{1,4,3,2,1}]
F = matrix{{x_4, 
	  t*x_4+x_3, 
	  t^2*x_4+2*t*x_3+2*x_2, 
	  t^3*x_4+3*t^2*x_3+6*t*x_2+6*x_1}}
time sagbi(F,Limit=>30)
///
--------------------------------------------
TEST ///
-- The following runs out of memory...
--Invariants of A^1, with a nilpotent action on A^6
x = symbol x
kk = ZZ/101
R = kk[t,x_1..x_6, MonomialOrder=>Lex, Degrees=>{1,6,5,4,3,2,1}]
F = matrix{{x_6, 
	  t*x_6+x_5, 
	  t^2*x_6+2*t*x_5+2*x_4, 
	  t^3*x_6+3*t^2*x_5+6*t*x_4+6*x_3,
	  t^4*x_6+4*t^3*x_5+12*t^2*x_4+24*t*x_3+24*x_2,
	  t^5*x_6+5*t^4*x_5+20*t^3*x_4+60*t^2*x_3+120*t*x_2+120*x_1}}
time sagbi(F,Limit=>30)
///
--------------------------------------------
TEST ///
///
