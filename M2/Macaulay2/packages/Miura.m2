-- -*- coding: utf-8 -*-
newPackage(
	"Miura", 
	Version => "0.2",
	Date => "5 October 2017",
	Authors => {
		{Name => "Joe Suzuki", 
		     Email => "j-suzuki@sigmath.es.osaka-u.ac.jp", 
		     HomePage => "http://suzuki.bayesnet.org"}
	},
	Headline => "arithmetic of Miura curves",
        Keywords => {"Commutative Algebra"},
	DebuggingMode => false
	)

export {"setPolynomialRing", "setQuotientRing", "reduced", "add", "double", "scalarMultiplication"}
exportMutable {"PR", "QR"}

-------------------------------------------------------
-- Methods
-------------------------------------------------------

-- specify its polynomial ring
setPolynomialRing=method()
setPolynomialRing(GaloisField, List, List):=(kk, v, w)->(
if gcd w !=1 then error "-- the weights has gcm>1" ;
if min w <1 then error "-- some weights are not positive" ;
if #v != #w then error "-- the numbers of variables and weights are different" ;
PR=kk[v,MonomialOrder=>{Weights => w, Weights=>toList(1 ..#w) }]
)
setPolynomialRing(List, List):=(v, w)->(
if gcd w !=1 then error "-- the weights has gcm>1" ;
if min w <1 then error "-- some weights are not positive" ;
if #v != #w then error "-- the numbers of variables and weights are different" ;
PR=QQ[v,MonomialOrder=>{Weights=>w, Weights=>toList(1 ..#w) }]
)

-- specify its quotient ring
setQuotientRing=method()
setQuotientRing RingElement := p->QR=PR/ideal p
setQuotientRing List := LL->QR=PR/ideal LL
setQuotientRing Ideal := J->QR=PR/J

-- compute its reduced inverse ideal
inverse(Ideal):=J->(
if ring(J) =!=QR then error "-- expected the same ring";
quotient(ideal (entries gens gb J)#0#0, J)
)

-- compute its reduced ideal
reduced=method()
reduced Ideal := L->inverse inverse L

-- add reduced ideals
add=method()
add(Ideal, Ideal) := (J, K)->(
if ring(J) =!=ring(K) then error "-- expected the same ring" ;
reduced (J*K)
)

-- double reduced ideal 
double=method()
double Ideal :=J ->add(J, J)

-- add reduced ideal multiple times 
scalarMultiplication=method()
scalarMultiplication(Ideal, ZZ) := (J, m)->(
if m<0 then error "--expected a positive integer";
if m==0 then return ideal 1_(ring J);
if even m then return double scalarMultiplication(J, m//2);
return add(double scalarMultiplication(J, (m-1)//2), J)
)

-------------------------------------------------------
--DOCUMENTATION Miura
-------------------------------------------------------
beginDocumentation()

document{
     Key => Miura,
     Headline => "Miura curve arithmetic",
	 PARA {"The Miura package realizes arithmetic on the curves such as hyper-elliptic curves (e.g., y^2=x^5+x+1), C_{ab} curves (e.g., y^3=x^4+2x+1), complete intersection (e.g. {y^2-x^3-1,z^2-x*y-1}).
	 For the Miura form, the pole orders should be specified such as 2 and 3 for x and y of an elliptic curve.  
	 Currently, only divisor class group computation is available for the package. 
	 For the elliptic curves, [(P)-(O)]+[(Q)-(O)] = [(P+Q)-(O)] for two points P, Q and the point O at infinity. 
	 For the general nonsingular curves, any divisor class is uniquely expressed by E-g(O) with E a positive divisor of degree g (genus). 
	 This package reduces the divisor class  group addition to ideal class group multiplication, and utilizes Groebner basis computation. 
	 See http://arxiv.org/pdf/1512.08040v1.pdf for the detail"}
     }
	 
document{
    Key => "setPolynomialRing for GF",
	Inputs => {"kk"=>{"any field"},"v"=>{"a list of variables"},"w"=>{"a list of weights corresponding to", TT "v"} }, 
    Usage => "setPolynomialRing(kk,v,w)",
 	Outputs => {ofClass PolynomialRing},
    Headline => "Set Polynomial Ring for Galois Field given Variables and their Weights",
    PARA {"The function setPolynomialRing generates the polynomial ring kk[v] with the monomial order with respect to weights w over the variables v. 
	The elements in w should be positive and mutually prime. 
	The numbers of elements in v and w should coincide.
	The variable PR keeps the polynomial ring.
	The monomial order is specific in Miura. For example, for an elliptic curve, it is based on the pole order 2i+3j for x^iy^j"},
	EXAMPLE {"setPolynomialRing(GF 7, {x,y},{2,3})"}
}

document{
    Key => "setPolynomialRing for QQ",
	Inputs => {"v"=>{"a list of variables"},"w"=>{"a list of weights corresponding to", TT "v"} }, 
    Usage => "setPolynomialRing(v,w)",
 	Outputs => {ofClass PolynomialRing},
    Headline => "Set Polynomial Ring for QQ given Variables and their Weights",
    PARA {"The function setPolynomialRing generates the polynomial ring kk[v] with the monomial order with respect to weights w over the variables v. 
	The elements in w should be positive and mutually prime. 
	The numbers of elements in v and w coincide.
	The variable PR keeps the polynomial ring.
	The monomial order is specific in Miura. For example, for an elliptic curve, it is based on the pole order 2i+3j for x^iy^j"},
	EXAMPLE {"setPolynomialRing({x,y}, {2,3})"}
}

document{
    Key => "setQuotientRing given RingElement",
	Inputs => {"p"=>{"a generator of the curve"}}, 
    Usage => "setQuotientRing(p)",
 	Outputs => {ofClass Ideal},
    Headline => "Set Quotient Ring given Single Defining Equation",
    PARA {"The function setQuotientRing sets the quotient ring QR of PR over an ideal, where the ideal may be given by defining equations.
	The global variables PR and QR are specified when setting its polynomial ring and quotient ring, respectively."},
	EXAMPLE {"setPolynomialRing(GF 7, {x,y}, {2,3})", "setQuotientRing(y^2-x^3-3*x)"}
}

document{
    Key => "setQuotientRing given List of RingElements",
	Inputs => {"LL"=>{"a list of generators of the curve"}}, 
    Usage => "setQuotientRing(LL)",
 	Outputs => {ofClass Ideal},
    Headline => "Set Quotient Ring given Multiple Defining Equations",
    PARA {"The function setQuotientRing sets the quotient ring QR of PR over an ideal, where the ideal may be given by defining equations.
	The global variables PR and QR are specified when setting its polynomial ring and quotient ring, respectively."},
	EXAMPLE {"setPolynomialRing(GF 5,{x,y,z},{4,6,5})","setQuotientRing({y^2-x^3-1, z^2-x*y-1})"}
}

document{
    Key => "setQuotientRing given Ideal",
	Inputs => {"J"=>{"ideal of the curve"}}, 
    Usage => "setQuotientRing(J)",
 	Outputs => {ofClass Ideal},
    Headline => "Set Quotient Ring given Ideal",
    PARA {"The function setQuotientRing sets the quotient ring QR of PR over an ideal, where the ideal may be given by defining equations.
	The global variables PR and QR are specified when setting its polynomial ring and quotient ring, respectively."},
	EXAMPLE {"setPolynomialRing(GF 7,{x,y},{2,3})","setQuotientRing(ideal(y^2-x^3-3*x))"}
}

document{
    Key => reduced,
	Inputs => {"J" => {"an integral ideal"}},
    Usage => "K=reduced(J)",
    Outputs => {ofClass Ideal},
    Headline => "Compute Reduced Ideal",
    PARA {"The function reduced computes the reduced ideal given an integral ideal by executing inverse twice.
	The reduced ideal is the ideal that minimizes the pole order among the ideals in the same class"},   
	EXAMPLE {"setPolynomialRing({x,y},{2,3})","setQuotientRing(y^2-x^3-7*x)", "J=ideal(x,y); reduced(J)"},
	SeeAlso => {"inverse"}
}

document{
    Key => add,
	Inputs => {"J" => {"an integral ideal"},"K" => {"an integral ideal"}},
    Usage => "L=add(J,K)",
    Outputs => {ofClass Ideal},
    Headline => "Add Reduced Ideals",
    PARA {"The function add computes the reduced ideal of multiplication of two integral ideals.
	Each reduced ideal is a representative of its ideal class, and the addition is executed over the ideal class group."},    
	EXAMPLE {"setPolynomialRing(GF 13, {x,y}, {2,3}); setQuotientRing(y^2-x^3-7*x)","J=ideal(x, y); K=ideal(x-2, y-3); add(J, K)",
	"L=J*K; reduced(L)", "setPolynomialRing(GF 5,{x,y,z},{4,6,5})", "setQuotientRing({y^2-x^3-1, z^2-x*y-1})", "J=ideal(x-2,y-2,z)", "K=ideal(x-4,y,z-1)", "add(J, K)"},
	SeeAlso => {"reduced", "double", "scalarMultiplication"}
}

document{
    Key => double,
	Inputs => {"J" => {"an integral ideal"}},
    Usage => "K=double(J)",
    Outputs => {ofClass Ideal},
    Headline => "Double Reduced Ideal",
    PARA {"The function double computes the reduced ideal of J*J given an integral ideal J.
	double(J) and add(J,J) outputs the same but double(J) is often more useful in practice."},    
	EXAMPLE {"setPolynomialRing({x,y},{2,3}); setQuotientRing(y^2-x^3-7*x); J=ideal(x,y)", "double J", "reduced(J*J)", "add(J,J)"},
	SeeAlso => {"add", "scalarMultiplication"}
}

document{
    Key => scalarMultiplication,
	Inputs => {"J" => {"an integral ideal"}, "m" => {"a nonnegative integer"}},
    Usage => "K=scalarMultiplication(J,m)",
    Outputs => {ofClass Ideal},
    Headline => "Add Reduced Ideal Multiple Times",
    PARA {"The function scalarMultiplication computes the reduced ideal of an integral ideal scalarMultiplicationplied by a nonnegative integer"},    
	EXAMPLE {"setPolynomialRing(GF 13,{x,y},{2,3}); setQuotientRing(y^2-x^3-7*x)","J=ideal(x,y)","scalarMultiplication(J,5)",
	"setPolynomialRing({x,y}, {2,3})", "setQuotientRing(y^2-x^3-7*x)", "J=ideal(x,y)", "K=ideal(x-2,y-3)", "add(J,K)", "scalarMultiplication(K,5)"},
	SeeAlso => {"add", "double"}
}


--------------------------------------------
--TEST
--------------------------------------------

TEST ///
-- Hyper Elliptic Curves over GF 2^3
setPolynomialRing(GF 2^3,{x,y},{2,5});
setQuotientRing(y^2-x^5-1);
J:=ideal(x+1,y);
K:=ideal(x,y-1);
assert(add(J,K)==add(K,J));
A:=add(J,K);
A2:=scalarMultiplication(A,2);
A5:=scalarMultiplication(A,5);
assert(scalarMultiplication(A2,5)==scalarMultiplication(A5,2));
///
TEST ///
-- Hyper Elliptic Curves over QQ
setPolynomialRing({x,y},{2,5});
setQuotientRing(y^2-x^5-1);
J:=ideal(x+1,y);
K:=ideal(x,y-1);
assert(add(J,K)==add(K,J));
A:=add(J,K);
A2:=double(A);
A5:=scalarMultiplication(A,5);
assert(scalarMultiplication(A2,5)==double(A5));
///
TEST ///
--Cab Curves over GF 5
setPolynomialRing(GF 5,{x,y},{3,4});
setQuotientRing(y^3-x^4-1);
J:=ideal(x-2,y-3);
K:=ideal(x-4,y);
assert(add(J,K)==add(K,J));
A:=add(J,K);
A2:=double(A);
A5:=scalarMultiplication(A,5);
assert(scalarMultiplication(A2,5)==double(A5));
///

TEST ///
--general Miura curve over GF 5
setPolynomialRing(GF 5,{x,y,z},{4,6,5});
setQuotientRing({y^2-x^3-1,z^2-x*y-1});
J:=ideal(x-2,y-2,z);
K:=ideal(x-4,y,z-1);
assert(add(J,K)==add(K,J));
A:=add(J,K);
A2:=double(A);
A5:=scalarMultiplication(A,5);
assert(scalarMultiplication(A2,5)==double(A5));
///

