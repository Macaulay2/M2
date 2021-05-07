-- -*- coding: utf-8 -*-
newPackage(
	"EllipticCurves",
    	Version => "0.1", 
    	Date => "May 3, 2014",
    	Authors => {{Name => "Alessandro Oneto", 
		  Email => "oneto@math.su.se"},
	    {Name => "Stefano Marseglia", 
		  Email => "stefanom@math.su.se"}},
	Keywords => {"Algebraic Number Theory"},
    	Headline => "group law on elliptic curves (and counting points with brute force methods)"
    	)

export {
    "EllipticCurve",
    "EllipticCurveW",
    "EllPoint",
    "EllPointW",
    "ellCurve",
    "ellPoint",
    "isElliptic",
    "isOnEllCurve",
    "Discriminant",
    "jInvariant",
    "toShortWForm",
    "ellCurveFromjInv",
    "toWeierstrass",
    "bruteForceRationalPoints",
    "bruteForceGroupOrder"
    }
protect curve
protect coord
protect coeff1
protect coeff2
protect a1
protect a2
protect a3
protect a4
protect a6
protect field
protect coeff

---------------------------------------------
-- New types
---------------------------------------------
EllipticCurve = new Type of HashTable
EllipticCurveW = new Type of HashTable
EllPoint = new Type of HashTable
EllPointW = new Type of HashTable
---------------------------------------------
-- Definying elliptic curves and points
---------------------------------------------
ellCurve = method()
ellCurve(Thing,Thing,Thing) := (A,B,K) ->(
    A= (sub(matrix{{A}},K))_0_0;
    B= (sub(matrix{{B}},K))_0_0;
    if (-16*(4*A^3+27*B^2) == 0) then (
	net (expression "The curve Y^2 = X^3"+ expression A expression "X" + expression B expression" is singular over " expression K expression ".")
	)
     else (new EllipticCurve from hashTable{coeff1=>A,coeff2=>B, field=> K})
     );

ellCurve(List,Thing) := (L,K) ->(
    a:=(sub(matrix{{L#0}},K))_0_0;
    b:=(sub(matrix{{L#1}},K))_0_0;
    c:=(sub(matrix{{L#2}},K))_0_0;
    d:=(sub(matrix{{L#3}},K))_0_0;
    e:=(sub(matrix{{L#4}},K))_0_0;
    b2:=a^2+4*b;
    b4:=2*d+a*c;
    b6:=c^2+4*e;
    b8:=a^2*e+4*b*e-a*c*d+b*c^2-d^2;
    Delta:=-b2^2*b8-8*b4^3-27*b6^2+9*b2*b4*b6;
    if (Delta == 0) then error("The curve is singular over" net K "." ) else (
	new EllipticCurveW from hashTable{a1=>a,a2=>b,a3=>c,a4=>d,a6=>e, field=> K})
    )
isOnEllCurve = method(TypicalValue => Boolean)
isOnEllCurve(List,EllipticCurve) := (P,E)->(
    if (P#0==0 and P#1!=0 and P#2==0) then true else (P#1^2*P#2==P#0^3+(E.coeff1)*P#0*P#2^2+(E.coeff2)*P#2^3)
    )
isOnEllCurve(List,EllipticCurveW) := (P,E)->(
    if (P#0==0 and P#1!=0 and P#2==0) then true else (P#1^2*P#2+E.a1*P#0*P#1*P#2+E.a3*P#1*P#2^2==P#0^3+E.a2*P#0^2*P#2+E.a4*P#0*P#2^2+E.a6*P#2^3)
    )

ellPoint = method()
ellPoint(List,EllipticCurve) := (L,E)->(
    x:=(sub(matrix{{L#0}},E.field))_0_0;
    y:=(sub(matrix{{L#1}},E.field))_0_0;
    z:=(sub(matrix{{L#2}},E.field))_0_0;
    if (not isOnEllCurve({x,y,z},E)) then (
	net (expression "The point " expression {x,y,z} expression " is not on the" expression E expression ".") 
	)
    else (
	if (x==0 and z==0) then (new EllPoint from hashTable{coord=>{0,1,0},curve=>E})
	else (new EllPoint from hashTable{coord=>{x/z,y/z,1},curve=>E})
	)
    )
ellPoint(List,EllipticCurveW) := (L,E)->(
    x:=(sub(matrix{{L#0}},E.field))_0_0;
    y:=(sub(matrix{{L#1}},E.field))_0_0;
    z:=(sub(matrix{{L#2}},E.field))_0_0;
    if (not isOnEllCurve({x,y,z},E)) then (
	net (expression "The point " expression {x,y,z} expression " is not on the" expression E expression ".") 
	)
    else (
	if (x==0 and z==0) then (new EllPointW from hashTable{coord=>{0,1,0},curve=>E})
	else (new EllPointW from hashTable{coord=>{x/z,y/z,1},curve=>E})
	)
    )
---------------------------------------------
-- Printing for elliptic curves and points
---------------------------------------------
expression(EllipticCurve):= E->(
    expression "Elliptic Curve defined by Y^2 = X^3"+ expression(E.coeff1) expression "X"+ expression E.coeff2 expression " over" expression E.field expression "."
    )
expression(EllipticCurveW):= E->(
    expression "Elliptic Curve defined by Y^2" + expression E.a1 expression "XY" + expression E.a3 expression "Y = X^3"+ expression E.a2 expression "X^2"+ expression E.a4 expression "X" + expression E.a6 expression " over" expression E.field expression "."
    );

net(EllipticCurve) := E-> net expression E
net(EllipticCurveW) := E-> net expression E

expression(EllPoint):= P->(
    expression "Point " expression P.coord expression " on the " expression P.curve
    )

net(EllPoint) := P-> net expression P

expression(EllPointW):= P->(
    expression "Point " expression P.coord expression " on the " expression P.curve
    )

net(EllPointW) := P-> net expression P
---------------------------------------------
-- Auxiliary functions
---------------------------------------------

--given an ideal, this function checks if it it defines an elliptic curve, i.e. if it is 1 dimensional and it's non-singular.

isElliptic = method(TypicalValue => Boolean);
isElliptic Ideal := J -> (
    I := J;
    test := false;
    S := ring J;
    hvar := symbol hvar;
    KK := coefficientRing S;
    if not isHomogeneous J then (
	S = KK[first entries vars S , hvar];
	I = homogenize(sub(J,S),hvar);
	);
    test = (dim I - 1) == 1 and saturate ideal singularLocus I == sub(ideal(1),S) and rank HH^1(cotangentSheaf Proj(S/I)) == 1;
    if (dim I - 1) != 1 then (<< "It has to be a curve" << endl);
    if saturate ideal singularLocus I != sub(ideal(1),S) then (<< "The curve has to be smooth" << endl);
    if rank HH^1(cotangentSheaf Proj(S/I)) != 1 then (<< "The curve has to be of genus 1" << endl);
    return test;
  )

Discriminant = method()
Discriminant EllipticCurve := E -> -16*(4*E.coeff1^3+27*E.coeff2^2)
Discriminant EllipticCurveW := E -> (
    b2:=E.a1^2+4*E.a2;
    b4:=2*E.a4+E.a1*E.a3;
    b6:=E.a3^2+4*E.a6;
    b8:=E.a1^2*E.a6+4*E.a2*E.a6-E.a1*E.a3*E.a4+E.a2*E.a3^2-E.a4^2;
    -b2^2*b8-8*b4^3-27*b6^2+9*b2*b4*b6
    )

toShortWForm = method(TypicalValue => EllipticCurve)
toShortWForm EllipticCurveW := E ->(
    if ((char E.field ==2) or (char E.field ==3)) then (error "The curve can't be written in short Weierstrass form.")
	else (
	    b2:=E.a1^2+4*E.a2;
	    b4:=2*E.a4+E.a1*E.a3;
    	    b6:=E.a3^2+4*E.a6;
    	    b8:=E.a1^2*E.a6+4*E.a2*E.a6-E.a1*E.a3*E.a4+E.a2*E.a3^2-E.a4^2; 
    	    c4:=b2^2-24*b4;
    	    c6:=-b2^3+36*b2*b4-216*b6;
    	    ellCurve(-27*c4,-54*c6,E.field)
	    )
    )

jInvariant = method()
jInvariant EllipticCurve := E -> (1728*4*E.coeff1^3)/(4*E.coeff1^3+27*E.coeff2^2)
jInvariant EllipticCurveW := E -> (
    b2:=E.a1^2+4*E.a2;
    b4:=2*E.a4+E.a1*E.a3;
    b6:=E.a3^2+4*E.a6;
    b8:=E.a1^2*E.a6+4*E.a2*E.a6-E.a1*E.a3*E.a4+E.a2*E.a3^2-E.a4^2;
    c4:=b2^2-24*b4;
    c4^3/Discriminant(E)
    )

ellCurveFromjInv = method(TypicalValue=>EllipticCurveW)
ellCurveFromjInv Thing := j-> (
    K:= ring j;
    
    if (j==0 and char K == 3) then ellCurve(1,0, K) else (
	if (j==0 and char K == 2) then ellCurve({0,0,1,0,0}, K) else (
    	    if (j==0) then ellCurve({0,0,1,0,0}, K) else (
		if (j==1728) then ellCurve(1,0, K) else (
	    	    ellCurve({1,0,0,-36/(j-1728),-1/(j-1728)}, ring (-1/(j-1728)))
	    	    )
		)
	    )
	)
    )

---------------------------------------------
-- Binary and unary operations, group law
---------------------------------------------

EllipticCurve == EllipticCurve := (E,F)->(
    (E.coeff1===F.coeff1 and E.coeff2===F.coeff2)
    )

EllipticCurveW == EllipticCurveW := (E,F)->(
    (E.a1===F.a1 and E.a2===F.a2 and E.a3===F.a3 and E.a4===F.a4 and E.a6===F.a6)
    )

EllPoint == EllPoint := (P,Q)->(
    (P.curve == Q.curve and P.coord===Q.coord)
    )

EllPointW == EllPointW := (P,Q)->(
    (P.curve == Q.curve and P.coord===Q.coord)
    )

EllPoint + EllPoint := (P,Q)->(
    if (not P.curve==Q.curve) then (<< "The points " << P.coord << " and " << Q.coord <<"are not on the same curve."<< endl )
     else (
	 lam:=0;
	 if P.coord=={0,1,0} then Q else (
	     if Q.coord=={0,1,0} then P else (
	    	 if ((P.coord#0==Q.coord#0) and (P.coord#1==-Q.coord#1)) then ellPoint({0,1,0},P.curve) else (
		     if ((P.coord#0==Q.coord#0)) then (lam=(3*(P.coord#0)^2+P.curve.coeff1)/(2*P.coord#1));
		     if ((P.coord#0!=Q.coord#0)) then (lam=(Q.coord#1-P.coord#1)/(Q.coord#0-P.coord#0));
		     xS:=lam^2-(P.coord#0)-(Q.coord#0);
		     yS:=lam*((P.coord#0)-xS)-(P.coord#1);
		     ellPoint({xS,yS,1},P.curve)
		     )
	    	 )
	     )
    	 )
     )

-EllPoint := (P) -> (
    if P.coord=={0,1,0} then ellPoint({0,1,0},P.curve) else ellPoint({P.coord#0,-P.coord#1,1},P.curve)
    )

EllPoint - EllPoint := (P,Q)->P+(-Q)

--given n the function returns the binary expansion: n=u_0+2u_1+4u_2+... u_i=0,1.
binaryExp = n -> (
    u:={};
    u=(while (n!=0) list (n%2) do (n=floor(n/2)))
    )

ZZ*EllPoint := (n,P)-> (
    if n==0 then (ellPoint({0,1,0},P.curve)) else (	
	L:= Symbol;
	S:=ellPoint({0,1,0},P.curve);	
    if n>0 then (
	L=binaryExp(n);
    	S=ellPoint({0,1,0},P.curve);
    	for i to (length(L)-1) do (
	    if (L#i)==1 then S=S+P;
	    P=P+P;
	    );
    	S
	)
    else (
	 n=-n;
	 P=-P;
	 L=binaryExp(n);
    	 S=ellPoint({0,1,0},P.curve);
    	 for i to (length(L)-1) do (
	     if (L#i)==1 then S=S+P;
	     P=P+P;
	     );
    	 S
	 )
     )
     )
 
EllPoint*ZZ := (P,n)->n*P

EllPointW + EllPointW := (P,Q) ->(
    if (not P.curve==Q.curve) then (<< "The points " << P.coord << " and " << Q.coord <<"are not on the same curve."<< endl )
    else (
	 lam:=0;
    	 mu:=0;
    	 a1:=P.curve.coeff#0;
    	 a3:=P.curve.coeff#2;
    	 a2:=P.curve.coeff#1;
    	 a4:=P.curve.coeff#3;
    	 a6:=P.curve.coeff#4;
    	 if P.coord=={0,1,0} then Q else (
	     if Q.coord=={0,1,0} then P else (
	    	 if ((P.coord#0==Q.coord#0) and (P.coord#1+Q.coord#1+a1*Q.coord#0+a3==0)) then {0,1,0} else (
		     if (P.coord#0!=Q.coord#0) then (
		    	 lam=(Q.coord#1-P.coord#1)/(Q.coord#0-P.coord#0);
		    	 mu=(P.coord#1*Q.coord#0-P.coord#0*Q.coord#1)/(Q.coord#0-P.coord#0);
		    	 );
		     if (P.coord#0==Q.coord#0) then (
		    	 lam=(3*P.coord#0^2+2*a2*P.coord#0+a4-a1*P.coord#1)/(2*P.coord#1+a1*P.coord#0+a3);
		    	 mu=(-P.coord#0^3+a4*P.coord#0+2*a6-a3*P.coord#1)/(2*P.coord#1+a1*P.coord#0+a3);
		    	 );
		     xS:=lam^2-a1*lam-a2-(P.coord#0)-(Q.coord#0);
		     yS:=-(lam+a1)*xS-mu-a3;
		     ellPoint({xS,yS,1},P.curve)
		     )
	    	 )
	     )
    	 )
    )

-EllPointW := P -> (
    if P.coord=={0,1,0} then ellPoint({0,1,0},P.curve) else ellPoint({P.coord#0,-P.coord#1-a1*P.coord#0-a3,1},P.curve)
    )

EllPointW-EllPointW := (P,Q)-> P+(-Q)

ZZ*EllPointW := (n,P)-> (
    if n==0 then (ellPoint({0,1,0},P.curve)) else (
	L:= Symbol;
	S:=ellPoint({0,1,0},P.curve);	
    if n>0 then (
	L=binaryExp(n);
    	for i to (length(L)-1) do (
	    if (L#i)==1 then S=S+P;
	    P=P+P;
	    );
    	S
	)
    else (
	 n=-n;
	 P=-P;
	 L=binaryExp(n);
    	 S=ellPoint({0,1,0},P.curve);
    	 for i to (length(L)-1) do (
	     if (L#i)==1 then S=S+P;
	     P=P+P;
	     );
    	 S
	 )
     )   
    )

EllPointW*ZZ := (P,n)->n*P

---------------------------------------------
-- Nagell's algorithm
---------------------------------------------
toWeierstrass = method();
toWeierstrass (List,List,Thing) := (L,O,K) -> (
    -- check on inputs:
    -- 1) coefficients in the same field assigned;
    -- 2) coefficients of a quadric and a point in the plane;
    -- 3) the point has to be on the curve.
    G := 0;
    x := symbol x;
    y := symbol y;
    z := symbol z;
    L = apply(L,i -> sub(i,K));
    O = apply(O,i -> sub(i,K));
    if (not (#L == 10) or not (#O == 3) or (O == {sub(0,K),sub(0,K),sub(0,K)})) then (
	(<< "Wrong inputs." << endl);
	) else (
	S := K[x,y,z];
	M := basis(3,S);
	F := 0;
	for i in 0..9 do F = F + L#i*(first entries M_i);
	if not (sub(F,{x => O#0, y => O#1, z => O#2}) == sub(0,K)) then (
		(<< "The point is not on the curve" << endl);
		) else (
		if (dim(singularLocus(S/ideal(F))) > 0) then (
		    (<< "The curve is singular" << endl);
		    ) else (
		    -- we assume O#1 != 0
		    if (O#1 == 0) then (
			if (O#0 == 0) then (
			    F = sub(F,{x => x, y => z, z => y});
			    O = {O#0,O#2,O#1};
			) else (
			    F = sub(F,{x => y, y => x, z => z});
			    O = {O#1,O#0,O#2};
			);
		    );
		F = sub(F,{ x => x*O#1+y*O#0, y => y*O#1, z => z*O#1+y*O#2});
		-- extract coefficients of x*y^2, y^2*z;
		D := coefficients(F , Monomials => {x*y^2,y^2*z});
		dd := apply(entries D#1 , i -> first i);
		if dd#0 == 0 then (
			F = sub(F,{x => z, y => y, z => x});
		) else (
			F = sub(F,{x => dd#0*x-dd#1*z, y => dd#0*y, z => dd#0*z});
		);
		-- extract coefficients of y*z^2, z^3;
		C := coefficients(F , Monomials => {y*z^2,z^3});
		cc := apply(entries C#1 , i -> first i);
		-- when O is a flex
		if cc#0 == 0 then (
			F = sub(F,{x => 1, y => y, z => x});
			F = sub(F/(first first entries (coefficients(F, Monomials => {y^2}))#1),S);
			G = {sub(contract(y,F),{y => 0})};
			G = append(G,sub(F,{y => 0}));
		) else (
			F = sub(F,{x => cc#0*x, y => cc#0*y-cc#1*z, z => cc#0*z});
			ff := {sub(sub(contract(z^2,F),{z => 0}),{x => 1, y => x, z => 1})};
			ff = append(ff,sub(sub(contract(z,F),{z => 0}),{x => 1, y => x, z => 1}));
			ff = append(ff,sub(sub(F,{z => 0}),{x => 1, y => x, z => 1}));
			G = {ff#1};
			G = append(G,-ff#0*ff#2);
		);
	    );
	u := leadCoefficient(G#1);
	a1 := sub(contract(x,G#0),{x => 0});
	a3 := u*sub(G#0,{x => 0});
	a2 := sub(contract(x^2,G#1),{x => 0});
	a4 := u*sub(contract(x,G#1),{x => 0});
	a6 := u^2*sub(G#1,{x => 0});
	return ellCurve({a1,a2,a3,a4,a6},K);
	);
    );
);

---------------------------------------------
-- Counting Points, brute force methods
---------------------------------------------
bruteForceRationalPoints = method()
bruteForceRationalPoints(EllipticCurve) := E -> (
    g:=first gens E.field;
    d:=#first entries basis ambient E.field;
    n:=(char E.field)^d;
    L:=append(apply(n-1,i->g^i),0);
    S:={ellPoint({0,1,0},E)};
    for i to (n-1) do (
    	for j to (n-1) do(
	    if (isOnEllCurve({L#i,L#j,1},E)) then (S=append(S,ellPoint({L#i,L#j,1},E)));
	    );
	);
    S
    )
bruteForceRationalPoints(EllipticCurveW) := E -> (
    g:=first gens E.field;
    d:=#first entries basis ambient E.field;
    n:=(char E.field)^d;
    L:=append(apply(n-1,i->g^i),0);
    S:={ellPoint({0,1,0},E)};
    for i to (n-1) do (
    	for j to (n-1) do(
	    if (isOnEllCurve({L#i,L#j,1},E)) then (S = append(S,ellPoint({L#i,L#j,1},E)));
	    );
	);
    S
    )

bruteForceGroupOrder = method()
bruteForceGroupOrder(EllipticCurve) := E -> (
    K:=E.field;
    --I don't like the next line!
    if (not instance(E.field,GaloisField)) then (K=GF(char(E.field)));
    g:=first gens K;
    d:=#first entries basis ambient K;
    n:=(char K)^d;
    S:={1};
    apply(ceiling((n-1)/2),i->S=append(S,(last S)*g^2));
    counter:=1;
    for i from 0 to (n-2) do (
	if (member(g^(3*i)+E.coeff1*g^i+E.coeff2, S)) then counter=counter+2;
	if (g^(3*i)+E.coeff1*g^i+E.coeff2==0) then counter=counter+1;
	);  
    if member(E.coeff2,S) then counter=counter+2;
    counter
    )
bruteForceGroupOrder(EllipticCurveW) := E -> (
    K:=E.field;
    if ((not char K==2) and (not char K==3)) then (bruteForceGroupOrder toShortWForm E) else (
    	if (char K == 2) then error "This method doesn't work in characteristic 2." else (
	    --I don't like the next line!
    	    if (not instance(E.field,GaloisField)) then (K=GF(char(E.field)));
    	    g:=first gens K;
    	    d:=#first entries basis ambient K;
    	    n:=(char K)^d;
	    b2:=E.a1^2+4*E.a2;
    	    b4:=2*E.a4+E.a1*E.a3;
    	    b6:=E.a3^2+4*E.a6;
	    S:={1};
    	    apply(ceiling((n-1)/2),i->S=append(S,(last S)*g^2));
    	    counter:=1;
    	    for i from 0 to (n-2) do (
	    	if (member(4*g^(3*i)+b2*g^(2*i)+2*b4*g^i+b6, S)) then counter=counter+2;
	    	if (4*g^(3*i)+b2*g^(2*i)+2*b4*g^i+b6==0) then counter=counter+1;
	    	);  
    	    if member(b6,S) then counter=counter+2;
    	    counter
	    )
	)
    )

---------------------------------------------
-- Documentation
---------------------------------------------
beginDocumentation()

document {
     Key => EllipticCurves,
     Headline => "elliptic curves and a brute-force method for point counting",
     }

document {
    Key => EllipticCurve,
    Headline => "The class of elliptic curves in short Weierstrass form.",
    }

document {
    Key => EllipticCurveW,
    Headline => "The class of elliptic curves in Weierstrass form.",
    }

document {
    Key => EllPoint,
    Headline => "The class of points of elliptic curves in short Weierstrass form.",
    }

document {
    Key => EllPointW,
    Headline => "The class of points of elliptic curves in Weierstrass form.",
    }

document {
     Key => ellCurve,
     Headline => "A method for creating elliptic curves.",
     }

document {
    Key => (ellCurve,Thing,Thing,Thing),
    Inputs => {"A"=>{"a coefficient in the field ", TT "K"}, "B"=>{"a coefficient in the field ", TT "K"}, "K"=>{"any field"}},
    Outputs => {ofClass EllipticCurve},
    Usage => "ellCurve(A,B,K)",
    Headline => "A method for creating elliptic curves in short Weierstrass form.",
    PARA {"Returns an elliptic curve in short Weierstrass form given by the equation: y^2=x^3+", TT "A","x+",TT "B"},
    EXAMPLE {"ellCurve(3,8,GF(13))"}
    }
document {
    Key => (ellCurve,List,Thing),
    Inputs => {"L"=>{"a list of five coefficients in the field ", TT "K"}, "K"=>{"any field"}},
    Outputs => {ofClass EllipticCurveW},
    Usage => "ellCurve(L,K)",
    Headline => "A method for creating elliptic curves in Weierstrass form.",
    PARA {"Returns an elliptic curve in Weierstrass with coefficients from the list."},
    EXAMPLE {"ellCurve({1,2,3,4,6},GF(29))","ellCurve({0,-1,1,-10,-20},QQ)"}
    }
document {
     Key => ellPoint,
     Headline => "A method for defining point on an elliptic curve.",
     }
document {
    Key => (ellPoint,List,EllipticCurve),
    Inputs => {"L"=>{"the projective coordinates of the point"}, "E"=>{"the elliptic curve on which the point lies"}},
    Outputs => {ofClass EllPoint},
    Usage => "ellPoint(L,E)",
    Headline => "A method for defining point on an elliptic curve.",
    PARA {"Returns the point of projecive coordinates { ", TT "L#0"," : ", TT "L#1"," : ", TT "L#2"," } on the elliptic curve ", TT "E"},
    EXAMPLE {"E=ellCurve(3,8,GF(13))","P=ellPoint({1,5,1},E)"}
    }
document {
    Key => (ellPoint,List,EllipticCurveW),
    Inputs => {"L"=>{"the projective coordinates of the point"}, "E"=>{"the elliptic curve on which the point lies"}},
    Outputs => {ofClass EllPointW},
    Usage => "ellPoint(L,E)",
    Headline => "A method for defining point on an elliptic curve.",
    PARA {"Returns the point of projective coordinates { ", TT "L#0"," : ", TT "L#1"," : ", TT "L#2"," } on the elliptic curve ", TT "E"},
    EXAMPLE {"E=ellCurve({1,2,3,4,6},GF(29))","P=ellPoint({1,5,1},E)"}
    }
document {
     Key => isOnEllCurve,
     Headline => "A method to check if a point is on an elliptic curve.",
     }
document {
    Key => (isOnEllCurve,List,EllipticCurve),
    Inputs => {"L"=>{"the projective coordinates of the point"}, "E"=>{"the elliptic curve on which the point lies"}},
    Usage => "isOnEllCurve(L,E)",
    Headline => "A method to check if a point is on an elliptic curve.",
    PARA {"Tells whether the point of projecive coordinates { ", TT "L#0"," : ", TT "L#1"," : ", TT "L#2"," } is on the elliptic curve ", TT "E"},
    EXAMPLE {"E=ellCurve(3,8,GF(13))","isOnEllCurve({1,5,1},E)"}
    }
document {
    Key => (isOnEllCurve,List,EllipticCurveW),
    Inputs => {"L"=>{"the projective coordinates of the point"}, "E"=>{"the elliptic curve on which the point lies"}},
    Usage => "isOnEllCurve(L,E)",
    Headline => "A method to check if a point is on an elliptic curve.",
    PARA {"Tells whether the point of projecive coordinates { ", TT "L#0"," : ", TT "L#1"," : ", TT "L#2"," } is on the elliptic curve ", TT "E"},
    EXAMPLE {"E=ellCurve({1,2,3,4,6},GF(29))","isOnEllCurve({1,5,1},E)"}
    }
document {
    Key => {isElliptic,(isElliptic,Ideal)},
    Inputs => {"I"=>{ofClass Ideal}},
    Usage => "isElliptic(I)",
    Headline => "A method to check if an ideal defines an elliptic curve.",
    PARA {"Given a non-homogeneous ideal in two variables check if it represents a "},
    EXAMPLE {"S=ZZ/7[x,y]","I=ideal(y^2-x^3-x-1)","isElliptic(I)"},
    }
document {
     Key => Discriminant,
     Headline => "A method to compute the discriminant of an elliptic curve.",
     }
document {
    Key => (Discriminant,EllipticCurve),
    Inputs => {"E"},
    Usage => "Discriminant(E)",
    Headline => "A method to compute the discriminant of an elliptic curve.",
    EXAMPLE {"E=ellCurve(3,8,GF(13))","Discriminant(E)"}
    }
document {
    Key => (Discriminant,EllipticCurveW),
    Inputs => {"E"},
    Usage => "Discriminant(E)",
    Headline => "A method to compute the discriminant of an elliptic curve.",
    EXAMPLE {"E=ellCurve({1,2,3,4,6},GF(29))","Discriminant(E)"}
    }
document {
    Key => {toShortWForm,(toShortWForm,EllipticCurveW)},
    Inputs => {"E"},
    Usage => "toShortWForm(E)",
    Headline => "A method to transform an elliptic curve from Weierstrass form to short Weierstrass form.",
    EXAMPLE {"E=ellCurve({1,2,3,4,6},GF(29))","toShortWForm(E)"}
    }
document {
    Key => {toWeierstrass,(toWeierstrass,List,List,Thing)},
    Inputs => {"L"=>{"the coefficients of a plane cubic."}, "O"=>{"the coordinates of a point over the curve."}, "K"=>{"any field"}},
    Outputs => {ofClass EllipticCurve},
    Usage => "toWeierstrass(List,List,Thing)",
    Headline => "A method to transform a smooth plane cubic into Weierstrass form.",
    PARA{"The method use the a modified version Nagell's algorithm which works in any characteristic."},
    PARA{"Reference: Tibouchi, M. ''A Nagell Algorithm in Any Characteristic'', Cryptography and Security: From Theory to Applications, 474--479, 2012."},
    EXAMPLE {"toWeierstrass({-1,0,+1,0,0,+1,0,-1,0,+2},{1,2,1},ZZ/13)"}
    }
document {
     Key => jInvariant,
     Headline => "A method to compute the j-invariant of an elliptic curve.",
     }
document {
    Key => (jInvariant,EllipticCurve),
    Inputs => {"E"},
    Usage => "jInvariant(E)",
    Headline => "A method to compute the j-invariant of an elliptic curve.",
    EXAMPLE {"E=ellCurve(3,8,GF(13))","jInvariant(E)"}
    }
document {
    Key => (jInvariant,EllipticCurveW),
    Inputs => {"E"},
    Usage => "jInvariant(E)",
    Headline => "A method to compute the j-invariant of an elliptic curve.",
    EXAMPLE {"E=ellCurve({1,2,3,4,6},GF(29))","jInvariant(E)"}
    }
document {
    Key => {(ellCurveFromjInv),(ellCurveFromjInv,Thing)},
    Inputs => {"j"},
    Usage => "ellCurveFromjInv(j)",
    Headline => "A method to define an elliptic curve from a given j-invariant.",
    EXAMPLE {"ellCurveFromjInv(46/3)"}
    }
document {
    Key=>{(symbol +,EllPoint,EllPoint),(symbol +,EllPointW,EllPointW)},
    Inputs => {"P","Q"},
    Outputs => {ofClass EllPoint},
    Usage => "P+Q",
    PARA{"Computes the sum of two points."},
    EXAMPLE {"E=ellCurve(3,8,GF(13))","P=ellPoint({1,5,1},E)","Q=ellPoint({2,3,1},E)","P+Q"}
    }
document {
    Key=>{(symbol -,EllPoint,EllPoint),(symbol -,EllPointW,EllPointW)},
    Inputs => {"P","Q"},
    Outputs => {ofClass EllPoint},
    Usage => "P-Q",
    PARA{"Computes the difference of two points."},
    EXAMPLE {"E=ellCurve(3,8,GF(13))","P=ellPoint({1,5,1},E)","Q=ellPoint({2,3,1},E)","P-Q"}
    }
document {
    Key=>{(symbol -,EllPoint),(symbol -,EllPointW)},
    Inputs => {"P"},
    Outputs => {ofClass EllPoint},
    Usage => "-P",
    PARA{"Computes the inverse of a point."},
    EXAMPLE {"E=ellCurve(3,8,GF(13))","P=ellPoint({1,5,1},E)","-P"}
    }
document {
    Key=>{(symbol *,ZZ,EllPoint),(symbol *,EllPoint,ZZ),(symbol *,ZZ,EllPointW),(symbol *,EllPointW,ZZ)},
    Inputs => {"n","P"},
    Outputs => {ofClass EllPoint},
    Usage => "n*P",
    PARA{"Computes the multiple of a point using the double and add algorithm."},
    EXAMPLE {"E=ellCurve(3,8,GF(13))","P=ellPoint({1,5,1},E)","4*P"}
    }
document {
    Key => bruteForceRationalPoints,
    Headline => "A brute-force method to list the points on an elliptic curve.",
    }
document {
    Key => (bruteForceRationalPoints,EllipticCurve),
    Inputs => {"E"},
    Usage => "bruteForceRationalPoints(E)",
    Headline => "A brute-force method to list the points on an elliptic curve.",
    EXAMPLE {"E=ellCurve(3,8,GF(13))","bruteForceRationalPoints(E)"}
    }
document {
    Key => (bruteForceRationalPoints,EllipticCurveW),
    Inputs => {"E"},
    Usage => "bruteForceRationalPoints(E)",
    Headline => "A brute-force method to list the points on an elliptic curve.",
    EXAMPLE {"E=ellCurve({1,2,3,4,6},GF(29))","bruteForceRationalPoints(E)"}
    }
document {
     Key => bruteForceGroupOrder,
     Headline => "A brute-force method to count the points on an elliptic curve in characteristic different from 2.",
     }
document {
    Key => (bruteForceGroupOrder,EllipticCurve),
    Inputs => {"E"},
    Usage => "bruteForceGroupOrder(E)",
    Headline => "A brute-force method to count the points on an elliptic curve in characteristic different from 2.",
    EXAMPLE {"E=ellCurve(3,8,GF(13))","bruteForceGroupOrder(E)"}
    }
document {
    Key => (bruteForceGroupOrder,EllipticCurveW),
    Inputs => {"E"},
    Usage => "bruteForceGroupOrder(E)",
    Headline => "A brute-force method to count the points on an elliptic curve in characteristic different from 2.",
    EXAMPLE {"E=ellCurve({1,2,3,4,6},GF(29))","bruteForceGroupOrder(E)"}
    }
document {
    Key=>{(net,EllipticCurve),(net,EllipticCurveW),(expression,EllipticCurve),(expression,EllipticCurveW)},   
    PARA{"Prints nicely an elliptic curve."},
    }
document {
    Key=>{(net,EllPoint),(net,EllPointW),(expression,EllPoint),(expression,EllPointW)},   
    PARA{"Prints nicely a point on an elliptic curve."},
    }
document {
    Key=>{(symbol ==,EllPoint,EllPoint),(symbol ==,EllPointW,EllPointW)},
    PARA{"Checks if two points are the same."},
    }
document {
    Key=>{(symbol ==,EllipticCurve,EllipticCurve),(symbol ==,EllipticCurveW,EllipticCurveW)},
    PARA{"Checks if two elliptic curves are the same."},
    }

--------------------------------------------
--TEST
--------------------------------------------
TEST ///
E:=ellCurve(171,853,GF(2671));
P:=ellPoint({1980,431,1},E);
assert(1024*P == ellPoint({1718,584,1},E));
assert(0*P == ellPoint({0,1,0},E));
assert((P*1943) == ellPoint({1432,667,1},E));
assert((P*947) == ellPoint({1046,547,1},E));
Q:=-P;
assert(Q*(-1943) == ellPoint({1432,667,1},E));
///
TEST ///
E:=ellCurve(171,853,ZZ/2671);
P:=ellPoint({1980,431,1},E);
assert(1024*P == ellPoint({1718,584,1},E));
assert(0*P == ellPoint({0,1,0},E));
assert((P*1943) == ellPoint({1432,667,1},E));
assert((P*947) == ellPoint({1046,547,1},E));
assert((-P)*(-1943) == ellPoint({1432,667,1},E));
///
TEST///
E:=ellCurve(3,8,GF(13));
F:=ellCurve(3,8,GF(13^2));
assert(E==E);
assert(E!=F);
P:=ellPoint({1,5,1},E);
Q:=ellPoint({1,5,1},F);
R:=P+P;
assert(P!=Q);
assert(P==(R-P));
///
TEST///
assert(bruteForceGroupOrder(ellCurve(3,8,GF(13)))==9);
assert(bruteForceGroupOrder(ellCurve(3,8,GF(13^2)))==171);
assert(bruteForceGroupOrder(ellCurve(3,8,GF(13^3)))==2268);
assert(bruteForceGroupOrder(ellCurve(171,853,GF(2671)))==2638);
assert(bruteForceGroupOrder(ellCurve(171,853,(ZZ/2671)))==2638);
assert(bruteForceGroupOrder(ellCurve({1,2,3,4,6},GF(29)))==30);
assert(bruteForceGroupOrder(ellCurveFromjInv(sub(2,GF(27))))==18);
///
TEST///
C:=ellCurve({0,-1,1,-10,-20},QQ);
assert(jInvariant C == -122023936/161051);
assert(Discriminant(ellCurve(1,0,QQ))==-64);
assert(Discriminant(ellCurve({0,0,1,0,0},QQ))==-27);
assert(jInvariant(ellCurve(1,0,QQ))==1728);
assert(jInvariant(ellCurve({0,0,1,0,0},QQ))==0);
assert(toShortWForm C == ellCurve(-13392,-1080432,QQ));
///
TEST///
j := sub(3,GF(13));
E := ellCurveFromjInv(j);
EE := toShortWForm E;
assert(Discriminant E == Discriminant EE);
assert((jInvariant E) == (jInvariant EE));
///
TEST///
E:=toWeierstrass({-1,0,0,0,0,-1,0,1,0,-2},{1,2,1},QQ);
F:=ellCurve(1,2,QQ);
assert(jInvariant(F) == jInvariant(E));
///

TEST///
K:=GF(13);
EE:=toShortWForm toWeierstrass({-1,0,0,0,0,-1,0,1,0,-2},{1,2,1},K);
FF:=ellCurve(1,2,K);
assert(jInvariant(FF) == jInvariant(EE));
///
end
