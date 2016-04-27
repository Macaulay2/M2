-- -*- coding: utf-8 -*-
newPackage(
	"Miura", 
	Version => "0.1",
	Date => "25 December 2015",
	Authors => {
		{Name => "Joe Suzuki", 
		     Email => "suzuki@math.sci.osaka-u.ac.jp", 
		     HomePage => ""}
	},
	Headline => "Miura: Divisor Class Group Computation",
	DebuggingMode => false
	)

export {"pR", "qR", "inv", "reduced", "add", "double", "multi"}

-- specify the underlying polynomial ring 
pR=(kk,v,w)->kk[v,MonomialOrder=>{Weights => w, Weights=>toList(1 ..#w) }];

-- specify the quotient ring
qR=(PR,p)->PR/ideal p

-- computing the reduced inverse ideal
inv=J-> quotient(ideal first first entries gens gb J, J)

-- computing the reduced ideal
reduced=J->inv inv J

-- computing the reduced multiplied ideal
add=(J,K)-> reduced (J*K)

-- computing the reduced squared ideal 
double=J->add(J,J)

-- computing the reduced ideal multiplied by integer 
multi=(J,m)-> if m==0 then return ideal promote(1, ring J) else if m%2==0 then double multi(J,m//2) else add(double multi(J,(m-1)//2),J)

-------------------------------------------------------
--DOCUMENTATION Miura
-------------------------------------------------------
beginDocumentation()

document{
     Key => Miura,
     Headline => "A package for computing objects for general nonsingular curves in the Miura form, an extension of the Weierstrass form",
	 PARA {"Miura expresses any nonsingular curves. For example, hyper-elliptic curves (e.g., y^2=x^5+x+1), C_{ab} curves (e.g. y^3=x^4+2x+1),
complete intersection (e.g. {y^2-x^3-1,z^2-x*y-1}).  
For the Miura form, the pole orders should be specified such as 2 and 3 for x and y of an elliptic curve.  Currently, only divisor class group computation is available for the package. For the elliptic curves,
[(P)-(O)]+[(Q)-(O)] = [(P+Q)-(O)] for two points P, Q and the point O at infinity. For the general nonsingular curves, any divisor class is uniquely expressed by E-g(O) with E a positive divisor of degree g (genus). This package reduces the divisor class  group addition to ideal class group multiplication, and utilizes Groebner basis computation. See http://arxiv.org/pdf/1512.08040v1.pdf for the details."}
     }
	 
document{
    Key => pR,
	Inputs => {"kk"=>{"any field"},"v"=>{"a list of variables"},"w"=>{"a list of weights corresponding to", TT "v"} }, 
    Usage => "R=pR(kk,v,w)",
 	Outputs => {ofClass PolynomialRing},
    Headline => "A method for specifying the underlying polynomial ring",
    PARA {"The function pR generates the polynomial ring kk[v] with the monomial order w.r.t. weights w over the variables v. The elements in w should be positive and mutually prime. The number of elements in v and w coincide."},
	EXAMPLE {"R=pR(GF 13,{x,y},{2,3})"}
}

document{
    Key => qR,
	Inputs => {"PR"=>{"the polynomial ring specified using", TT "pR"},"p"=>{"the ideal (generators) of the curve"}}, 
    Usage => "I=qR(R,p)",
 	Outputs => {ofClass Ideal},
    Headline => "A method for specifying the underlying quotient ring",
    PARA {"The function qR generates the quotient ring R over the ideal p. The variables and equations (the ideal generators) should be specified by pR and in the form of Miura, respectively."},
	EXAMPLE {"R=pR(GF 13,{x,y},{2,3})","I=qR(R,y^2-x^3-7*x)"}
}

document{
    Key => inv,
	Inputs => {"J" => {"an integral ideal"}},
    Usage => "K=inv(J)",
    Outputs => {ofClass Ideal},
    Headline => "A method for computing the reduced inverse ideal",
    PARA {"The function inv computes the inverse reduced ideal given an integral ideal"},    
	EXAMPLE {"R=pR(GF 13,{x,y},{2,3})", "I=qR(R,y^2-x^3-7*x)", "J=ideal(x,y);inv(J)"}
}
 
document{
    Key => reduced,
	Inputs => {"J" => {"an integral ideal"}},
    Usage => "K=reduced(J)",
    Outputs => {ofClass Ideal},
    Headline => "A method for computing the reduced ideal",
    PARA {"The function reduced computes the reduced ideal given an integral ideal"},   
	EXAMPLE {"R=pR(GF 13,{x,y},{2,3})","I=qR(R,y^2-x^3-7*x)","J=ideal(x,y);reduced(J)"}
}

document{
    Key => add,
	Inputs => {"J" => {"an integral ideal"},"K" => {"an integral ideal"}},
    Usage => "L=add(J,K)",
    Outputs => {ofClass Ideal},
    Headline => "A method for computing the reduced ideal of J*K",
    PARA {"The function add computes the reduced ideal of multiplication of two integral ideals"},    
	EXAMPLE {"R=pR(GF 13,{x,y},{2,3})","I=qR(R,y^2-x^3-7*x)","J=ideal(x,y)",
			 "K=ideal(x-2,y-3)","add(J,K)","L=J*K","reduced(L)"}
}

document{
    Key => double,
	Inputs => {"J" => {"an integral ideal"}},
    Usage => "K=double(J)",
    Outputs => {ofClass Ideal},
    Headline => "A method for computing the squared ideal J*J",
    PARA {"The function double computes the reduced ideal of J*J given an integral ideal J"},    
	EXAMPLE {"R=pR(GF 13,{x,y},{2,3})","I=qR(R,y^2-x^3-7*x)","J=ideal(x,y)", "double J", "reduced(J*J)","add(J,J)"}
}

document{
    Key => multi,
	Inputs => {"J" => {"an integral ideal"}, "m" => {"a positive integer"}},
    Usage => "K=multi(J,m)",
    Outputs => {ofClass Ideal},
    Headline => "A method for computing the reduced ideal of an integral ideal J multimplied by integer m",
    PARA {"	        The function multi computes the reduced ideal of an integral ideal multiplied by a nonnegative integer"},    
	EXAMPLE {"R=pR(GF 13,{x,y},{2,3})","I=qR(R,y^2-x^3-7*x)","J=ideal(x,y)","multi(J,5)"}
}

--------------------------------------------
--TEST
--------------------------------------------
TEST ///
--Hyper Elliptic Curves
R=pR(GF 2^3,{x,y},{2,5});
I=qR(R,y^2-x^5-1);
J=ideal(x+1,y);
K=ideal(x,y-1);
L=ideal(x,y+1);
A=add(J,K);
multi(A,2);
multi(A,3);
multi(A,4);
A5=multi(A,5);
add(A5,A5);
///
TEST ///
--Cab Curves
R=pR(GF 5,{x,y},{3,4});
I=qR(R,y^3-x^4-1);
J=ideal(x-2,y-3);
K=ideal(x-4,y);
L=ideal(x,y-1);
A=add(J,add(K,L));
A2=multi(A,2);
A3=multi(A,3);
A4=multi(A,4);
A5=multi(A,5);
A6=multi(A,6);
///

TEST ///
--General Curves
R=pR(GF 5,{x,y,z},{4,6,5});
I=qR(R,{y^2-x^3-1,z^2-x*y-1});
J=ideal(x-2,y-2,z);
K=ideal(x-4,y,z-1);
L=ideal(x,y-1,z-4);
M=ideal(x,y-4,z-1);
A=J*K*L*M;
inv(A);
multi(A,654);
multi(A,327);
multi(A,109);
///

end
