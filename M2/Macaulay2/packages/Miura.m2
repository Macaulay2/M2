-- -*- coding: utf-8 -*-
newPackage(
	"Miura", 
	Version => "1.0",
	Date => "25 December 2015",
	Authors => {
		{Name => "Joe Suzuki", 
		     Email => "suzuki@math.sci.osaka-u.ac.jp", 
		     HomePage => ""}
	},
	Headline => "Miura: Divisor Class Group Arithmetic",
	DebuggingMode => false
	)

export {"pR", "qR", "inv", "reduced", "add", "double", "multi"}

-- specify the underlying polynomial ring 
pR=(kk,v,w)->kk[v,MonomialOrder=>{Weights => w, Weights=>toList(1 ..#w) }];

-- specify the quotient ring
qR=(R,p)->R/ideal p

-- computing the reduced inverse ideal
inv=J-> quotient(ideal first first entries gens gb J, J)

-- computing the reduced ideal
reduced=J->inv inv J

-- computing the reduced multiplied ideal
add=(J,K)-> reduced (J*K)

-- computing the reduced squared ideal 
double=J->add(J,J)

-- computing the reduced ideal multiplied by integer 
multi=(J,m)-> if m==0 then return ideal 1_I else if m\%2==0 then double multi(J,m//2) else add(double multi(J,(m-1)//2),J)

beginDocumentation()
-------------------------------------------------------
--DOCUMENTATION Miura
-------------------------------------------------------

doc///
    Key
        pR
    Headline
     	specify the underlying polynomial ring
    Usage
     	R=pR(kk,v,w)
    Inputs
	    kk: a complete field 
	    v: a list of variables
	    w: a list of weights corresponding to v
    Outputs
     	R: the polynomial ring kk[v] with the monomial order w.r.t. weights w over v.
    Description
     	Text
	        The function pR generates the polynomial ring kk[v] with the monomial order w.r.t. weights w over the variables v. 
		    The elements in w should be positive and mutually prime. The number of elements in v and w coincide.    
	    Example
		    R=pR(GF 13,{x,y},{2,3});
    SeeAlso
     	   qR
///

doc///
    Key
        qR
    Headline
     	specify the underlying quotient ring
    Usage
     	I=qR(R,p)
    Inputs
	    R: the polynomial ring specified using pR  
	    p: the ideal (generators) of the curve
    Outputs
     	I: the quatient ring R/ideal p 
    Description
     	Text
	       The function qR generates the quotient ring R over the ideal p. 
		   The variables and equations (the ideal generators) should be specified by pR and in the form of Miura, respectively.
	    Example
		    R=pR(GF 13,{x,y},{2,3});
            I=qR(R,y^2-x^3-7*x);
    SeeAlso
     	   pR
///

doc///
    Key
        inv
    Headline
     	compute the reduced inverse ideal
    Usage
     	K=inv(J)
    Inputs
	    J: an integral ideal  
    Outputs
     	K: the reduced inverse ideal of J
    Description
     	Text
	        The function inv computes the inverse reduced ideal given an integral ideal    
	    Example
		    R=pR(GF 13,{x,y},{2,3});
            I=qR(R,y^2-x^3-7*x);
            J=ideal(x,y);
			inv(J)
    SeeAlso
     	reduced
///

doc///
    Key
        reduced
    Headline
     	compute the reduced ideal
    Usage
     	K=reduced(J)
    Inputs
	    J: an integral ideal  
    Outputs
     	K: the reduced ideal of J
    Description
     	Text
	        The function reduced computes the reduced ideal given an integral ideal    
	    Example
		    R=pR(GF 13,{x,y},{2,3});
            I=qR(R,y^2-x^3-7*x);
            J=ideal(x,y);
			reduced(J)
    SeeAlso
     	inv, add
///

doc///
    Key
        add
    Headline
     	compute the reduced multiplied ideal
    Usage
     	L=add(J,K)
    Inputs
	    J,K: integral ideals  
    Outputs
     	L: the reduced ideal of J*K
    Description
     	Text
	        The function add computes the reduced ideal of multiplication of two integral ideals    
	    Example
		    R=pR(GF 13,{x,y},{2,3});
            I=qR(R,y^2-x^3-7*x);
            J=ideal(x,y);
			K=ideal(x-2,y-3)
			add(J,K)
			L=J*K
			reduced(L)
    SeeAlso
     	inv, reduced
///

doc///
    Key
        double
    Headline
     	compute the reduced squared ideal
    Usage
     	K=double(J)
    Inputs
	    J: integral ideal  
    Outputs
     	K: the reduced ideal of J*J
    Description
     	Text
	        The function double computes the reduced ideal of J*J given an integral ideal J    
	    Example
		    R=pR(GF 13,{x,y},{2,3});
            I=qR(R,y^2-x^3-7*x);
            J=ideal(x,y);
			double J
			reduced(J*J)
			add(J,J)
    SeeAlso
     	reduced, add
///

doc///
    Key
        multi
    Headline
     	compute the reduced ideal multimplied by integer
    Usage
     	K=multi(J,m)
    Inputs
	    J: integral ideal
        m: nonnegative integer		
    Outputs
     	K: the reduced ideal of J multiplied m times
    Description
     	Text
	        The function multi computes the reduced ideal of an integral ideal multiplied by a nonnegative integer    
	    Example
		    R=pR(GF 13,{x,y},{2,3});
            I=qR(R,y^2-x^3-7*x);
            J=ideal(x,y);
			multi(J,5)
		SeeAlso
     	add, double
///

TEST ///
--Hyper Elliptic Curves
R=pR(GF 2^3,{x,y},{2,5});
I=qR(R,y^2-x^5-1);
J=ideal(x+1,y);
K=ideal(x,y-1);
L=ideal(x,y+1);
A=add(J,K)
multi(A,2)
multi(A,3)
multi(A,4)
A5=multi(A,5)
add(A5,A5)
--Cab Curves
R=pR(GF 5,{x,y},{3,4});
I=qR(R,y^3-x^4-1);
J=ideal(x-2,y-3);
K=ideal(x-4,y);
L=ideal(x,y-1);
A=add(J,add(K,L))
A2=multi(A,2)
A3=multi(A,3)
A4=multi(A,4)
A5=multi(A,5)
A6=multi(A,6)
--General Curves
R=pR(GF 5,{x,y,z},{4,6,5});
I=qR(R,{y^2-x^3-1,z^2-x*y-1});
J=ideal(x-2,y-2,z);
K=ideal(x-4,y,z-1);
L=ideal(x,y-1,z-4);
M=ideal(x,y-4,z-1);
A=J*K*L*M
inv(A)
multi(A,654)
multi(A,327)
multi(A,109)
///

end