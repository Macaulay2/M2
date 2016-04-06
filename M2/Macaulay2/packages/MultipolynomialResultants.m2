
newPackage(
       "MultipolynomialResultants",
	Version => "0.2", 
    	Date => "April 6, 2016",
    	Authors => {{Name => "Giovanni Stagliano'", 
		     Email => "giovannistagliano@gmail.com" 
                    }
                   },
    	Headline => "Resultants and discriminants for multivariate polynomials",
    	DebuggingMode => false,
	Reload => false
    	)

export{
   "Resultant",
   "Discriminant"
};

needsPackage "PushForward"

verbose:=false;
    
Resultant=method(TypicalValue => RingElement);
    
Resultant Matrix := (F) -> (
      if verbose then <<"Running Resultant for "<<toString F<<endl;
      ------
      if numgens target F != 1 then error("expected a matrix with one row");
      if not isPolynomialRing ring F then error("the base ring must be a polynomial ring");
      if numgens source F != numgens ring F then error("the number of variables in the ring must be equal to the number of entries of the matrix, but got " | toString(numgens ring F) | " variables and " | toString(numgens source F) | " entries");
--    if not isHomogeneous F then error("expected a homogeneous matrix");
    n:=numgens source F-1;
    K:=coefficientRing ring F;
    x:=local x;
    Pn:=K[x_0..x_n];
    F=sub(F,vars Pn);
    F':=F;
       if not isField K then (K':=frac(K); Pn':=K'[x_0..x_n]; F'=sub(F,Pn'));
    R:=internalResultant F';
    if R != 0 then (
        if not isField K then (
             if not isUnit denominator R then return R else return (denominator R)^(-1) * (numerator R);
        );
        return R;
    );
    if dim ideal F' > 0 then return sub(0,K);
    if min degrees ideal F' == max degrees ideal F' then return Resultant SLtransformation F else error("Resultant not implemented yet for this case");
);
    
internalResultant = (F) -> (
    if verbose then <<"Running internalResultant for "<<toString F<<endl;
    ------
    -- "Poisson Formula": Theorem 3.4, p. 96 of [David A. Cox and John Little and Donal O'shea - Using Algebraic Geometry - (2005)]
    n:=numgens source F-1;
    K:=coefficientRing ring F;
    x:=local x;
    R:=K[x_0..x_n];
    F=sub(F,vars R);
      if n==0 then return leadCoefficient F_(0,0);
    d:=flatten degrees ideal F;
    xn:=x_n; S:=K[x_0..x_(n-1)];
    f:=sub(sub(F,{xn=>1}),S);
    Fbar:=submatrix'(sub(sub(F,{xn=>0}),S),,{n});
    Res0:=internalResultant Fbar;
       if Res0 == 0 then return Res0;
    A:=S/ideal(submatrix'(f,,{n}));
    mf:=map(A^1,A^1,matrix{{sub(f_(0,n),A)}});
    mf=pushFwd(map(A,K[],{}),mf);
    Res0^(d_n) * sub(det mf,K)
);
    
SLtransformation = (F) -> (
    -- F row matrix of polynomials
    n:=numgens source F -1;
    K:=coefficientRing ring F;
    A:=matrix 0; d:=0; randomness:=1;
    while d == 0 do (
       A=matrix(for i to n list for j to n list sub(random(-randomness+1,randomness),K));
       d=determinant A
    ); 
    A=d^(-1) * (matrix A_0) | submatrix'(A,,{0});
    if verbose then <<"Applied the matrix: "<<A<<endl; 
    transpose(sub(A,ring F)*transpose(F))
);

Discriminant=method(TypicalValue => RingElement);
    
Discriminant RingElement := (G) -> (
    -- p. 426 of [Israel M. Gelfand, Mikhail Kapranov, Andrei Zelevinsky-Discriminants, resultants, and multidimensional determinants (1994)];
    -- see also [TAKESHI SAITO - THE DISCRIMINANT AND THE DETERMINANT OF A HYPERSURFACE OF EVEN DIMENSION]
       if not (isPolynomialRing ring G and isHomogeneous G) then error("expected a homogeneous polynomial");
    DisG:=Resultant transpose jacobian matrix{{G}};
       if (not isPolynomialRing ring DisG or DisG==0) then return DisG;
    -- n:=numgens ring G-2; d:=degree ideal G; a:=((d-1)^(n+2)-(-1)^(n+2))/d;
    -- DisG/(d^a)
    g:=gcd flatten entries (coefficients DisG)_1;
    DisG=DisG/g;
    (denominator DisG)^(-1) * numerator(DisG)
);
        
beginDocumentation() 
document { 
    Key => MultipolynomialResultants, 
    Headline => "package for computation of resultants and discriminants", 
    EM "MultipolynomialResultants", " is a package to compute resultants and discriminants.",
    PARA{},
    "Let ",TEX///$F_0,\ldots,F_n$///," be ",TEX///$n+1$///," homogeneous polynomials in ",TEX///$n+1$///," variables ",TEX///$x_0,\ldots,x_n$///,". The resultant ",TEX///$R(F_0,\ldots,F_n)$///," is an irreducible polynomial in the coefficients of ",TEX///$F_0,\ldots,F_n$///," which vanishes whenever ",TEX///$F_0,\ldots,F_n$///," have a common root.
    The discriminant of a homogeneous polynomial is defined, up to a scalar factor, as the resultant of its partial derivatives. In this package, the resultant is computed, recursively, through the Poisson Formula.",
    PARA{},
    "For the general theory, see one of the following:",
    PARA{},
    "1) David A. Cox, John Little, Donal O'shea - ",HREF{"http://link.springer.com/book/10.1007%2Fb138611","Using Algebraic Geometry"}, ", Graduate Texts in Mathematics, Volume 185 (2005).", 
    PARA{},
    "2) Israel M. Gelfand, Mikhail M. Kapranov, Andrei V. Zelevinsky - ",HREF{"http://link.springer.com/book/10.1007%2F978-0-8176-4771-1","Discriminants, Resultants, and Multidimensional Determinants"}, ", Mathematics: Theory & Applications (1994)",
}
document { 
    Key => {Resultant,(Resultant,Matrix)}, 
    Headline => "multipolynomial resultant", 
    Usage => "Resultant F", 
    Inputs => { "F" => Matrix => {"a row matrix whose entries are ", TEX///$n+1$///," homogeneous polynomials ", TEX///$F_0,\ldots,F_n$///," in ", TEX///$n+1$///," variables"} 
}, 
    Outputs => { 
    {"the resultant of ",TEX///$F_0,\ldots,F_n$///} 
}, 
    EXAMPLE { 
    "R = ZZ[a,b,c,d][x,y];",
    "F = matrix{{x^7+3*x^4*y^3+a*x*y^6+b*y^7,x^8+x^5*y^3+c*x*y^7+d*y^8}}",
    "time Resultant F",
    "R=ZZ/97[t_0..t_3];",
    "F=matrix{{random(1,R),random(4,R),random(2,R),random(1,R)}}",
    "time Resultant F",
    "a=symbol a; x=symbol x; R=(QQ[a_(0,0),a_(0,1),a_(0,2),a_(1,0),a_(1,1),a_(1,2),a_(1,3),a_(1,4),a_(1,5),a_(2,0),a_(2,1),a_(2,2),a_(2,3),a_(2,4),a_(2,5),a_(2,6),a_(2,7),a_(2,8),a_(2,9)])[x_0,x_1,x_2];",
    "L = a_(0,0)*x_0+a_(0,1)*x_1+a_(0,2)*x_2",
    "Q = a_(1,0)*x_0^2+a_(1,1)*x_0*x_1+a_(1,3)*x_1^2+a_(1,2)*x_0*x_2+a_(1,4)*x_1*x_2+a_(1,5)*x_2^2", 
    "C = a_(2,0)*x_0^3+a_(2,1)*x_0^2*x_1+a_(2,3)*x_0*x_1^2+a_(2,6)*x_1^3+a_(2,2)*x_0^2*x_2+a_(2,4)*x_0*x_1*x_2+a_(2,7)*x_1^2*x_2+a_(2,5)*x_0*x_2^2+a_(2,8)*x_1*x_2^2+a_(2,9)*x_2^3",
    "time Resultant matrix{{L,Q,C}}"
},
    SeeAlso => {resultant} 
}
document { 
    Key => {Discriminant,(Discriminant,RingElement)}, 
    Headline => "resultant of the partial derivatives", 
    Usage => "Discriminant F", 
    Inputs => { "F" => RingElement => {"a homogeneous polynomial"} 
}, 
    Outputs => { 
    {"the discriminant of ",TT "F"} 
}, 
    EXAMPLE { 
    "(ZZ[a_0, a_1, a_2])[x_0, x_1]; F=a_0*x_0^2+a_1*x_0*x_1+a_2*x_1^2",
    "time Discriminant F",
    "(ZZ[a_0, a_1, a_2, a_3])[x_0, x_1]; F=a_0*x_0^3+a_1*x_0^2*x_1+a_2*x_0*x_1^2+a_3*x_1^3",
    "time Discriminant F",
},
    PARA{},
    "The next example illustrates how computing the intersection of a pencil generated by two degree ",TEX///$d$///," forms ",TEX///$F(x_0,\ldots,x_n), G(x_0,\ldots,x_n)$///," with the discriminant hypersurface in the space of forms of degree ",TEX///$d$///," on ",TEX///$\mathbb{P}^n$///,
    EXAMPLE {
    "R=ZZ/331[x_0..x_3];",
    "F=x_0^4+x_1^4+x_2^4+x_3^4",
    "G=x_0^4-x_0*x_1^3-x_2^4+x_2*x_3^3",
    "R'=ZZ/331[t_0,t_1][x_0,x_1,x_2];",
    "pencil=t_0*sub(F,R')+t_1*sub(G,R')",
    "time D=Discriminant pencil",
    "time factor D"
},
    SeeAlso => {discriminant} 
}

TEST /// -- testing resultant
GenericMatrix = (d,K) -> (
    -- row matrix of generic homogeneous polynomials of degrees d_0,d_1,...,d_n in n+1 variables
    n:=#d-1;
    N:=for i to n list binomial(n+d_i,d_i)-1;
    a:=local a; x:=local x;
    A:=for i to n list K[a_(i,0)..a_(i,N_i)];
--  S:=K[]; for i to n do S=S**A_i;
    S:=K[]; for i to n do S=K[gens S,a_(i,0)..a_(i,N_i)]; 
    R:=S[x_0..x_n];
    M:=matrix pack(n+1,for i to n list (sub(vars A_i,R)*transpose(gens (ideal vars R)^(d_i)))_(0,0));
    M
);
GenericResultant = (d,K) -> Resultant GenericMatrix (d,K);
standardResultant = (F) -> (
    R:=ring F;
    K:=coefficientRing R;
    x:=gens R;
    if 1 =!= #x-1 or numgens target F =!= 1 or numgens source F =!= 2 then error("invalid input data");
    d:=for i to 1 list degree ideal F_(0,i);
    f:=sub(F,{x_1=>1});
    S:=K[x_0];
    f=sub(f,S);
    sub(resultant(f_(0,0),f_(0,1),(gens S)_0),K)
);
compareResultants = (d0,d1,K) -> (
    t:=local t;
    R:=K[t_0,t_1];
    G:=matrix{{random(d0,R),random(d1,R)}};
    <<"Polynomials: "<<toString flatten entries G<<endl; 
    w:=Resultant G;
    v:=standardResultant G;
    <<"Resultants: "<<w<<endl;
    <<"            "<<v<<endl;
    w == v or w == -v
);
SLtransf = (F) -> (
    -- F row matrix of polynomials
    n:=numgens source F -1;
    K:=coefficientRing ring F;
    A:=matrix 0; d:=0; randomness:=2;
    while d == 0 do (
       A=matrix(for i to n list for j to n list sub(random(-randomness+1,randomness),K));
       d=determinant A
    ); 
    A=d^(-1) * (matrix A_0) | submatrix'(A,,{0});
    transpose(sub(A,ring F)*transpose(F))
);
L=true; for i from 1 to 5 do for j from 1 to 5 do L= L and compareResultants(i,j,QQ);
for i from 1 to 5 do for j from 1 to 5 do L= L and compareResultants(i,j,ZZ/33331);
assert(L) 
rightRing112=ZZ[a_(0,0),a_(0,1),a_(0,2),a_(1,0),a_(1,1),a_(1,2),a_(2,0),a_(2,1),a_(2,2),a_(2,3),a_(2,4),a_(2,5)];
rightRes112=a_(0,0)^2*a_(1,1)^2*a_(2,5)-a_(0,0)^2*a_(1,1)*a_(1,2)*a_(2,4)+a_(0,0)^2*a_(1,2)^2*a_(2,3)-2*a_(0,0)*a_(0,1)*a_(1,0)*a_(1,1)*a_(2,5)+a_(0,0)*a_(0,1)*a_(1,0)*a_(1,2)*a_(2,4)+a_(0,0)*a_(0,1)*a_(1,1)*a_(1,2)*a_(2,2)-a_(0,0)*a_(0,1)*a_(1,2)^2*a_(2,1)+a_(0,1)^2*a_(1,0)^2*a_(2,5)-a_(0,1)^2*a_(1,0)*a_(1,2)*a_(2,2)+a_(0,1)^2*a_(1,2)^2*a_(2,0)+a_(0,0)*a_(0,2)*a_(1,0)*a_(1,1)*a_(2,4)-a_(0,0)*a_(0,2)*a_(1,1)^2*a_(2,2)-2*a_(0,0)*a_(0,2)*a_(1,0)*a_(1,2)*a_(2,3)+a_(0,0)*a_(0,2)*a_(1,1)*a_(1,2)*a_(2,1)-a_(0,1)*a_(0,2)*a_(1,0)^2*a_(2,4)+a_(0,1)*a_(0,2)*a_(1,0)*a_(1,1)*a_(2,2)+a_(0,1)*a_(0,2)*a_(1,0)*a_(1,2)*a_(2,1)-2*a_(0,1)*a_(0,2)*a_(1,1)*a_(1,2)*a_(2,0)+a_(0,2)^2*a_(1,0)^2*a_(2,3)-a_(0,2)^2*a_(1,0)*a_(1,1)*a_(2,1)+a_(0,2)^2*a_(1,1)^2*a_(2,0)
assert(sub(GenericResultant((1,1,2),ZZ),vars rightRing112)==rightRes112);
x:=local x;
R:=ZZ[x_0..x_3];
assert( Resultant matrix{for i to 3 list x_i^(random(1,5))} == 1)
F=GenericMatrix((1,1,1),ZZ/3331) 
assert(Resultant F == Resultant SLtransf F)
F=GenericMatrix((2,2),ZZ/3331) 
assert(Resultant F == Resultant SLtransf F)
F=GenericMatrix((1,1,1),QQ) 
assert(Resultant F == Resultant SLtransf F)
F=GenericMatrix((2,2),QQ) 
assert(Resultant F == Resultant SLtransf F)
R=QQ[t_0,t_1]
assert(Resultant(matrix{{8*t_0*t_1+2*t_1^2, 6*t_0^2+3*t_0*t_1+6*t_1^2}}) == 2160)
-- Resultant matrix{{8*t_0*t_1^2+2*t_1^3, 6*t_0^2+3*t_0*t_1+6*t_1^2}}
R=QQ[x_0..x_6]; X=transpose vars R;
A=matrix for i to 6 list for j to 6 list random(QQ)
assert(Resultant transpose(A*X) == det A)
/// 
    
TEST /// -- testing discriminant
genericPolynomial = (d,n,K) -> ( 
    -- generic homogeneous polynomial in n+1 variables of degree d over K
    N:=binomial(n+d,d)-1;
    a:=local a; x:=local x;
    S:=K[a_0..a_N];
    R:=S[x_0..x_n];
    F:=((vars S)*transpose(gens (ideal vars R)^d))_(0,0);
    F
);
GenericDiscriminant = (d,n,K) -> Discriminant genericPolynomial(d,n,K);
dualOfSmoothVariety = (I) -> ( 
    R:=ring I; c:=codim I;
    K:=coefficientRing R; n:=numgens R-1;
    x:=local x; z:=local z;
    S:=K[x_0..x_n,z_0..z_n,MonomialOrder=>Eliminate (n+1)];
    I=(map(S,R,{x_0..x_n}))(I);
    JZ:=submatrix(jacobian I,{0..n},)|transpose(gens ideal(z_0..z_n)); 
    D:=ideal selectInSubring(1,gens gb saturate(I+minors(c+1,JZ),ideal(x_0..x_n)));
    trim sub(sub(D,K[z_0..z_n]),vars R) 
);
Veronese = (d,n,K) -> (
    t:=local t; x:=local x; T:=K[t_0..t_n]; R:=K[x_0..x_(binomial(n+d,d)-1)];
    saturate kernel map(T,R,gens((ideal vars T)^d))
);
compareDiscriminants = (d,n,K) -> (
    D1:=GenericDiscriminant(d,n,K);
    D2:=(dualOfSmoothVariety Veronese(d,n,K))_0;
    D2=sub(D2,vars ring D1);
    <<"via Poisson: "<<toString D1<<endl;
    <<"via Dual:    "<<toString D2<<endl;
    ideal(D2) == ideal(D1)
);
assert(compareDiscriminants(2,1,QQ) and compareDiscriminants(3,1,GF 5^5) and compareDiscriminants(4,1,QQ) and compareDiscriminants(5,1,ZZ/33331) and compareDiscriminants(2,2,ZZ/101))
/// 

end 



