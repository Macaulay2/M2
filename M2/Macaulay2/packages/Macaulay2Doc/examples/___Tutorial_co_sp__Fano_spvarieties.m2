R = ZZ/32003[a,b,c,d]
X = ideal(a^3+b^3+c^3+d^3)
KK = coefficientRing R 
S = KK [s,t,p_0..p_3,q_0..q_3]
F = map(S,R,
       s*matrix{{p_0..p_3}} +
       t*matrix{{q_0..q_3}}
       )
FX = F X
cFX = last coefficients(gens FX, Variables => {s,t})
S1 = KK[p_0..p_3,q_0..q_3]
cFX = substitute(cFX, S1)
S1bar = S1/ideal cFX
GR = coefficientRing R[x_0..x_5]
M = substitute(
    exteriorPower(2, matrix{{p_0..p_3},{q_0..q_3}}),
    S1bar)
gr = map (S1bar, GR, M)
fano = trim ker gr
codim fano
degree fano
betti fano
document {
     Key => Fano2, 
        TT "Fano2(k,X,GR) or  Fano2(k,X)", " -- computes 
        the ideal of a Fano scheme in the Grassmannian.",
        PARA{},
        "Given an ideal X representing a projective variety 
        in P^r, a positive integer k<r, and optionally a 
        ring GR with (exactly) r+1 choose k+1 variables, 
        representing the ambient space of the Grassmannian of 
        k-planes in P^r, this routine returns the ideal in
        GR of the Fano scheme that parametrizes the k-planes 
        lying on X. If the optional third argument is not 
        present, the routine fabricates its own local ring, 
        and returns an ideal over it."
        }
document {
     Key => symbol Grassmannian2, 
    TT "Grassmannian2(k,r,R) or 
        Grassmannian2(k,r)",
       "-- Given natural numbers k <= r,
        and optionally a ring R with at least binomial(r+1,k+1)
        variables, the routine defines the ideal of the 
        Grassmannian of projective k-planes in P^r, using 
        the first binomial(r+1,k+1) variables of R. 
        If R is not given, the routine makes and uses
        ZZ/31991[vars(0..binomial(r+1,k+1)-1]."
        }
Fano2 = method()
Fano2(ZZ,Ideal,Ring) := (k,X,GR) -> (
  -- Get info about the base ring of X:
  -- The coefficient ring (to make new rings of
  -- the same characteristic, for example)
  -- and the number of variables
  KK:=coefficientRing ring X;
  r := (numgens ring X) - 1;
  -- Next make private variables for our 
  -- intermediate rings, to avoid interfering
  -- with something outside:
  t:=symbol t;
  p:=symbol p;
  -- And rings
  S1 := KK[t_0..t_k];
  S2 := KK[p_0..p_(k*r+k+r)];
  S := tensor(S1,S2);
  -- Over S we have a generic point of a generic
  -- line, represented by a row vector, which
  -- we use to define a map from the base ring
  -- of X
  F := map(S,ring X,
          genericMatrix(S,S_0,1,k+1)*
          genericMatrix(S,S_(k+1),k+1,r+1)
          );
  -- We now apply F to the ideal of X
  FX := F X;
  -- and the condition we want becomes the condition
  -- that FX vanishes identically in the t_i.
  -- The following line produces the matrix of
  -- coefficients of the monomials in the 
  -- variables labelled 0..k:
  cFX := last coefficients (gens FX, Variables => toList apply(0..k, i -> S_i));
  -- We can get rid of the variables t_i
  -- to ease the computation:
  cFX = substitute(cFX, S2);
  -- The ring we want is the quotient
  S2bar := S2/ideal cFX;
  -- Now we want to move to the Grassmannian,
  -- represented by the ring GR
  -- We define a map sending the variables of GR
  -- to the minors of the generic matrix in the
  -- p_i regarded as elements of S1bar
  gr := map(S2bar,GR,
            exteriorPower(k+1, 
            genericMatrix(S2bar,S2bar_0,k+1,r+1)
            )
           );
  -- and the defining ideal of the Fano variety is
  ker gr
)
Fano2(ZZ, Ideal) := (k,X) -> (
  KK:=coefficientRing ring X;
  r := (numgens ring X) - 1;
  -- We can specify a private ring with binomial(r+1,k+1)
  -- variables as follows
  GR := KK[Variables => binomial(r+1,k+1)];
  -- the work is done by
  Fano2(k,X,GR)
)
Grassmannian2 = method()
Grassmannian2(ZZ,ZZ,Ring) := (k,r,R) ->( 
        KK := coefficientRing R;
        RPr := KK[Variables => r+1];
        Pr := ideal(0_RPr);
        Fano2(k,Pr)
     )
Grassmannian2(ZZ,ZZ) := (r,k) -> (
        R := ZZ/31991[
               vars(0..(binomial(r+1,k+1)-1))
                    ];
        Grassmannian2(k,r,R)
                     )
KK = ZZ/31991
R = KK[a,b,c,d]
X = ideal(a*b-c*d)
I = Fano2(1,X)
dim I
degree I
KK = ZZ/31991
P5 = KK[a..f]
MVero = genericSymmetricMatrix(P5,a,3)
Vero = minors(2,MVero)
catalecticant = (R,v,m,n) -> 
        map(R^m,n,(i,j)-> R_(i+j+v))
catalecticant(P5,1,2,4)
M13 = catalecticant(P5,0,2,1) |
           catalecticant(P5,2,2,3)
S13 = minors(2,M13)
M22 = catalecticant(P5,0,2,2) | catalecticant(P5,3,2,2)
S22 = minors(2, M22)
Verores = res coker gens Vero
S22res = res coker gens S22
S13res = res coker gens S13
betti Verores
betti S22res
betti S13res
FVero = Fano2(1, Vero)
betti gens FVero
FS13 = Fano2(1, S13)
hilbertPolynomial coker gens FS13
FS22 = Fano2(1, S22)
hilbertPolynomial coker gens FS22
