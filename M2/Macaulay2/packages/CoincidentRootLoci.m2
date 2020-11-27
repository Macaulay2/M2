
if version#"VERSION" < "1.15" then error "this package requires Macaulay2 version 1.15 or newer";

newPackage(
       "CoincidentRootLoci",
	Version => "0.1.2", 
        Date => "June 22, 2020",
    	Headline => "coincident root loci",
        Authors => {{Name => "Maria Chiara Brambilla", Email => "brambilla@dipmat.univpm.it"},
                    {Name => "Giovanni Staglianò", Email => "giovannistagliano@gmail.com"}},
	Keywords => {"Real Algebraic Geometry", "Interfaces"},
        PackageExports => {"Cremona","Resultants"},
        DebuggingMode => false,
        AuxiliaryFiles => true,
        OptionalComponentsPresent => try get "!qepcad -h 2>&1" then true else false,
        CacheExampleOutput => true,
        Reload => false
)

export{"apolar",
       "recover",
       "CoincidentRootLocus",
       "coincidentRootLocus",
       "CRL",
       "supsets",
       "realRankBoundary",
       "complexrank",
       "realrank",
       "realroots",
       "QepcadOptions",
       "randomBinaryForm",
       "randomInCoisotropic",
       "generic",
       "projectiveJoin",
       "tangentSpace",
       "polarDegrees"
}

load "./CoincidentRootLoci/equationsCRL.m2";
load "./CoincidentRootLoci/equationsDualCRL.m2";

CLEAR = true;
VAR = "t";
KK = QQ;

--================================================================
--=== Coincident root loci =======================================
--================================================================

CRLOCUS := local CRLOCUS;

CoincidentRootLocus = new Type of MutableHashTable;

coincidentRootLocus = method(TypicalValue => CoincidentRootLocus, Dispatch => Thing, Options => {Variable => VAR});

coincidentRootLocus Thing := o -> LK -> ( 
   LK = sequence LK;
   (lambda,K) := if #LK == 2 and (class last LK === Ring or class last LK === QuotientRing) then LK else (LK,KK); 
   lambda = toList deepSplice lambda;
   try assert(ring matrix {lambda} === ZZ and min lambda > 0) else error "expected a list of positive integers";
   if not isField K then error "the coefficient ring needs to be a field";
   lambda = rsort lambda;
   if class CRLOCUS_(lambda,K,o.Variable) === CoincidentRootLocus then return CRLOCUS_(lambda,K,o.Variable); 
   d := #lambda;
   n := sum lambda;
   R := for i to d-1 list Grass(0,1,K,Variable=>toString(o.Variable)|toString(i));
   P1d := R_0; for i from 1 to d-1 do P1d = P1d ** R_i;
   y := local y; F := P1d[y_0,y_1];
   g := for i to d-1 list flatten entries sub(vars R_i,P1d);
   s := switch1 product apply(d,j->(switch1(g_j,F))^(lambda_j));
   phi := rationalMap(ring matrix {s},Grass(0,n,K,Variable=>o.Variable),s);
   if n <= 7 then forceImage(phi,idealCRL_lambda gens target phi);
   phi2 := if class phi === RationalMap then phi else parametrize(phi,getSymbol toString(o.Variable));
   if phi#"idealImage" =!= null and phi2#"idealImage" === null then forceImage(phi2,image phi);
   multiplicities := for i to n list #select(lambda,e -> e == i);
   X := new CoincidentRootLocus from {
           "partition" => lambda,
           "multiplicities" => multiplicities,
           "dim" => d,
           "ambient" => n,
           "degree" => lift((d!) * (product lambda) / (product apply(multiplicities,i->i!)),ZZ),
           "map" => phi,
           "map2" => phi2,
           "variable" => o.Variable,
           "singularLocus" => null,
           "dual" => null
   };
   CRLOCUS_(lambda,K,o.Variable) = X
);

coincidentRootLocus (VisibleList,Ring) := o -> (lambda,K) -> coincidentRootLocus(lambda,K,Variable=>o.Variable);

coincidentRootLocus (List) := o -> (lambda) -> coincidentRootLocus(lambda,KK,Variable=>o.Variable);

CRL = coincidentRootLocus;

JoinOfCoincidentRootLoci = new Type of MutableHashTable;

dualOfCoincidentRootLocus = method(TypicalValue => JoinOfCoincidentRootLoci);

dualOfCoincidentRootLocus (CoincidentRootLocus) := (X) -> ( 
   l := partition X;
   n := X#"ambient";
   m1 := (X#"multiplicities")_1;
   B := apply(for i to #l-1 list if l_i >= 2 then {n-l_i+2}|toList(l_i-2:1) else continue,t -> coincidentRootLocus(t,coefficientRing X,Variable=>X#"variable"));
   if #B == 0 then error "method not applicable";
   if #B == 1 then return first B;
   new JoinOfCoincidentRootLoci from {
           "listCRloci" => B,
           "dim" => n-1-m1,
           "ambient" => n,
           "degree" => if m1 == 0 then lift(((#l+1)!) * (product apply(l,j->j-1)) / (product apply((X#"multiplicities")_{2..#(X#"multiplicities")-1},i -> i!)),ZZ) else null,
           "ideal" => if n <= 6 then trim idealDualCRL_l gens ring X else null,
           "dual" => X
   }
);

expression CoincidentRootLocus := (X) -> (
    s := (if # partition X == 1 then "(" else "") | toString unsequence toSequence partition X | (if # partition X == 1 then ")" else "");
    s = "CRL" | substring(s,0,#s-1) | (if coefficientRing X =!= KK then ";"|toString(coefficientRing X) else "") | (if X#"variable" =!= VAR then ";'"|toString(X#"variable")|"'" else "") | ")";
    expression s
);

expression JoinOfCoincidentRootLoci := (Z) -> (
    L := apply(Z#"listCRloci",X -> toString expression X);
    E := L_0;
    for i from 1 to #L-1 do E = E|" * "|L_i;
    E = E|" (dual of "|toString expression dual Z|")";
    expression E
);

net CoincidentRootLocus := (X) -> net expression X;

net JoinOfCoincidentRootLoci := (Z) -> net expression Z;

toString CoincidentRootLocus := (X) -> "CRL("|toString partition X|","|toString coefficientRing X|","|"Variable=>"|toString X#"variable"|")"; 

toString JoinOfCoincidentRootLoci := (X) -> "dual("|toString dual X|")";

map CoincidentRootLocus := o -> (X) -> X#"map";

partition CoincidentRootLocus := (X) -> X#"partition";

partition JoinOfCoincidentRootLoci := (X) -> partition dual X;

ideal CoincidentRootLocus := (X) -> (
   if (map X)#"idealImage" =!= null then return image map X;
   forceImage(map X,trim homogenize(kernel affineMap X,first gens ring X));
   if (X#"map2")#"idealImage" === null then forceImage(X#"map2",image map X);
   image map X
);

ideal JoinOfCoincidentRootLoci := (Z) -> (  -- [H. Lee, B. Sturmfels, Duality of multiple root loci, Corollary 2.3]
   if Z#"ideal" === null then Z#"ideal" = projectiveJoin(unsequence toSequence apply(Z#"listCRloci",affineMap),SubringLimit=>if codim Z == 1 then 1 else infinity);
   Z#"ideal"
);

ring CoincidentRootLocus := (X) -> target map X;

ring JoinOfCoincidentRootLoci := (Z) -> ring dual Z;

coefficientRing CoincidentRootLocus := (X) -> coefficientRing ring X;

coefficientRing JoinOfCoincidentRootLoci := (Z) -> coefficientRing ring Z;

dim CoincidentRootLocus := (X) -> X#"dim";

dim JoinOfCoincidentRootLoci := (Z) -> Z#"dim";

codim CoincidentRootLocus := {} >> opts -> X -> X#"ambient" - X#"dim";

codim JoinOfCoincidentRootLoci := {} >> opts -> Y -> Y#"ambient" - Y#"dim";

degree CoincidentRootLocus := (X) -> X#"degree";

degree JoinOfCoincidentRootLoci := (Z) -> (
   if Z#"degree" === null then Z#"degree" = degree ideal Z;
   Z#"degree"
);

singularLocus (CoincidentRootLocus) := (X) -> ( -- Chipalkatti - On equations defining coincident root loci
   if X#"singularLocus" === null then (
       d := X#"ambient";
       e := X#"multiplicities";
       S := {};
       local f;
       for r1 from 1 to d do for r2 from 1 to d do if r1+r2<=d and e_r1>=1 and e_r2>=1 and r1!=r2 then (f = new MutableList from e; f#r1 = e_r1 - 1; f#r2 = e_r2 - 1; f#(r1+r2) = e_(r1+r2) + 1; S = append(S,toList f));  
       for r2 from 1 to d do for t from 1 to d do if t*r2<=d and t*r2!=r2 and e_(t*r2)>0 and e_r2>=t then (f = new MutableList from e; f#(t*r2) = e_(t*r2) + 1; f#r2 = e_r2 - t; S = append(S,toList f));
       for r1 from 1 to d do for r2 from 1 to d do for t1 from 1 to d do for t2 from 1 to d do if t1*r1==t2*r2 and t1*r1<=d and r1!=r2 and r1!=t1*r1 and r2!=t1*r1 and e_r1>=t1 and e_r2>=t2 then (f = new MutableList from e; f#r1 = e_r1 - t1; f#r2 = e_r2 - t2; f#(t1*r1) = e_(t1*r1) + 2; S = append(S,toList f));
       X#"singularLocus" = sort apply(apply(unique S,v -> rsort deepSplice apply(#v,j->v_j:j)),l -> coincidentRootLocus(l,coefficientRing X,Variable=>X#"variable"));
   );
   X#"singularLocus"
);

dual (CoincidentRootLocus) := JoinOfCoincidentRootLoci => {} >> o -> X -> (
   if X#"dual" === null then X#"dual" = dualOfCoincidentRootLocus X;
   X#"dual"
);

dual (JoinOfCoincidentRootLoci) := JoinOfCoincidentRootLoci => {} >> o -> Z -> Z#"dual";

CoincidentRootLocus == CoincidentRootLocus := (X,Y) -> X#"variable" === Y#"variable" and coefficientRing X === coefficientRing Y and partition X == partition Y;

JoinOfCoincidentRootLoci == JoinOfCoincidentRootLoci := (X,Y) -> X#"dual" === Y#"dual";

member (RingElement,Ideal) := (F,I) -> (
  if not (isPolynomialRing ring I and isHomogeneous I) then error "expected a homogeneous ideal in a polynomial ring";
  if not (isPolynomialRing ring F and numgens ring F == 2 and isHomogeneous F and first degree F == numgens ring I -1 and char coefficientRing ring F === char coefficientRing ring I) then error("expected a binary form of degree "|toString(numgens ring I -1)|" over "|toString(coefficientRing ring I));
  t := gens ring I;
  c := switch1 F;
  sub(I,apply(#t,i -> t_i => c_i)) == 0
);

member (RingElement,CoincidentRootLocus) := (F,X) -> (
  checkBinaryForm F;
  if first degree F =!= X#"ambient" then error("expected a binary form of degree "|toString(X#"ambient"));
  if char coefficientRing ring F =!= char coefficientRing X then error("expected a binary form over "|toString(coefficientRing X));
  if (map X)#"idealImage" === null then (
      if codim X == 1 then return discriminant F == 0;
      if coefficientRing ring F === coefficientRing X then return dim((map X)^* switch2(F,Variable=>X#"variable")) - dim(X) >= 0;
  );
  t := gens ring X;
  c := switch1 F;
  return sub(ideal X,apply(#t,i -> t_i => c_i)) == 0;
);

member (RingElement,JoinOfCoincidentRootLoci) := (F,Z) -> (
  checkBinaryForm F;
  if first degree F =!= Z#"ambient" then error("expected a binary form of degree "|toString(Z#"ambient"));
  if char coefficientRing ring F =!= char coefficientRing Z then error("expected a binary form over "|toString(coefficientRing Z));
  if Z#"ideal" =!= null or coefficientRing ring F =!= coefficientRing Z then return sub(ideal Z,apply(gens ring Z,switch1 F,(t,c) -> t => c)) == 0;
  X := Z#"dual";
  l := flatten entries compress matrix {apply(partition X,j -> j-1)};
  J := apolar(F,sum l);
  if J == 0 then return false;
  L := switch2(J,Variable=>X#"variable");
  Y := coincidentRootLocus(l,coefficientRing X,Variable=>X#"variable");
  f := map Y;
  return if (map Y)#"idealImage" === null then dim(f^* L) - (#l) >= 0 else dim((ideal Y) + L) >= 1;
);

random (CoincidentRootLocus) := o -> (X) -> (
   f := map X;
   p := f ideal apply(entries diagonalMatrix toList((dim X):1),u -> random(u,source f));
   switch2 p
);

random (JoinOfCoincidentRootLoci) := o -> (Z) -> (
   X := Z#"dual";
   f := map X;
   J :=sub(transpose jacobian matrix f,apply(gens source f,x -> x => random coefficientRing X));
   T := ideal image basis(1,intersect apply(entries transpose mingens image J, u -> minors(2,(vars target f)||matrix{u})));
   assert(dim T -1 == dim X);
   H := ideal sum(T_*,g -> random(coefficientRing X) * g);
   switch2 pairingIsomorphism dualVariety H
);

generic = method(Options => {Variable => null, Reduce => false})

generic (CoincidentRootLocus) := o -> (X) -> (
   x := if o.Variable === null then (if toString(X#"variable") =!= toString(VAR) then VAR else "x") else o.Variable;
   M := if o.Reduce then matrix X#"map2" else matrix map X;
   R := Grass(0,1,frac ring M,Variable=>x);
   F := switch1(flatten entries sub(M,frac ring M),R);
   R':= (ring numerator 1_(coefficientRing R))[gens R];
   sub(F,R')
);

generic (PolynomialRing) := o -> (R) -> ( -- undocumented
   x := if o.Variable === null then VAR else o.Variable;
   P := Grass(0,1,frac R,Variable=>x);
   F := switch1(flatten entries sub(vars R,frac R),P);
   P':= (ring numerator 1_(coefficientRing P))[gens P];
   sub(F,P')
);

describe (CoincidentRootLocus) := (X) -> (
   S := "Coincident root locus associated with the partition "|toString partition X|" defined over "|toString coefficientRing X|newline;
   S = S|"ambient: P^"|toString X#"ambient"|" = Proj("|toString ring X|")"|newline;
   S = S|"dim    = "|toString dim X|newline;
   S = S|"codim  = "|toString codim X|newline;
   S = S|"degree = "|toString degree X|newline;
   SingX := singularLocus X;
   if #SingX == 0 
   then S = S|"The singular locus is empty"
   else if #SingX == 1
   then S = S|"The singular locus coincides with the coincident root locus associated with the partition "|toString partition first SingX
   else S = S|"The singular locus is the union of the coincident root loci associated with the partitions: "|newline|toString toSequence apply(SingX,partition);
   if codim X == 1 and (map X)#"idealImage" =!= null then S = S|newline|"The defining polynomial has "|toString(# terms (ideal X)_0)|" terms of degree "|toString degree X;
   return S;
);

describe (JoinOfCoincidentRootLoci) := (X) -> (
   S := "Dual of the coincident root locus associated with the partition "|toString partition dual X|" defined over "|toString coefficientRing X|newline;
   S = S|"which coincides with the join of the coincident root loci associated with the partitions:"|(if #(X#"listCRloci") > 2 then newline else " ")|toString toSequence apply(X#"listCRloci",partition)|newline;
   S = S|"ambient: P^"|toString X#"ambient"|" = Proj("|toString ring X|")"|newline;
   S = S|"dim    = "|toString dim X|newline;
   S = S|"codim  = "|toString codim X;
   if X#"degree" =!= null then S = S|newline|"degree = "|toString degree X;
   if codim X == 1 and X#"ideal" =!= null then S = S|newline|"The defining polynomial has "|toString(# terms (ideal X)_0)|" terms of degree "|toString degree X;
   return S;
);

refinedPartitions = method()
refinedPartitions (List) := (l) -> (
   L := flatten for h to #l-1 list if l_h > 1 then for i from 1 to l_h - 1 list rsort(l_{0..h-1} | {i,l_h - i}| l_{(h+1..#l-1)}) else continue;
   if #L == 0 then return {l} else return rsort unique ({l} | L | flatten apply(L,refinedPartitions))
);

coarseningPartitions = method()
coarseningPartitions (List) := (l) -> (
   if #l== 1 then return {l};
   rsort({l} | unique flatten apply(unique flatten for i to #l-1 list for j from i+1 to #l-1 list rsort append(l_{0..i-1}|l_{i+1..j-1}|l_{j+1..#l-1},l_i+l_j),coarseningPartitions))
);

isRefinement = method(); -- l1 <= l2
isRefinement (List,List) := (l1,l2) -> member(l1,coarseningPartitions l2);

isSubset(CoincidentRootLocus,CoincidentRootLocus) := (X,Y) -> (
   if ring X =!= ring Y then return false;
   isRefinement(partition X,partition Y)
);

CoincidentRootLocus ? CoincidentRootLocus := (X,Y) -> (
   if X == Y 
   then return symbol == 
   else if isSubset(X,Y) 
   then return symbol <
   else if isSubset(Y,X)
   then return symbol >
   else if dim X < dim Y 
   then return symbol <
   else if dim X > dim Y
   then return symbol >
   else return incomparable;
);

supsets = method();
supsets (CoincidentRootLocus) := (X) -> sort apply(refinedPartitions partition X,l -> coincidentRootLocus(l,coefficientRing X,Variable=>X#"variable"));

subsets (CoincidentRootLocus) := (X) -> sort apply(coarseningPartitions partition X,l -> coincidentRootLocus(l,coefficientRing X,Variable=>X#"variable"));

affineMap = method(TypicalValue => RingMap)

affineMap (CoincidentRootLocus) := (X) -> (
   f := X#"map2";
   x := gens source f;
   n := #x -1;
   K := coefficientRing f;
   F := sub(sub(matrix f,x_0=>1),K[x_{1..n}]);
   map(ring F,target f,F)
);

affineMap (JoinOfCoincidentRootLoci) := (X) -> ( -- Thm 2.5 [Lee, Sturmfels - Duality of multiple root loci]
   K := coefficientRing X;
   n := X#"ambient";
   l := select(partition X,i -> i>1);
   s := local s; t := local t; u := local u;
   T := K[s_0..s_(#l-1),t_0..t_(#l-1),flatten for i to #l-1 list for j to l_i-3 list u_(i,j)];
   y := local y; 
   F := T[y_0,y_1];
   f := switch1 sum for i to #l-1 list s_i * (switch1({1_T,t_i},F))^(n-l_i+2) * switch1(prepend(1_T,toList(u_(i,0)..u_(i,l_i-3))),F);
   map(T,ring X,f)
);

chowForm (CoincidentRootLocus) := o -> (X) -> chowForm(map X#"map2",Variable=>o.Variable);

isInCoisotropic (Ideal,CoincidentRootLocus) := o -> (L,X) -> (
   if not (isPolynomialRing ring L and numgens ring L == 2 and coefficientRing ring L === coefficientRing X and isHomogeneous L and unique flatten degrees L === {X#"ambient"}) then error("expected an ideal generated by binary forms of degree "|toString(X#"ambient")|" over "|toString(coefficientRing X));
   return isInCoisotropic(sub(switch2 L,vars ring X),ideal X,Duality=>o.Duality);
);

randomInCoisotropic = method()
randomInCoisotropic (Ideal,ZZ,Ideal) := (I,i,p) -> (
   if ring p =!= ring I then error "common ring not found";
   if not isPolynomialRing ring I then error "expected a polynomial ring";
   if not (isHomogeneous I and isHomogeneous p) then error "expected a homogeneous ideal";
   if not (dim p == 1 and degree p == 1 and unique degrees p == {{1}}) then error "expected last argument to be the ideal of a point";
   if not isSubset(I,p) then error "expected a point of the variety";
   subs := apply(gens ring I,flatten entries coefficients parametrize p,(x,s) -> x => s);
   T := trim ideal((vars ring I) * sub(jacobian I,subs));
   K := coefficientRing ring I;
   c := codim I;
   n := numgens ring I -1; 
   d := n - c; -- dim I -1
   L := trim(T + ideal apply(d-i,i -> ((gens p) * random(K^n,K^1))_(0,0)));
   m := numgens L;
   H := trim ideal apply(n-c+1-i,i -> ((gens L) * random(K^m,K^1))_(0,0));
   (H,(dim H - 1 == c - 1 + i and dim T - 1 == d and dim L - 1 == i and isSubset(T,L) and isSubset(L,p)))
);
randomInCoisotropic (Ideal,ZZ) := (I,i) -> (
   local H; condition := false;
   while not condition do (H,condition) = randomInCoisotropic(I,i,point I);
   H
);

randomInCoisotropic (CoincidentRootLocus,ZZ) := (X,i) -> (
   local H; condition := false;
   while not condition do (H,condition) = randomInCoisotropic(ideal X,i,switch2 random X);
   J := switch2 H;
   if isIdeal J then J else ideal J
);

CoincidentRootLocus + CoincidentRootLocus := (X,Y) -> ( -- intersection, undocumented
   if ring X =!= ring Y then error "expected same ring";
   S := toList((set subsets X) * (set subsets Y));
   E := S;
   for A in S do for B in select(subsets A,s -> dim A - dim s > 0) do E = delete(B,E);
   if #E == 1 then first E else E
);

tangentSpace = method() -- undocumented
tangentSpace (CoincidentRootLocus,RingElement) := (X,F) -> (
   checkBinaryForm F;
   if first degree F =!= X#"ambient" then error("expected a binary form of degree "|toString(X#"ambient"));
   if coefficientRing ring F =!= coefficientRing X then error("expected a binary form over "|toString(coefficientRing X));
   if not member(F,X) then error("expected a binary form belonging to "|toString net X);
   J := sub(jacobian ideal X,apply(gens ring X,switch1 F,(x,s) -> x => s));
   sub(switch2 ideal((vars ring X) * J),vars ring F)
);
tangentSpace (Ideal,Ideal) := (I,p) -> (
   if ring p =!= ring I then error "common ring not found";
   if not isPolynomialRing ring I then error "expected a polynomial ring";
   if not (isHomogeneous I and isHomogeneous p) then error "expected homogeneous ideals";
   if not (dim p == 1 and degree p == 1 and unique degrees p == {{1}}) then error "expected second argument to be the ideal of a point";
   if not isSubset(I,p) then error "expected a point of the variety";
   subs := apply(gens ring I,flatten entries coefficients parametrize p,(x,s) -> x => s);
   trim ideal((vars ring I) * sub(jacobian I,subs))
);

--================================================================
--===== Polar degrees of CRL =====================================
--================================================================

polarDegrees = method();
polarDegrees (CoincidentRootLocus) := (X) -> (
   lambda := partition X;
   n := #lambda;
   e := n - (X#"multiplicities")_1;
   -- to be replaced "degree dual CRL" with the explicit Oeding formula
   P := for j to e list lift((sum(conj(lambda,j),u -> (last u) * degree dual CRL(first u,ZZ/101)))/(n-j+1),ZZ);
   P | toList(n-e : 0)
);

conj = method();
conj (List,ZZ) := (lambda,j) -> (
   n := #lambda;
   r := sum lambda;
   d := n + r - j;
   D := descendantsPartitions(lambda,j);
   F := apply(D,a -> (apply(a|toList((n-#a):0),i->i+1),partitionMultiplicity(a,lambda)));
   select(F,y -> #select(first y,u -> u==1) == 0)
);

partitionMultiplicity = method()
partitionMultiplicity (List,List) := (lambda',lambda) -> (
   try assert(ring matrix {lambda} === ZZ and min lambda > 0 and ring matrix {lambda'} === ZZ and min lambda' > 0) else error "expected lists of positive integers";
   lambda = rsort lambda;
   lambda' = rsort lambda';
   n := #lambda;
   lambda' = lambda'|toList((n-#lambda'):0);
   r := sum lambda;
   r' := sum lambda';
   j := r-r';
   J := toList (set {0,1})^**n;
   for i to n-3 do J = apply(J,splice);
   J = select(apply(J,toList),a -> sum a == j);
   A := select(J,a -> rsort(lambda' + a) == lambda);
   #A
);

descendantsPartitions = method()
descendantsPartitions (List,ZZ) := (lambda,j) -> (
   try assert(ring matrix {lambda} === ZZ and min lambda > 0) else error "expected a list of positive integers";
   lambda = rsort lambda;
   n := #lambda;
   J := toList (set {0,1})^**n;
   for i to n-3 do J = apply(J,splice);
   J = select(apply(J,toList),a -> sum a == j);
   unique apply(J,a -> rsort flatten entries compress matrix{lambda - a})
);

--================================================================
--=== Real algebraic boundary ====================================
--================================================================

realRankBoundary = method(Options => {Variable => VAR})

realRankBoundary (ZZ,ZZ,Ring) := o -> (n,k,K) -> (
   if not (1 <= k and ceiling((n+1)/2) <= k and k <= n) then return {};
   if k == n then return coincidentRootLocus(toList prepend(2,n-2:1),K,Variable=>o.Variable);
   if n == 4 and k == 3 then return coincidentRootLocus({2,1,1},K,Variable=>o.Variable);
   if n == 5 and k == 4 then return {realRankBoundary(5,3,K,Variable=>o.Variable),realRankBoundary(5,5,K,Variable=>o.Variable)};
   if n == 6 and k == 5 then return append(realRankBoundary(6,4,K,Variable=>o.Variable),realRankBoundary(6,6,K,Variable=>o.Variable));
   if k == ceiling((n+1)/2) then (
       if odd n then (
           return dual coincidentRootLocus(toList prepend(3,k-2:2),K,Variable=>o.Variable);
       ) else ( 
           return {dual coincidentRootLocus({3,3}|toList(k-4:2),K,Variable=>o.Variable),dual coincidentRootLocus(toList prepend(4,k-3:2),K,Variable=>o.Variable)};
       );
   );
   if n == 7 and k == 5 then (
       return {dual coincidentRootLocus({3,2,2},K,Variable=>o.Variable),dual coincidentRootLocus({4,3},K,Variable=>o.Variable),dual coincidentRootLocus({5,2},K,Variable=>o.Variable)};
   );
   if n == 7 and k == 6 then (
       return {dual coincidentRootLocus({4,3},K,Variable=>o.Variable),dual coincidentRootLocus({5,2},K,Variable=>o.Variable),dual coincidentRootLocus({7},K,Variable=>o.Variable)};
   );
   if n == 8 and k == 6 then (
       return {dual coincidentRootLocus({3,3,2},K,Variable=>o.Variable),dual coincidentRootLocus({4,2,2},K,Variable=>o.Variable),dual coincidentRootLocus({4,4},K,Variable=>o.Variable),dual coincidentRootLocus({5,3},K,Variable=>o.Variable),dual coincidentRootLocus({6,2},K,Variable=>o.Variable)};
   );
   if n == 8 and k == 7 then (
       return {dual coincidentRootLocus({4,4},K,Variable=>o.Variable),dual coincidentRootLocus({5,3},K,Variable=>o.Variable),dual coincidentRootLocus({6,2},K,Variable=>o.Variable),dual coincidentRootLocus({8},K,Variable=>o.Variable)};
   );
   error "not implemented yet";
);

realRankBoundary (ZZ,ZZ) := o -> (n,k) -> realRankBoundary(n,k,KK,Variable=>o.Variable);

--================================================================
--=== Apolarity ==================================================
--================================================================

catalecticantMatrix = method()

catalecticantMatrix (RingElement,ZZ) := (F,k) -> ( 
   mm := flatten entries gens (ideal vars ring F)^k;
   mm':= flatten entries gens (ideal vars ring F)^(first degree F - k);
   D := matrix {apply(mm,t -> diff(t,F))};
   sub(last coefficients(D,Monomials=>mm'),coefficientRing ring F)
);

catalecticantMatrix (PolynomialRing,ZZ) := (R,k) -> (
   K := coefficientRing R;
   n := numgens R -1;
   x := gens R;
   H := matrix for i to n-k list for j to k list x_(i+j);
   H * (coefficients pairingIsomorphism(k,K))^-1
);

Trim = method()
Trim (Ideal) := (I) -> (
   if not isPolynomialRing ring I then error "expected an ideal in a polynomial ring";
   K := coefficientRing ring I;
   if isField K and class K =!= FractionField then return trim I;
   n := numgens ring I -1;
   x := local x;
   local R; local R'; local K'; local I';
   if not isField K then (
       K' = frac K;
       R = K[x_0..x_n];
       R'= K'[x_0..x_n];
       I'= trim sub(sub(I,vars R),R');
   ) else (
       K' = K;
       K = ring numerator(1_K');
       R = K[x_0..x_n];
       R'= K'[x_0..x_n];
       I'= trim sub(I,vars R');
   );
   if I' == 0 then return ideal ring I;
   return sub(trim ideal matrix{apply(I'_*,g -> sub(g * lcm apply(flatten entries sub(last coefficients g,K'),denominator),R))},vars ring I);
);

apolar = method(Options => {Variable => VAR})

apolar (RingElement) := o -> (F) -> (
   checkBinaryForm F;
   K := coefficientRing ring F;
   d := first degree F;
   I := Trim sum(d+1, k -> apolar(F,k));
   try assert((numgens I == 1 and first degree I_0 == 1) or (numgens I == 2 and sum apply(degrees I,first) == d+2)) else error "internal error encountered";
   I
);

apolar (RingElement,ZZ) := o -> (F,k) -> (
   checkBinaryForm F;
   K := coefficientRing ring F;
   if not isField K then (
       K':= frac K;
       x := local x;
       y := local y;
       R := K[x,y];
       R':= K'[x,y];
       F':= sub(sub(F,vars R),R');
       return sub(sub(Trim apolar(F',k),R),vars ring F);
   );
   A := catalecticantMatrix(F,k);
   I := (gens (ideal vars ring F)^k) * (mingens kernel A);
   Trim ideal I
);

apolar (ZZ,ZZ,Ring) := o -> (n,s,K) -> (-- apolar map P^n --> G(2*s-n-1,s,K)  ~ G(n-s,s)
   if n > 2*s then error "expected integers n,s with n < 2*s+1";
   Pn := Grass(0,n,K,Variable=>o.Variable);
   H := catalecticantMatrix(Pn,s);
   f := rationalMap dualize map rationalMap(Pn,Grass(n-s,s,K,Variable=>o.Variable),gens minors(n-s+1,H));
   assert(target f === Grass(2*s-n-1,s,K,Variable=>o.Variable));
   f
);

apolar (ZZ,ZZ) := o -> (n,s) -> apolar(n,s,KK,Variable=>o.Variable); 

recover = method()

recover (Ideal) := (I) -> (
   x := local x;
   K := coefficientRing ring I;
   if not isField K then try (
      I' := sub(I,vars Grass(0,1,frac(K),Variable=>x));
      F' := recover I';
      assert(K === ring numerator(1_(coefficientRing ring F')));
      R := K[gens ring F'];
      return sub(sub((Trim ideal F')_0,R),vars ring I);
   );
   if not (isPolynomialRing ring I and isHomogeneous I) then error "expected a homogeneous ideal in a polynomial ring";
   if # unique degrees I > 1 then if numgens I == 2 then I = ideal image basis(max flatten degrees I,I) else error "expected an ideal generated by forms of the same degree";
   M := matrix apply(I_*,switch1);
   p := plucker(M,Variable=>x);
   k := numgens target M -1;
   N := numgens source M -1;
   f := apolar(2*N-k-1,N,K,Variable=>x);
   q := f^* p;
   if degrees q =!= toList((2*N-k-1):{1}) then if dim I >= 1 then error "the ideal is not apolar";
   F := sub(switch2(q,Variable=>x),vars ring I);
   (Trim ideal F)_0
);

recover (RingElement,RingElement) := (g,h) -> recover ideal(g,h);

--================================================================
--=== Real rooted forms and Qepcad ===============================
--================================================================

isSomeLinearCombinationRealRooted = method(TypicalValue => Boolean, Options => {QepcadOptions => (20000000,10000), Range => (-infinity,infinity), Verbose => false})
isSomeLinearCombinationRealRooted (Matrix) := o -> (F) -> ( 
   if numgens target F != 1 then error "expected a matrix with one row";
   if not (isPolynomialRing ring F and numgens ring F == 2) then error "expected a matrix with entries in a polynomial ring with two variables";
   K := coefficientRing ring F;
   x := local x;
   y := local y;
   Txy := K[x,y];
   F = sub(F,vars Txy);
   Fe := F;
   if K =!= QQ then (
       if not(isPolynomialRing K and coefficientRing K === QQ and numgens K <= 1) then error "expected coefficient ring of the form QQ or QQ[e]";
       Fe = sub(F,frac(coefficientRing K)[gens Txy]);
   );
   if not isHomogeneous ideal Fe then error "expected a homogeneus matrix";
   d := unique apply(flatten entries Fe,degree);
   if #d > 1 then error "expected forms of the same degree" else d = first d;
   if #d > 1 then error "expected a standard graded polynomial ring" else d = first d;
   n := numgens source F -1;
   if n == 0 and K === QQ then (
       Tx := K[x];
       f := sub(sub(F_(0,0),y=>1),Tx);
       if o.Verbose then <<"--computing bezoutian matrix of a binary form of degree "<<d<<endl;
       if first degree f < d-1 then return false else return (#select(leadingPrincipalMinors bezoutianMatrix f,m -> m <= 0) == 0);
   );
   t := local t;
   Rxy := K[t_0..t_n][x,y];
   Rx := K[t_0..t_n][x];
   q := (sub(F,Rxy) * transpose(vars coefficientRing Rxy))_(0,0);
   q' := sub(sub(q,y=>1),Rx);
   d' := first degree q';
   if d' < d-1 then return false;
   tn := last gens coefficientRing Rx;       
   Sx := if n-1 >= 0 then K[t_0..t_(n-1)][x] else K[x];       
   q1 := sub(sub(q',tn => 1),Sx);
   isRealRootedViaQEPCAD(q1,QepcadOptions=>o.QepcadOptions,Range=>o.Range,Verbose=>o.Verbose)
);

isRealRootedViaQEPCAD = method(Options => {QepcadOptions => (20000000,10000), Range => (-infinity,infinity), Verbose => false}) 
isRealRootedViaQEPCAD (RingElement) := o -> (F) -> (
   if not isPolynomialRing ring F then error "expected a polynomial";
   if not isPolynomialRing coefficientRing ring F then error "expected a polynomial with polynomial coefficients";
   if first o.QepcadOptions < 20000000 or last o.QepcadOptions < 10000 then <<"--warning: small number of cells in the garbage collected space"<<endl;
   try assert((class o.Range === Sequence or class o.Range === Array) and # o.Range == 2) else error "Range option accepts a sequence or an array of length 2";
   if numgens ring F != 1 then error "expected a univariate polynomial";
   K := (coefficientRing coefficientRing ring F)[vars(0 .. (numgens coefficientRing ring F)-1)];
   z := local z; 
   R := K[z];
   F' := sub(F,apply(numgens K,j -> (gens coefficientRing ring F)_j => (gens K)_j) | {first gens ring F => z});
   try assert(F == sub(F',apply(numgens K,j -> (gens K)_j => (gens coefficientRing ring F)_j) | {z => first gens ring F})) else error "something went wrong";
   S := "[Existence of real values of "|toString unsequence toSequence gens K|" such that a given polynomial in "|toString(R)|" is real-rooted]"|newline;
   gg := toSequence((gens coefficientRing K)|(gens K)|(gens R));
   S = S | (if #gg =!= 1 then toString gg else "("|toString(first gg)|")") | newline;
   S = S | toString(numgens coefficientRing K) | newline;
   S = S | concatenate apply(gens K,g -> "(F "|toString(g)|") ") | "(X " | toString(first degree F') | " " | toString(z) | ") " | "["|replace("\\*"," ",replace("\\+"," + ",replace("-"," - ",toString F'))) | " = 0]. " | newline;
   if numgens coefficientRing K > 0 and (first o.Range =!= -infinity or last o.Range =!= infinity) then (
      ass1 := toString(first o.Range) | (if class o.Range === Sequence then " < " else " <= ") | toString(first gens coefficientRing K);
      ass2 := toString(first gens coefficientRing K) | (if class o.Range === Sequence then " < " else " <= ") | toString(last o.Range);
      ass := if first o.Range =!= -infinity and last o.Range =!= infinity 
             then "[ " | ass1 | " /\\ " | ass2 | " ]"
             else if first o.Range =!= -infinity then "[ " | ass1 | " ]" else "[ " | ass2 | " ]";
      if o.Verbose then <<"--assumption: " << ass << endl;
      S = S | "assume " | ass | newline;
   );
   S = S | "finish " | newline;
   dir := rootPath | temporaryFileName();
   (dir|"-input") << S << close;
   C := "qepcad +N"|toString(first o.QepcadOptions)|" +L"|toString(last o.QepcadOptions)|" < '"|dir|"-input'";
   if o.Verbose then << "--running " << C << endl;
   try (Out := get("!"|C); if CLEAR then removeFile(dir|"-input")) else error "error occured while executing QEPCAD. Please make sure that it is installed and configured correctly.";
   jj := toList first regex("An equivalent quantifier-free formula:"|newline|newline,Out);
   V := substring(Out,sum jj,5);
   if V === "TRUE"|newline then return true else if V === "FALSE" then return false else (
      lOut := first first regex(newline|"=====================  The End  =======================",Out);
      error("unable to interpret QEPCAD output:"|newline|newline|substring(Out,sum jj,lOut - (sum jj) -2));
   );
);

realrank = method(Options => {QepcadOptions => (20000000,10000), Range => (-infinity,infinity), Verbose => false, Limit => infinity});
realrank (RingElement) := o -> (F) -> (
   checkBinaryForm F;
   K := coefficientRing ring F;
   if K =!= QQ and not(isPolynomialRing K and coefficientRing K === QQ and numgens K <= 1) then error "expected coefficient ring of the form QQ or QQ[e]";
   d := first degree F;
   if d == 5 and K === QQ then (if o.Verbose then <<"--trying to apply Comon and Ottaviani's algorithm for the real rank of a generic quintic"<<endl; try return realrank5 F else if o.Verbose then <<"--method failed due to the lack of genericity"<<endl;);
   Jl := apply(d+1,k -> apolar(F,k));
   J := Trim sum Jl;
   if numgens J == 1 then if first degree(J_0) == 1 then return 1;
   if numgens J != 2 then error "internal error encountered";
   (e1,e2) := toSequence sort apply(degrees J,first);
   if e1+e2 =!= d+2 then error "internal error encountered";
   g1 := first select(J_*,g -> first degree g == e1);
   g2 := first select(J_*,g -> first degree g == e2);
   if o.Verbose then <<"--detected that the real rank belongs to "<<toString unique deepSplice{e1,e2..d}<<endl;
   if o.Verbose then (if e1 < e2 then <<"--verifying if the real rank is "<< e1 <<endl else <<"--preliminary tests to verify if the real rank is "<< e2 <<endl);
   if isSomeLinearCombinationRealRooted(matrix g1,QepcadOptions=>o.QepcadOptions,Range=>o.Range,Verbose=>o.Verbose) then return e1;
   if o.Verbose then if e1 < e2 then <<"--preliminary test to verify if the real rank is "<< e2 <<endl;
   if isSomeLinearCombinationRealRooted(matrix g2,QepcadOptions=>o.QepcadOptions,Range=>o.Range,Verbose=>o.Verbose) then return e2;
   for j from e2 to d-2 do (
      if j >= o.Limit then return (j..d);
      if o.Verbose then <<"--verifying if the real rank is "<< j <<endl;
      if isSomeLinearCombinationRealRooted(gens Jl_j,QepcadOptions=>o.QepcadOptions,Range=>o.Range,Verbose=>o.Verbose) then return j else continue;
   );
   if d-1 >= o.Limit then return (d-1,d);
   if K =!= QQ or d < 3 or discriminant F == 0 then (
      if o.Verbose then <<"--verifying if the real rank is "<< d-1 <<endl;
      if isSomeLinearCombinationRealRooted(gens Jl_(d-1),QepcadOptions=>o.QepcadOptions,Range=>o.Range,Verbose=>o.Verbose) then return (d-1) else return d;
   ) else (
      if isSomeLinearCombinationRealRooted(matrix F,QepcadOptions=>o.QepcadOptions,Range=>o.Range,Verbose=>o.Verbose) then return d else return (d-1);
   );
);

realrank5 = method()
realrank5 (RingElement) := (F) -> (
   if first degree F != 5 then error "expected a binary form of degree 5";
   if discriminant F == 0 then error "method failed due to the lack of genericity";
   if isSomeLinearCombinationRealRooted matrix F then return 5;
   a := switch1 F;
   x := local x; y := local y;
   R := QQ[x,y];
   g := (det matrix {{a_1,a_2,a_3},{a_2,a_3,a_4},{a_3,a_4,a_5}})*x^3   -
        (det matrix {{a_0,a_2,a_3},{a_1,a_3,a_4},{a_2,a_4,a_5}})*x^2*y + 
        (det matrix {{a_0,a_1,a_3},{a_1,a_2,a_4},{a_2,a_3,a_5}})*x*y^2 - 
        (det matrix {{a_0,a_1,a_2},{a_1,a_2,a_3},{a_2,a_3,a_4}})*y^3;
   if discriminant g == 0 then error "method failed due to the lack of genericity";
   if isSomeLinearCombinationRealRooted matrix g then return 3 else return 4;
);

complexrank = method(Options => {Limit => infinity}); 
complexrank (RingElement) := o -> (F) -> (
   checkBinaryForm F;
   if not isField coefficientRing ring F then error "the coefficient ring needs to be a field";
   d := first degree F;
   Jl := apply(d+1, k -> apolar(F,k));
   J := Trim sum Jl;
   if numgens J == 1 then if first degree(J_0) == 1 then return 1;
   if numgens J != 2 then error "internal error encountered";
   (e1,e2) := toSequence sort apply(degrees J,first);
   if e1+e2 =!= d+2 then error "internal error encountered";
   g1 := first select(J_*,g -> first degree g == e1);
   g2 := first select(J_*,g -> first degree g == e2);
   if discriminant g1 != 0 then return e1;
   if discriminant g2 != 0 then return e2;
   K := coefficientRing ring F;
   local I; local g;
   for j from e2 to d-1 do (
      if j >= o.Limit then return (j..d);
      I = Jl_j;
      g = (gens I * random(K^(numgens I),K^1))_(0,0);
      if discriminant g != 0 then return j else continue;
   );
   return d;
);

matriceCompagna = method(TypicalValue => Matrix)
matriceCompagna (RingElement,RingElement) := (f,h) -> (
   if not (ring f === ring h and isPolynomialRing ring f and numgens ring f == 1) then error "expected a univariate polynomial";
   x := first gens ring f;
   K := coefficientRing ring f;
   d := first degree f;
   f = (leadCoefficient f)^-1 * f;
   R := (ring f)/f;
   b := sub(matrix{apply(d,i -> x^i)},R);
   sub(last coefficients(h * b,Monomials=>b),K)
);

bezoutianMatrix = method(TypicalValue => Matrix)
bezoutianMatrix (RingElement) := (f) -> (
   if not (isPolynomialRing ring f and numgens ring f == 1) then error "expected a univariate polynomial";
   if not isUnit leadCoefficient f then (z := local z; f = sub(f,vars((frac coefficientRing ring f)[z])));
   x := first gens ring f;
   d := first degree f;
   v := apply(2*d-1,i -> trace matriceCompagna(f,x^i));
   matrix table(d,d,(i,j) -> v_(i+j))
);

leadingPrincipalMinors = method(TypicalValue => Boolean)
leadingPrincipalMinors (Matrix) := (M) -> (
   try assert(M - transpose M == 0) else error "expected a symmetric matrix";
   for i to numgens source M -1 list det submatrix(M,{0..i},{0..i})
);

realroots = method(Options => {Verbose => false})

realroots (RingElement) := o -> (F) -> (
   checkBinaryForm F;
   K := coefficientRing ring F;
   if K =!= QQ then error "expected coefficient ring to be QQ";
   x := local x;
   y := local y;
   R := K[x,y];
   F = sub(F,vars R);
   f := sub(sub(F,y => 1),K[x]);
   ro := toList(((first degree F) - (first degree f)) : [1.0,0.0]);
   ro = ro | apply(select(roots f,a -> abs(imaginaryPart a) < 0.0000001),r -> [realPart r,1.0]);
   if o.Verbose then (
      <<"number real roots: "<<#ro<<endl;
      <<"number distinct real roots: "<<#unique ro<<endl;
      <<"number non-real roots: "<<(first degree F) - #ro <<endl;
   );
   ro
);

--================================================================
--=== Joins ======================================================
--================================================================

projectiveJoin = method(Dispatch => Thing, Options => {SubringLimit => infinity});
projectiveJoin (Thing) := o -> U -> (
    U = sequence U;
    if #U == 0 then return;
    if apply(U,class) === (#U):RingMap
    then return joinOfAffineParameterizations(U,SubringLimit=>o.SubringLimit) 
    else if apply(U,class) === (#U):RationalMap
    then return joinOfParameterizations(U,SubringLimit=>o.SubringLimit) 
    else if apply(U,class) === (#U):Ideal 
    then (
       if #U <= 2 then return joinOfIdeals(U,SubringLimit=>o.SubringLimit);
       if #U >= 3 then return projectiveJoin(projectiveJoin unsequence toSequence(U_{0..#U-2}),last U,SubringLimit=>o.SubringLimit);
    ) else if apply(U,class) === (#U):CoincidentRootLocus 
    then return joinOfHooks U
    else error "expected a sequence of ideals, or rational maps, or ring maps, or coincident root loci";
);

joinOfIdeals = method(TypicalValue => Ideal, Dispatch => Thing, Options => {SubringLimit => infinity});
joinOfIdeals (Thing) := o -> U -> (
    U = sequence U;
    r := #U-1;
    if r == -1 then return;
    if apply(U,class) =!= (r+1):Ideal then error "expected a sequence of ideals";
    if # unique apply(U,ring) > 1 then error "expected same ring";
    K := coefficientRing ring first U;
    if not isField K then error "the coefficient ring needs to be a field";
    ringPn := ring first U;
    if not isPolynomialRing ringPn then error "expected ideals in a polynomial ring";
    if apply(U,isHomogeneous) =!= (r+1):true then error "expected homogeneous ideals";
    if r == 0 then return trim first U;
    n := numgens ringPn -1;
    x := local x;
    T := for i to r list K[x_(0,i)..x_(n,i)];
    t := local t; z := local z;
    R := K[t_0..t_r,flatten apply(T,gens),z_0..z_n,MonomialOrder=>Eliminate ((n+2)*(r+1))];
    W := ideal(matrix{{z_0..z_n}} - sum(r+1,i -> t_i * sub(vars T_i,R)));
    W = trim(W + sum(r+1,i -> sub(sub(U_i,vars T_i),R)));
    trim sub(sub(ideal selectInSubring(1,gens gb(W,SubringLimit=>o.SubringLimit)),K[z_0..z_n]),vars ringPn)
);
 
joinOfParameterizations = method(TypicalValue => Ideal, Dispatch => Thing, Options => {SubringLimit => infinity});
joinOfParameterizations (Thing) := o -> U -> (
    U = sequence U;
    r := #U-1;
    if r == -1 then return;
    if apply(U,class) =!= (r+1):RationalMap then error "expected a sequence of rational maps";
    if # unique apply(U,target) > 1 then error "expected maps with the same target";
    ringPN := target first U;
    if not isPolynomialRing ringPN then error "expected maps between projective spaces";
    for f in U do if not isPolynomialRing source f then error "expected maps between projective spaces";
    if r == 0 then return trim kernel(map first U,SubringLimit=>o.SubringLimit);
    N := numgens ringPN -1;
    x := local x;
    K := coefficientRing first U;
    T := for i to r list K[x_(0,i)..x_(numgens source U_i -1,i)];
    t := local t; z := local z;
    R := K[t_0..t_r,flatten apply(T,gens),z_0..z_N,MonomialOrder=>Eliminate (r+1+sum(U,f -> numgens source f))];
    W := trim ideal(matrix{{z_0..z_N}} - sum(r+1,i -> t_i *  sub(sub(matrix U_i,vars T_i),R)));
    trim sub(sub(ideal selectInSubring(1,gens gb(W,SubringLimit=>o.SubringLimit)),K[z_0..z_N]),vars ringPN)
);

joinOfAffineParameterizations = method(TypicalValue => Ideal, Dispatch => Thing, Options => {SubringLimit => infinity});
joinOfAffineParameterizations (Thing) := o -> U -> (
    U = sequence U;
    r := #U-1;
    if r == -1 then return;
    if apply(U,class) =!= (r+1):RingMap then error "expected a sequence of ring maps";
    if # unique apply(U,source) > 1 then error "expected ring maps with the same source";
    ringPN := source first U;
    if not isPolynomialRing ringPN then error "expected maps between polynomial rings";
    for f in U do if not isPolynomialRing target f then error "expected maps between polynomial rings";
    if r == 0 then return trim kernel(first U,SubringLimit=>o.SubringLimit);
    N := numgens ringPN -1;
    x := local x;
    K := coefficientRing ringPN;
    T := for i to r list K[x_(0,i)..x_(numgens target U_i -1,i)];
    t := local t; z := local z;
    R := K[t_0..t_r,flatten apply(T,gens),z_0..z_N,MonomialOrder=>Eliminate (r+1+sum(U,f -> numgens target f))];
    W := trim ideal(matrix{{z_0..z_N}} - sum(r+1,i -> t_i *  sub(sub(matrix U_i,vars T_i),R)));
    trim sub(sub(ideal selectInSubring(1,gens gb(W,SubringLimit=>o.SubringLimit)),K[z_0..z_N]),vars ringPN)
);

joinOfHooks = method(TypicalValue => JoinOfCoincidentRootLoci, Dispatch => Thing);
joinOfHooks (Thing) := U -> (
    U = sequence U;
    r := #U-1;
    if r == -1 then return;
    if apply(U,class) =!= (r+1):CoincidentRootLocus then error "expected a sequence of coincident root loci";
    R := ring first U;
    n := numgens R -1;
    local p; local p0; 
    l := for i to #U-1 list (
            if ring U_i =!= R then error "expected a sequence of coincident root loci with the same ring";
            p = partition U_i;
            p0 = first p;
            if p =!= prepend(p0,toList(n-p0:1)) then error "expected a sequence of coincident root loci determined by hook shapes";
            n + 2 - p0
    );
    m1 := n - sum l;
    if m1 < 0 then error "too many coincident root loci";
    dual coincidentRootLocus(l|toList(m1:1),coefficientRing R,Variable=>(first U)#"variable")
);

CoincidentRootLocus * CoincidentRootLocus := (X,Y) -> joinOfHooks(X,Y);

JoinOfCoincidentRootLoci * CoincidentRootLocus := (X,Y) -> joinOfHooks append(toSequence(X#"listCRloci"),Y);

CoincidentRootLocus * JoinOfCoincidentRootLoci := (X,Y) -> Y + X;

JoinOfCoincidentRootLoci * JoinOfCoincidentRootLoci := (X,Y) -> joinOfHooks (toSequence(X#"listCRloci") | toSequence(Y#"listCRloci"));

--================================================================
--=== Binary forms and utilities =================================
--================================================================

RingElement ~ := (F) -> RingElement := (G) -> (
  if not (isPolynomialRing ring F and isPolynomialRing ring G and numgens ring F == 2 and numgens ring G == 2) then error "expected two binary forms";
  K := coefficientRing ring F;
  if K =!= coefficientRing ring G then error "got different coefficient rings";
  (x,y) := (local x,local y);
  R := K[x,y];
  F = sub(F,vars R);
  G = sub(G,vars R);
  if not isField K then (
     K = frac K;
     R = K[gens R];
     F = sub(F,R);
     G = sub(G,R);
  );
  if not (isHomogeneous F and isHomogeneous G) then error "expected two binary forms";
  ideal F == ideal G
);

switch1 = method()

switch1 (RingElement) := (p) -> (
   checkBinaryForm p;
   G := ring p;
   mm := gens (ideal gens G)^(first degree p);
   c := flatten entries sub(last coefficients(p,Monomials=>mm),coefficientRing G);
   apply(#c,j -> c_j * (binomial(first degree p,j))^-1)
);

switch1 (List) := (l) -> switch1(l,Grass(0,1,ring matrix {l},Variable=>VAR));

switch1 (List,PolynomialRing) := (l,G) -> (
    if 2 != numgens G then error "expected polynomial ring with two variables";
    K := if isField ring matrix {l} then ring matrix {l} else frac(ring matrix {l});
    mm := flatten entries gens (ideal gens G)^(#l-1);
    F := sum(#l,j -> l_j * mm_j * binomial(#l-1,j));
    (Trim ideal F)_0
);

switch2 = method(Options => {Variable => null})

switch2 (Ideal) := o -> (p) -> (
   if not (isPolynomialRing ring p and isHomogeneous p) then error "expected a homogeneous ideal in a polynomial ring";
   if # unique degrees p > 1 then error "expected an ideal generated by forms of the same degree";
   d := first unique degrees p;
   x := if o.Variable === null then ring p else o.Variable;
   K := coefficientRing ring p;
   if d === {1} then (
       I := Trim ideal apply(entries transpose coefficients parametrize p,u -> switch1(u,Grass(0,1,K,Variable=>x)));
       return if numgens I == 1 then I_0 else I;
   ) else return linearSpanOfRows(matrix apply(p_*,switch1),Grass(0,first d,K,Variable=>x));
);

switch2 (RingElement) := o -> (F) -> switch2(ideal F,Variable=>o.Variable);

switch (RingElement) := (F) -> switch1 F;
switch (Ideal) := (I) -> switch2 I;
switch (List) := (L) -> switch2 switch1 L;

pairingIsomorphism = method()

pairingIsomorphism (Ring) := (R) -> inverse rationalMap for i to numgens R -1 list binomial(numgens R -1,i) * (gens R)_i;

pairingIsomorphism (Ideal) := (I) -> (pairingIsomorphism ring I) I;

pairingIsomorphism (ZZ,Ring) := (n,K) -> pairingIsomorphism Grass(0,n,K,Variable=>VAR);

randomBinaryForm = method(Options => {Variable => VAR})
randomBinaryForm (ZZ,Thing,Thing,Ring) := o -> (n,RealRank,ComplexRank,K) -> (
   try assert (RealRank === null or (ring RealRank === ZZ and RealRank >= 1 and RealRank <= n)) else error("illegal value for argument 2, expected null or integer between 1 and "|toString(n)|", the real rank"); 
   try assert (ComplexRank === null or (ring ComplexRank === ZZ and ComplexRank >= 1 and ComplexRank <= n)) else error("illegal value for argument 3, expected null or integer between 1 and "|toString(n)|", the complex rank"); 
   if n < 0 then error "illegal value for argument 1, expected a nonnegative integer";
   if not isField K then error "expected a field";
   x := o.Variable;
   R := Grass(0,1,K,Variable=>x);
   if RealRank === null and ComplexRank === null then return (Trim ideal random(n,R))_0;
   if RealRank === null and ComplexRank =!= null then (
       if ComplexRank === 1 then return (randomBinaryForm(1,,,K,Variable=>x))^n;
       return recover(randomBinaryForm(ComplexRank,,,K,Variable=>x),(randomBinaryForm(1,,,K,Variable=>x))^2 * randomBinaryForm(n-ComplexRank,,,K,Variable=>x));
   );
   if RealRank =!= null and ComplexRank === null then (
       if RealRank === 1 then return (randomBinaryForm(1,,,K,Variable=>x))^n;
       return recover(randomRealRootedBinaryForm(RealRank,K,Variable=>x),randomNotRealRootedBinaryForm(n+2-RealRank,K,Variable=>x));
   );
   if RealRank =!= null and ComplexRank =!= null then (
       if RealRank === 1 and ComplexRank === 1 then return (randomBinaryForm(1,,,K,Variable=>x))^n;
       if RealRank === ComplexRank then return recover(randomRealRootedBinaryForm(RealRank,K,Variable=>x),(randomBinaryForm(1,,,K,Variable=>x))^2 * randomBinaryForm(n-ComplexRank,,,K,Variable=>x));
       if RealRank + ComplexRank == n+2 and ComplexRank <= RealRank then return recover(randomNotRealRootedBinaryForm(ComplexRank,K,Variable=>x),randomRealRootedBinaryForm(RealRank,K,Variable=>x));
       error("unable to build a binary form of degree "|toString(n)|", real rank "|toString(RealRank)|" and complex rank "|toString(ComplexRank));
   );
);

randomBinaryForm (ZZ,Thing,Thing) := o -> (n,RealRank,ComplexRank) -> randomBinaryForm(n,RealRank,ComplexRank,KK,Variable=>o.Variable);

randomBinaryForm (ZZ,Ring) := o -> (n,K) -> randomBinaryForm(n,,,K,Variable=>o.Variable);

randomBinaryForm (ZZ) := o -> (n) -> randomBinaryForm(n,KK,Variable=>o.Variable);

randomRealRootedBinaryForm = method(Options => {Variable => VAR})
randomRealRootedBinaryForm (ZZ,Ring) := o -> (n,K) -> (Trim ideal product(n,i -> randomBinaryForm(1,,,K,Variable=>o.Variable)))_0;

randomNotRealRootedBinaryForm = method(Options => {Variable => VAR})
randomNotRealRootedBinaryForm (ZZ,Ring) := o -> (n,K) -> (
   x := o.Variable;
   q := randomBinaryForm(2,,,K,Variable=>x);
   while -discriminant q >= 0 do q = randomBinaryForm(2,,,K,Variable=>x);
   (Trim ideal(q * randomBinaryForm(n-2,,,K,Variable=>x)))_0
);

linearSpanOfRows = method();
linearSpanOfRows (Matrix,Ring) := (M,R) -> (
    m := numgens target M -1;
    u := local u; x := local x; K := coefficientRing R; n := numgens R -1;
    assert (numgens source M -1 == n and ring M === K);
    S := K[u_0..u_m,x_0..x_n,MonomialOrder=>Eliminate(m+1)];
    I := ideal(matrix{toList(u_0..u_m)} * M - matrix{toList(x_0..x_n)});
    Trim sub(sub(ideal selectInSubring(1,gens gb I),K[x_0..x_n]),vars R)
);

checkBinaryForm = method()
checkBinaryForm (RingElement) := (F) -> (
   E := "expected a binary form";
   if not (isPolynomialRing ring F and numgens ring F == 2) then error E;
   K := coefficientRing ring F;
   if isField K then (
       if not isHomogeneous F then error E;
   ) else (
       (x,y) := (local x,local y);
       R := K[x,y];
       F = sub(F,vars R);
       try assert(isHomogeneous sub(F,(frac K)[x,y])) else error E;
   );
);

load "./CoincidentRootLoci/documentation.m2"

load "./CoincidentRootLoci/tests.m2"

welcome := "CoincidentRootLoci v."|CoincidentRootLoci.Options.Version|" loaded successfully (last updated: "|CoincidentRootLoci.Options.Date|")";
if notify then
  if (options CoincidentRootLoci)#OptionalComponentsPresent then <<concatenate(#welcome:"*")<<endl<<welcome<<endl<<concatenate(#welcome:"*")<<endl else <<"--warning: Qepcad required but not present"<<endl;

