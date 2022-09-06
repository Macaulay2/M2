
-*
   Copyright 2020, Giovanni Staglianò.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-

newPackage(
       "Cremona",
	Version => "5.2", 
        Date => "September 6, 2022",
    	Authors => {{Name => "Giovanni Staglianò", Email => "giovannistagliano@gmail.com" }},
    	Headline => "rational maps between projective varieties",
	Keywords => {"Algebraic Geometry"},
        AuxiliaryFiles => true,
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry",
	     "journal URI" => "http://j-sag.org/",
	     "article title" => "A Macaulay2 package for computations with rational maps",
	     "acceptance date" => "11 June 2018",
	     "published article URI" => "https://msp.org/jsag/2018/8-1/p06.xhtml",
	     "published article DOI" => "10.2140/jsag.2018.8.61",
	     "published code URI" => "https://msp.org/jsag/2018/8-1/jsag-v8-n1-x06-Cremona.zip",
	     "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/Cremona.m2",
	     "release at publication" => "2e87a29e4b5b68af1bd8917a9c76d4008ff9fc5b",	    -- git commit number in hex
	     "version at publication" => "4.2.2",
	     "volume number" => "8",
	     "volume URI" => "https://msp.org/jsag/2018/8-1/"
	     }
	)

export{
   "ChernSchwartzMacPherson",
   "SegreClass",
   "EulerCharacteristic",
   "graph",
   "degreeMap",   
   "inverseMap",
   "isBirational",
   "isDominant",
   "isInverseMap",
   "projectiveDegrees",
   "toMap",
   "MathMode", 
   "Dominant", 
   "NumDegrees",
   "approximateInverseMap",
   "CodimBsInv",
   "parametrize",
   "specialCremonaTransformation",
   "quadroQuadricCremonaTransformation",
   "specialQuadraticTransformation",
   "RationalMap",
   "rationalMap",
   "BlowUpStrategy",
   "forceInverseMap",
   "forceImage",
   "point",
   "segre",
   "abstractRationalMap",
   "specialCubicTransformation",
   "isMorphism",
   "exceptionalLocus"
};

certificate := "MathMode: output certified!"|newline;

MultihomogeneousRationalMap = new Type of MutableHashTable;
WeightedHomogeneousRationalMap = new Type of MultihomogeneousRationalMap; -- this isn't really a subtype
RationalMap = new Type of MutableHashTable;

RationalMap.synonym = "rational map";

ChernSchwartzMacPherson = method(TypicalValue => RingElement, Options => {MathMode => false, BlowUpStrategy => "Eliminate", Verbose => true});
SegreClass = method(TypicalValue => RingElement, Options => {MathMode => false, BlowUpStrategy => "Eliminate", Verbose => true});
EulerCharacteristic = method(TypicalValue => ZZ, Options => {MathMode => false, BlowUpStrategy => "Eliminate", Verbose => true});
graph = method(Options => {BlowUpStrategy => "Eliminate"});
degreeMap = method(TypicalValue => ZZ, Options => {MathMode => false, BlowUpStrategy => "Eliminate", Verbose => true});
inverseMap = method(Options => {MathMode => false, BlowUpStrategy => "Eliminate", Verbose => true});
isBirational = method(TypicalValue => Boolean, Options => {MathMode => false, BlowUpStrategy => "Eliminate", Verbose => true});
isDominant = method(TypicalValue => Boolean, Options => {MathMode => false, Verbose => true});
isInverseMap = method(TypicalValue => Boolean);
projectiveDegrees = method(TypicalValue => List, Options => {MathMode => false, NumDegrees => infinity, BlowUpStrategy => "Eliminate", Verbose => true});
toMap = method(TypicalValue => RingMap, Options => {Dominant => null});
approximateInverseMap = method(Options => {MathMode => false, CodimBsInv => null, Verbose => true});
parametrize = method();
specialCremonaTransformation = method(TypicalValue=>RationalMap);
quadroQuadricCremonaTransformation = method(TypicalValue=>RationalMap);
specialQuadraticTransformation = method(TypicalValue=>RationalMap);
specialCubicTransformation = method(TypicalValue=>RationalMap);
rationalMap = method(Options => {Dominant => null});
forceInverseMap = method(TypicalValue => Nothing);
forceImage = method(TypicalValue => Nothing);
point = method();
segre = method(TypicalValue => RationalMap);
abstractRationalMap = method();
isMorphism = method(TypicalValue => Boolean);
exceptionalLocus = method(TypicalValue => Ideal, Options => {MathMode => false});

rationalMap RingMap := o -> ((cacheValue (o.Dominant,"RationalMapAssociatedToRingMap")) (phi -> (
   checkMultihomogeneousRationalMap phi;
   isStandardMap := degrees ambient target phi == toList((numgens ambient target phi):{1});
   isWeightedMap := degreeLength ambient target phi == 1 and max flatten degrees ambient target phi >= 2;
   if (not isStandardMap) and (not isWeightedMap) and (o.Dominant =!= "notSimplify") then phi = simplifyMap phi;
   if (o.Dominant =!= null and o.Dominant =!= "notSimplify") then phi = toMap(phi,Dominant=>o.Dominant);
   if isStandardMap then return new RationalMap from {
           "map" => phi,
           "maps" => null,
           "isDominant" => if o.Dominant === true or o.Dominant === infinity then true else null,
           "idealImage" => if o.Dominant === true or o.Dominant === infinity then trim ideal(0_(source phi)) else null,
           "isBirational" => null,
           "inverseRationalMap" => null,
           "projectiveDegrees" => {},
           "degree" => null,
           "dimAmbientTarget" => numgens ambient target phi -1,
           "dimTarget" => max(dim target phi -1,-1),
           "dimAmbientSource" => numgens ambient source phi -1,
           "dimSource" => max(dim source phi -1,-1),
           "blowUpIdeal" => null};
   Phi := new MultihomogeneousRationalMap from {
           "map" => phi,
           "maps" => null,
           "isDominant" => if o.Dominant === true or o.Dominant === infinity then true else null,
           "idealImage" => if o.Dominant === true or o.Dominant === infinity then trim ideal(0_(source phi)) else null,
           "isBirational" => null,
           "projectiveDegrees" => {},
           "degree" => null,
           "dimAmbientTarget" => apply(multigens ambient target phi,n -> (#n)-1),
           "dimTarget" => max(dim target phi - (# heft ambient target phi),-1),
           "dimAmbientSource" => numgens ambient source phi -1,
           "dimSource" => max(dim source phi -1,-1),
           "blowUpIdeal" => null,
           "baseLocus" => null};
   if isWeightedMap then (Phi = new WeightedHomogeneousRationalMap from Phi; Phi#"dimAmbientTarget" = toSequence flatten degrees ambient target phi);
   return Phi;
)));
rationalMap (Matrix) := o -> (F) -> rationalMap(toMap(F,Dominant=>null),Dominant=>o.Dominant);
rationalMap (List) := o -> (F) -> rationalMap(toMap(F,Dominant=>null),Dominant=>o.Dominant);
rationalMap (Ideal) := o -> (I) -> rationalMap(toMap(I,Dominant=>null),Dominant=>o.Dominant);
rationalMap (Ideal,ZZ) := o -> (I,d) -> rationalMap(toMap(I,d,Dominant=>null),Dominant=>o.Dominant);
rationalMap (Ideal,List) := o -> (I,d) -> rationalMap(toMap(I,d,Dominant=>null),Dominant=>o.Dominant);
rationalMap (Ideal,ZZ,ZZ) := o -> (I,d,e) -> rationalMap(toMap(I,d,e,Dominant=>null),Dominant=>o.Dominant);
rationalMap (PolynomialRing,List) := o -> (R,L) -> ( -- undocumented
   try assert(ring matrix{L} === ZZ) else error "expected a list of integers";
   J := if unique L_{1..#L-1} === {0} or L_{1..#L-1} === {}
        then ideal vars R 
        else saturate intersect flatten for i from 1 to #L-1 list for j from 1 to L_i list (point R)^i;
   rationalMap(J,L_0,Dominant=>o.Dominant)
);
rationalMapInt = method(Options => {Dominant => null});
rationalMapInt (MutableHashTable) := o -> (Phi) -> ( 
     Psi := Phi * rationalMap(target Phi,ambient target Phi);
     if Phi#"projectiveDegrees" =!= {} then setKeyValue(Psi,"projectiveDegrees",Phi#"projectiveDegrees"); 
     if Phi#"degree" =!= null then setKeyValue(Psi,"degree",Phi#"degree"); 
     if Phi#"idealImage" =!= null then setKeyValue(Psi,"idealImage",trim(lift(image Phi,ambient source Phi#"map") + ideal(source Phi#"map")));   
     if Phi#"maps" =!= null then setKeyValue(Psi,"maps",apply(Phi#"maps",f -> map(target Psi#"map",source Psi#"map",toMatrix f)));
     if o.Dominant === true or o.Dominant === infinity then (
            if Psi#"idealImage" === null then setKeyValue(Psi,"idealImage",trim(lift(image Phi,ambient source Phi#"map") + ideal(source Phi#"map")));
            setKeyValue(Psi,"map",map(target Psi#"map",(source Psi#"map")/(Psi#"idealImage"),toMatrix Psi#"map"));
            if Phi#"maps" =!= null then setKeyValue(Psi,"maps",apply(Phi#"maps",f -> map(target Psi#"map",source Psi#"map",toMatrix f)));
            setKeyValue(Psi,"isDominant",true); 
     );
     if class o.Dominant === Ideal then if ring o.Dominant === source Psi#"map" then if ((Psi#"map") o.Dominant == 0 and isHomogeneous o.Dominant) then (
            setKeyValue(Psi,"map",map(target Psi#"map",(source Psi#"map")/(o.Dominant),toMatrix Psi#"map"));
            if Phi#"maps" =!= null then setKeyValue(Psi,"maps",apply(Phi#"maps",f -> map(target Psi#"map",source Psi#"map",toMatrix f)));
     ); 
     if class o.Dominant === ZZ then (
            J := image(Psi,o.Dominant);
            setKeyValue(Psi,"map",map(target Psi#"map",(source Psi#"map")/J,toMatrix Psi#"map"));
            if Phi#"maps" =!= null then setKeyValue(Psi,"maps",apply(Phi#"maps",f -> map(target Psi#"map",source Psi#"map",toMatrix f)));
     ); 
     return Psi;
);
rationalMap (RationalMap) := o -> (Phi) -> rationalMapInt(Phi,Dominant=>o.Dominant);
rationalMap (MultihomogeneousRationalMap) := o -> (Phi) -> rationalMapInt(Phi,Dominant=>o.Dominant);

super RationalMap := Phi -> if target Phi === ambient target Phi then Phi else rationalMap Phi;
super MultihomogeneousRationalMap := Phi -> if target Phi === ambient target Phi then Phi else rationalMap Phi;

rationalMap (Ring,Ring,Matrix) := o -> (R,S,F) -> (
     if not (isPolynomialRing ambient S) then error("the ambient rings must be polynomial rings");
     phi := map(R,ambient S,F);
     if not isHomogeneous ideal S then error "got quotient of polynomial ring by a non-homogeneous ideal";
     if phi(ideal S) != 0 then error "the map is not valid";
     rationalMap(map(R,S,F),Dominant=>o.Dominant)
);

rationalMap (Ring,Ring,List) := o -> (R,S,L) -> (
     rationalMap(R,S,matrix{L},Dominant=>o.Dominant)
);

rationalMap (Ring,Ring) := o -> (R,S) -> (
    if (numgens ambient R != numgens ambient S) then error "expected ambient rings to have the same dimension";
    Phi := rationalMap(R,S,vars ambient R,Dominant=>o.Dominant);
    if (isPolynomialRing R and isPolynomialRing S and class Phi === RationalMap) then (
         setKeyValue(Phi,"maps",{map Phi});
         setKeyValue(Phi,"isDominant",true);
         setKeyValue(Phi,"isBirational",true);
         setKeyValue(Phi,"projectiveDegrees",toList((numgens R):1));
         setKeyValue(Phi,"degree",1);
    );
    Phi
);

rationalMap (Ring) := o -> (R) -> rationalMap(R,R,Dominant=>o.Dominant);

rationalMap (Ring,Tally) := o -> (R,E) -> ( 
    if not isField coefficientRing R then error "the coefficient ring needs to be a field";
    if not ((isPolynomialRing R or isQuotientRing R) and isPolynomialRing ambient R and isHomogeneous ideal R) then error "the base ring must be a quotient of a polynomial ring by a homogeneous ideal";
    -- if # heft R =!= 1 then error "expected standard grading";
    for X in keys E do if not(instance(X,Ideal) and (ring X === R or ring X === ambient R)) then error "expected a tally whose elements represent pure codimension 1 subschemes of a projective variety";
    D := pairs E;
    if not isPolynomialRing R then D = apply(D,d -> if ring first d === ambient R then (trim sub(first d,R),last d) else d);
    I := trim intersect for d in D list (first d)^(last d); 
    J := quotient(ideal I_0,I);
    d := first degrees I;
    if # heft R == 1 then d = first d;
    rationalMap(J,d,Dominant=>o.Dominant)
);

rationalMap (Tally) := o -> (E) -> ( 
    R := unique for X in keys E list (if not instance(X,Ideal) then error "expected a tally of ideals"; ring X);
    if #R == 1 or (#R == 2 and last R === ambient first R) then return rationalMap(first R,E,Dominant=>o.Dominant);
    if #R == 2 and first R === ambient last R then return rationalMap(last R,E,Dominant=>o.Dominant);
    error "expected a tally whose elements represent pure codimension 1 subschemes of a projective variety";
);

simplifyMap = method()
simplifyMap (RingMap) := (phi) -> ( 
   I := multisaturate ideal target phi;
   if I =!= ideal target phi and I != ideal target phi then (
        <<"--warning: target of ring map changed, its ideal was not multi-saturated"<<endl;
        R := (ambient target phi)/I;
        phi = map(R,source phi,sub(toMatrix phi,R));
   );
   return compose(map(target phi,target phi,vars ambient target phi),phi);
);

rationalMapWithoutChecking = method()
rationalMapWithoutChecking (RingMap) := (phi) -> (
   new RationalMap from {
           "map" => phi,
           "maps" => null,
           "isDominant" => null,
           "idealImage" => null,
           "isBirational" => null,
           "inverseRationalMap" => null,
           "projectiveDegrees" => {},
           "degree" => null,
           "dimAmbientTarget" => numgens ambient target phi -1,
           "dimTarget" => max(dim target phi -1,-1),
           "dimAmbientSource" => numgens ambient source phi -1,
           "dimSource" => max(dim source phi -1,-1),
           "blowUpIdeal" => null
      }
);

RationalMap ~ := (Phi) -> (
   new MultihomogeneousRationalMap from {
        "map" => Phi#"map",
        "maps" => Phi#"maps",
        "isDominant" => Phi#"isDominant",
        "idealImage" => Phi#"idealImage",
        "isBirational" => Phi#"isBirational",
        "projectiveDegrees" => Phi#"projectiveDegrees",
        "degree" => Phi#"degree",
        "dimAmbientTarget" => {Phi#"dimAmbientTarget"},
        "dimTarget" => Phi#"dimTarget",
        "dimAmbientSource" => Phi#"dimAmbientSource",
        "dimSource" => Phi#"dimSource",
        "blowUpIdeal" => Phi#"blowUpIdeal",
        "baseLocus" => null
   }
);

targetProj = memoize ((R,N) -> (
   if (numgens R -1 == N and # heft R == 1 and unique degrees R == {{1}}) then return R;
   K:=coefficientRing R;
   t:=local t; x:=local x; y:=local y; txy:=baseName first gens R; if class txy === IndexedVariable then txy = first txy; txy=toString txy;
   PNl:=(K[t_0..t_N],K[x_0..x_N],K[y_0..y_N]);   
   PN:=PNl_0; if txy === "t" then PN=PNl_1; if txy === "x" then PN=PNl_2;
   PN
));

toMap Matrix := o -> ((cacheValue (o.Dominant,"RingMapAssociatedToMatrix")) (F -> ( 
   if class ring F === FractionField then try F = lift((lcm apply(flatten entries F,denominator))*F,ring numerator (1_(ring F)));
   checkLinearSystem0 F;
   K:=coefficientRing ring F; 
   N:=numgens source F-1; 
   if N == -1 then return map(ring F,K[]);  
   PN:=targetProj(ambient ring F,N);    
   phi:=map(ring F,PN,F);
   if class o.Dominant === ZZ 
   then map(ring F,PN/(kernel(phi,o.Dominant)),F)
   else if o.Dominant === infinity or o.Dominant === true
   then map(ring F,PN/(trim kernel phi),F)
   else phi
)));

toMap List := o -> (F) -> toMap(matrix{F},Dominant=>o.Dominant);

toMap Ideal := o -> (F) -> (
   d := max degrees F;
   if unique degrees F == {d} then toMap(gens F,Dominant=>o.Dominant) else toMap(F,d,Dominant=>o.Dominant)
);

toMap RingMap := o -> (phi) -> (
   phi=phi * map(source phi,ambient source phi);
   if class o.Dominant === ZZ 
   then return map(target phi,(source phi)/(kernel(phi,o.Dominant)),toMatrix phi)
   else if o.Dominant === infinity or o.Dominant === true 
   then return map(target phi,(source phi)/(trim kernel phi),toMatrix phi);
   if class o.Dominant === Ideal then if ring o.Dominant === source phi then if (phi o.Dominant == 0 and isHomogeneous o.Dominant) then (
       return map(target phi,(source phi)/(o.Dominant),toMatrix phi);
   ); 
   return phi;
);

toMap (Ideal,List) := o -> (I,v) -> (
   if not isHomogeneous I then error("the ideal must be homogeneous");
   linSys:=gens image basis(v,I);
   toMap(linSys,Dominant=>o.Dominant)
);

toMap (Ideal,ZZ) := o -> (I,v) -> (
   J := ideal select(I_*,g -> degree g <= {v});
   if numgens J == 0 then J = sub(J,ring I);
   toMap(J,{v},Dominant=>o.Dominant)
)

toMap (Ideal,ZZ,ZZ) := o -> (I,v,jj) -> (
   -- Assume I a homogeneous saturated ideal
   -- Return the linear system of hypersurfaces of degree v with points of multiplicity jj along V(I) subset PP^d
   if not isPolynomialRing ring I then error "expected ideal in a polynomial ring";
   if # heft ring I =!= 1 then error "expected standard grading";
   if not isHomogeneous I then error "the ideal must be homogeneous";
   if not isField coefficientRing ring I then error "the coefficient ring needs to be a field";
   if jj <= 0 then error "expected a positive integer";
   homComp := method();
   -- homComp (ZZ,Ideal) := (d,I) -> ideal image basis(d,I);
   homComp (ZZ,Ideal) := (d,I) -> (
      vv := ideal vars ring I;
      D := flatten degrees I;
      if d < min D then return ideal ring I;
      trim sum for i from min D to min(d,max D) list if # select(D,g -> g == i) > 0 then (vv^(d - i) * ideal select(I_*,g -> degree g == {i})) else continue
   );
   Jac := method();
   Jac (Matrix,ZZ) := (F,d) -> (
      mm := flatten entries gens (ideal vars ring F)^d;
      transpose matrix{apply(mm,t -> diff(t,transpose F))}
   );
   if jj == 1 then return toMap(gens homComp(v,I),Dominant=>o.Dominant);
   K := coefficientRing ring I; 
   d := numgens ring I -1;
   x := local x;
   PP := K[x_0..x_d];
   C := homComp(v-jj+1,sub(I,vars PP));
   n := numgens C -1;
   if n == -1 then return toMap(sub(matrix{{}},ring I),Dominant=>o.Dominant);
   bs := gens homComp(v,sub(I,vars PP));
   N := numgens source bs -1;
   if N == -1 then return toMap(sub(matrix{{}},ring I),Dominant=>o.Dominant);
   a := local a; b := local b;
   de := binomial(d+jj-1,jj-1) -1;
   R := K[b_(0,0)..b_(n,de), a_0..a_N, MonomialOrder=>Eliminate ((de+1)*(n+1))];
   R' := R[x_0..x_d];
   M := sub(Jac(bs,jj-1),R')*sub(transpose matrix{{a_0..a_N}},R') - transpose((gens sub(C,R'))*sub(matrix for i to n list for j to de list b_(i,j),R'));
   sys := sub(trim ideal last coefficients M,R);
   sols := sub(ideal selectInSubring(1,gens gb sys),K[a_0..a_N]);
   f := map parametrize sols;
   if dim target f <= 0 then return toMap(sub(matrix{{}},ring I),Dominant=>o.Dominant);
   PP' := PP[gens target f];
   linSys := transpose sub(sub((coefficients (sub(matrix f,PP') * transpose sub(bs,PP'))_(0,0))_1,PP),vars ring I);
   toMap(linSys,Dominant=>o.Dominant)
);

toMap (RationalMap) := o -> (Phi) -> (
    maps Phi; 
    if o.Dominant === null then return Phi#"map" else return toMap(Phi#"map",Dominant=>o.Dominant);
);

toMap (MultihomogeneousRationalMap) := o -> (Phi) -> (
    maps Phi; 
    if o.Dominant === null then return Phi#"map" else return toMap(Phi#"map",Dominant=>o.Dominant);
);

map (RationalMap) := o -> (Phi) -> Phi#"map";

map (ZZ,RationalMap) := o -> (i,Phi) -> (
    if i < 0 then error "expected a nonnegative integer";
    if i == 0 then return map Phi;
    n := # maps Phi;
    if i > n-1 then error("the number of minimal representatives of the map is "|toString(n));
    (Phi#"maps")_i
);

map (MultihomogeneousRationalMap) := o -> (Phi) -> Phi#"map";

map (ZZ,MultihomogeneousRationalMap) := o -> (i,Phi) -> (
    if i < 0 then error "expected a nonnegative integer";
    if i == 0 then return map Phi;
    n := # maps Phi;
    if i > n-1 then error("the number of minimal representatives of the map is "|toString(n));
    (Phi#"maps")_i
);

expression RationalMap := (Phi) -> (
    if Phi#"dimTarget" < 0 or Phi#"dimSource" < 0 then return expression("map from " | expressionVar(Phi#"dimTarget" , Phi#"dimAmbientTarget") | " to " | expressionVar(Phi#"dimSource" , Phi#"dimAmbientSource"));
    d:= max flatten degrees ideal compress matrix Phi; type:="";
      if d === 1 then type = "linear "; if d === 2 then type = "quadratic "; if d === 3 then type = "cubic ";
    if (isPolynomialRing target Phi#"map" and isPolynomialRing source Phi#"map" and Phi#"isBirational" === true) then (
         if #(Phi#"projectiveDegrees")>=3 then (
              if unique(Phi#"projectiveDegrees") == {1} then return expression("projective transformation of PP^"|toString(Phi#"dimTarget")) else return expression("Cremona transformation of PP^"|toString(Phi#"dimTarget")|" of type "|toString(((Phi#"projectiveDegrees")_1,(Phi#"projectiveDegrees")_(#(Phi#"projectiveDegrees")-2))));
         );
         return expression(type|"Cremona transformation of PP^"|toString(Phi#"dimTarget"));
    );
    return expression(type|(if Phi#"isBirational" === true then "birational " else (if Phi#"isDominant" === true then "dominant rational " else "rational "))| "map from " | expressionVar(Phi#"dimTarget" , Phi#"dimAmbientTarget") | " to " | expressionVar(Phi#"dimSource" , Phi#"dimAmbientSource"));
);

expression MultihomogeneousRationalMap := (Phi) -> (
    if Phi#"dimTarget" < 0 or Phi#"dimSource" < 0 then return expression("map from " | expressionVar(Phi#"dimTarget" , Phi#"dimAmbientTarget") | " to " | expressionVar(Phi#"dimSource" , Phi#"dimAmbientSource"));
    return expression((if Phi#"isBirational" === true then "birational " else (if Phi#"isDominant" === true then "dominant rational " else "rational "))| "map from " | expressionVar(Phi#"dimTarget" , Phi#"dimAmbientTarget") | " to " | expressionVar(Phi#"dimSource" , Phi#"dimAmbientSource"));
);

net RationalMap := (Phi) -> nicePrint Phi;

net MultihomogeneousRationalMap := (Phi) -> nicePrint Phi;

texMath MultihomogeneousRationalMap := texMath RationalMap := texMath @@ net;

RationalMap#{WebApp,AfterPrint} = RationalMap#{WebApp,AfterNoPrint} = 
RationalMap#{Standard,AfterPrint} = RationalMap#{Standard,AfterNoPrint} = (Phi) -> (
  << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << class Phi << " (" << expression Phi << ")" << endl;
);

MultihomogeneousRationalMap#{WebApp,AfterPrint} = MultihomogeneousRationalMap#{WebApp,AfterNoPrint} = 
MultihomogeneousRationalMap#{Standard,AfterPrint} = MultihomogeneousRationalMap#{Standard,AfterNoPrint} = (Phi) -> (
  << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << class Phi << " (" << expression Phi << ")" << endl;
);

nicePrint = method(TypicalValue => Net)

nicePrint (List) := (F) -> (
   E := net("{");
   if #F > 0 then E = E || stack append(for i to #F-2 list (" "|net(F_i)|",")||" "," "|net(last F));
   E||net("}")
);

nicePrint (Ideal) := (I) -> nicePrint flatten entries gens I;

nicePrint (PolynomialRing) := (R) -> (
   K := coefficientRing R;
   if degreeLength R == 1 and min flatten degrees R >= 1 and max flatten degrees R >= 2 then return "Proj("|net(K)|net(new Array from gens R)|",Degrees=>"|net(degrees R)|")";
   mm := apply(multigens R,m -> new Array from m);
   P := "Proj("|net(K)|net(mm_0)|")";
   for i from 1 to #mm-1 do P = P|" x Proj("|net(K)|net(mm_i)|")";
   return P;
);

nicePrint (QuotientRing) := (R) -> ("subvariety of "|nicePrint(ambient R)|" defined by")||nicePrint(ideal R);

nicePrint (MutableHashTable) := (Phi) -> "-- rational map --"||("source: "|nicePrint(source Phi))||("target: "|nicePrint(target Phi))||"defining forms: "|nicePrint(entries Phi); 

describeInt = method()

describeInt (MutableHashTable) := (Phi) -> (
    d := max degrees ideal compress matrix Phi; 
    isStandardMap := false; if class Phi#"dimAmbientTarget" === ZZ then (d = first d; isStandardMap = true);
    descr:="rational map defined by "|(if not isStandardMap then "multiforms" else "forms")|" of degree "|toString(d)|newline;
    descr=descr|"source variety: "|expressionVar(ideal target Phi#"map",Phi#"dimTarget",Phi#"dimAmbientTarget")|newline;
    descr=descr|"target variety: "|expressionVar(ideal source Phi#"map",Phi#"dimSource",Phi#"dimAmbientSource")|newline;
    if Phi#"isDominant" =!= true and Phi#"idealImage" =!= null then descr=descr|"image: "|expressionVar(if Phi#"dimAmbientSource" == Phi#"dimSource" then Phi#"idealImage" else (lift(Phi#"idealImage",ambient source Phi#"map") + ideal source Phi#"map"))|newline;
    if Phi#"isDominant" =!= null then descr=descr|"dominance: "|toString(Phi#"isDominant")|newline;
    if Phi#"isBirational" =!= null then (
             descr=descr|"birationality: "|toString(Phi#"isBirational");
             if isStandardMap then if Phi#"inverseRationalMap" =!= null then descr=descr|" (the inverse map is already calculated)";
             descr=descr|newline;
    );
    if Phi#"isBirational" =!= true and Phi#"degree" =!= null then descr=descr|"degree of map: "|toString(Phi#"degree")|newline;
    if Phi#"projectiveDegrees" =!= {} then descr=descr|"projective degrees: "|toString(Phi#"projectiveDegrees")|newline;
    if Phi#"maps" =!= null then (
                 descr=descr|"number of minimal representatives: "|toString(# Phi#"maps");
                 if # Phi#"maps" >1 then descr=descr|", with degrees "|toString(toSequence apply(Phi#"maps",F-> max degrees ideal compress toMatrix F));
                 descr=descr|newline;
                 B:=ideal Phi; dimB:=max(dim B - (# heft ambient source Phi),-1);
                 descr=descr|"dimension base locus: "|toString(dimB)|newline;
                 if isStandardMap then if dimB>=0 then descr=descr|"degree base locus: "|toString(degree B)|newline;     
    );
    descr=descr|"coefficient ring: "|toString(coefficientRing ambient target Phi#"map");
    net expression descr
);

describe (RationalMap) := (Phi) -> describeInt Phi;

describe (MultihomogeneousRationalMap) := (Phi) -> describeInt Phi;

toString RationalMap := (Phi) -> "rationalMap("|toExternalString(map Phi)|")";

toString MultihomogeneousRationalMap := (Phi) -> "rationalMap("|toExternalString(map Phi)|")";

imageInt = method()
imageInt (MutableHashTable) := (Phi) -> (
   if Phi#"idealImage" =!= null then return Phi#"idealImage";
   f := Phi#"map";
   if all(flatten apply(entries Phi,degree),o -> o <= 0) then f = map(target f,source f,(first gens target f) * toMatrix f);
   setKeyValue(Phi,"idealImage",trim kernel f);
   return Phi#"idealImage";
);
imageInt (MutableHashTable,ZZ) := (Phi,d) -> (
   if Phi#"idealImage" === null then return kernel(Phi#"map",d);
   J := select(flatten entries gens Phi#"idealImage",g -> degree g <= {d});
   if #J == 0 then return trim ideal 0_(target Phi);
   J = ideal J;
   if unique degrees J == {{d}} then return J else return ideal image basis(d,J);
);

image (RationalMap) := (Phi) -> imageInt(Phi);

image (RationalMap,ZZ) := (Phi,d) -> imageInt(Phi,d);

image (ZZ,RationalMap) := (d,Phi) -> imageInt(Phi,d); -- undocumented

image (MultihomogeneousRationalMap) := (Phi) -> imageInt(Phi);

image (MultihomogeneousRationalMap,ZZ) := (Phi,d) -> imageInt(Phi,d);

image (ZZ,MultihomogeneousRationalMap) := (d,Phi) -> imageInt(Phi,d); -- undocumented

kernel(RingMap,ZZ) := o -> (phi,d) -> kernelComponent(phi,d); 

kernel(ZZ,RingMap) := o -> (d,phi) -> kernel(phi,d); -- undocumented

kernelComponent=method(TypicalValue => Ideal);
kernelComponent(RingMap,ZZ) := (phi,d) -> (
   checkRationalMap0 phi;
   if d<0 then return ideal source phi;
   Pn:=ambient target phi; Pm:=ambient source phi;  
   Phi:=lift(toMatrix phi,Pn); 
   e:=max degrees ideal Phi; if #e==1 then e=first e;
   Z:=transpose gens image basis(d*e,ideal target phi); 
   mm:=if source phi === Pm then transpose gens (ideal vars Pm)^d else transpose lift(gens image basis(d,ideal vars source phi),Pm); 
   f:=numgens target mm -1; g:=numgens target Z -1;
   a:=local a; b:=local b; K:=coefficientRing Pm;
   AB:=K[a_0..a_f,b_0..b_g]; A:=matrix{{a_0..a_f}}; B:=matrix{{b_0..b_g}};
   x:=local x; y:=local y; Pn':=AB[x_0..x_(numgens Pn-1)]; Pm':=AB[y_0..y_(numgens Pm-1)]; 
   pol:=(map(Pn',Pm',sub(Phi,vars Pn'))) (A * sub(mm,vars Pm')) - (B * sub(Z,vars Pn')); 
   eqs:=trim ideal sub(last coefficients pol,AB);
   trim sub(ideal(submatrix(transpose mingens kernel transpose sub(last coefficients(gens eqs,Monomials=>(vars AB)),K),{0..f})*mm),source phi)
);

forceImage (RationalMap,Ideal) := (Phi,I) -> (
   if Phi#"idealImage" =!= null then error "not permitted to reassign image of rational map";
   if not(isHomogeneous I and ring I === target Phi) then error "expected homogeneous ideal in the coordinate ring of the target variety";
   setKeyValue(Phi,"idealImage",trim I);
);

forceImage (MultihomogeneousRationalMap,Ideal) := (Phi,I) -> (
   if Phi#"idealImage" =!= null then error "not permitted to reassign image of rational map";
   if not(isHomogeneous I and ring I === target Phi) then error "expected homogeneous ideal in the coordinate ring of the target variety";
   setKeyValue(Phi,"idealImage",trim I);
);

image (RationalMap,String) := (phi,alg) -> (
   if alg =!= "F4" and alg =!= "MGB" then error "expected Strategy to be \"F4\" or \"MGB\"";
   if phi#"idealImage" =!= null then return image phi;
   n := phi#"dimAmbientTarget";
   m := phi#"dimAmbientSource";
   K := coefficientRing phi;
   t := local t; x := local x;
   R := K[t_0..t_n,x_0..x_m,MonomialOrder=>Eliminate(n+1)];
   s := map(R,ambient source phi,{t_0..t_n});
   F := s lift(matrix phi,ambient source phi);
   I := s ideal source phi;  
   s':= map(R,ambient target phi,{x_0..x_m});
   J := s' ideal target phi;
   V := I + J + ideal(F - matrix{{x_0..x_m}});
   G := groebnerBasis(V,Strategy=>alg);
   G' := ideal sub(selectInSubring(1,G),K[x_0..x_m]);
   forceImage(phi,sub(sub(G',vars ambient target phi),target phi));
   image phi
);

image (MultihomogeneousRationalMap,String) := (phi,alg) -> (
   if alg =!= "F4" and alg =!= "MGB" then error "expected Strategy to be \"F4\" or \"MGB\"";
   error "not implemented yet in the case the source is a multi-projective variety";
);

matrix (RationalMap) := o -> (Phi) -> toMatrix map Phi;

matrix (ZZ,RationalMap) := o -> (i,Phi) -> (
    if i < 0 then error "expected a nonnegative integer";
    if i == 0 then return matrix Phi;
    n := # maps Phi;
    if i > n-1 then error("the number of minimal representatives of the map is "|toString(n));
    toMatrix (Phi#"maps")_i
);

matrix (MultihomogeneousRationalMap) := o -> (Phi) -> toMatrix map Phi;

matrix (ZZ,MultihomogeneousRationalMap) := o -> (i,Phi) -> (
    if i < 0 then error "expected a nonnegative integer";
    if i == 0 then return matrix Phi;
    n := # maps Phi;
    if i > n-1 then error("the number of minimal representatives of the map is "|toString(n));
    toMatrix (Phi#"maps")_i
);

coefficients (RationalMap) := o -> (Phi) ->  ( -- matrix M s.t M * (transpose gens((ideal vars R)^d)) == transpose F
    if o.Variables =!= null then error "option not available";
    R:=ambient target Phi#"map";
    F:=lift(matrix Phi,R);
    d:=max flatten degrees ideal compress F;
    mons:=if o.Monomials === null then gens((ideal vars R)^d) else o.Monomials;
    M:=transpose sub(last coefficients(F,Monomials=>mons),coefficientRing R);
    -- if class mons === List then mons = matrix{mons}; 
    -- try assert(M * (transpose mons) - transpose F == 0) else error "internal error encountered";
    M
);

entries (RationalMap) := (Phi) -> flatten entries matrix Phi;

entries (MultihomogeneousRationalMap) := (Phi) -> flatten entries matrix Phi;

source (RationalMap) := (Phi) -> target Phi#"map";

source (MultihomogeneousRationalMap) := (Phi) -> target Phi#"map";

target (RationalMap) := (Phi) -> source Phi#"map";

target (MultihomogeneousRationalMap) := (Phi) -> source Phi#"map";

coefficientRing (RationalMap) := (Phi) -> coefficientRing source Phi;

coefficientRing (MultihomogeneousRationalMap) := (Phi) -> coefficientRing source Phi;

directImageInt = method()

directImageInt (MutableHashTable,Ideal) := (Phi,I) -> (
    if (ring I =!= source Phi and ring I === ambient source Phi) then return directImageInt(Phi,sub(I,source Phi)); 
    if not (ring I === source Phi and isHomogeneous I) then error "expected homogeneous ideal in the coordinate ring of the source variety";
    trim preimage(map Phi,I)
);

RationalMap SPACE Ideal := (Phi,I) -> directImageInt(Phi,I);

MultihomogeneousRationalMap SPACE Ideal := (Phi,I) -> directImageInt(Phi,I);

directImageStrongInt = method()

directImageStrongInt (MutableHashTable,Ideal) := (Phi,I) -> (
    if (ring I =!= source Phi and ring I === ambient source Phi) then return directImageStrongInt(Phi,sub(I,source Phi)); 
    if not (ring I === source Phi and isHomogeneous I) then error "expected homogeneous ideal in the coordinate ring of the source variety";
    trim intersect apply(maps Phi,F -> preimage(F,I))
);

RationalMap _* := (Phi) -> Ideal := (I) -> directImageStrongInt(Phi,I);

MultihomogeneousRationalMap _* := (Phi) -> Ideal := (I) -> directImageStrongInt(Phi,I);

inverseImageStrongInt = method() 

inverseImageStrongInt (MutableHashTable,Ideal) := (Phi,I) -> (
   if (ring I =!= target Phi and ring I === ambient target Phi) then return inverseImageStrongInt(Phi,sub(I,target Phi)); 
   Z := intersect apply(maps Phi,F -> inverseImage(F,I,MathMode=>true));
-- if #(heft ambient ring Z) > 1 then Z = multisaturate Z;
   return Z;
);

RationalMap ^** Ideal := (Phi,I) -> inverseImageStrongInt(Phi,I);

MultihomogeneousRationalMap ^** Ideal := (Phi,I) -> inverseImageStrongInt(Phi,I);

inverseImageWeakInt = method() 

inverseImageWeakInt (MutableHashTable,Ideal) := (Phi,I) -> (
   if (ring I =!= target Phi and ring I === ambient target Phi) then return inverseImageWeakInt(Phi,sub(I,target Phi)); 
   Z := inverseImage(map Phi,I,MathMode=>false);
-- if #(heft ambient ring Z) > 1 then Z = multisaturate Z;
   return Z;
);

RationalMap ^* := (Phi) -> Ideal := (I) -> inverseImageWeakInt(Phi,I);

MultihomogeneousRationalMap ^* := (Phi) -> Ideal := (I) -> inverseImageWeakInt(Phi,I);

inverseImage = method(TypicalValue => Ideal, Options => {MathMode => false});
inverseImage (RingMap,Ideal) := o -> (phi,J) -> (
   if source phi =!= ring J then error "expected homogeneous ideal in the coordinate ring of the target variety";
   B:=ideal toMatrix phi;
   K:=coefficientRing ring B;
   if o.MathMode or class K === FractionField then return saturate(phi J,B);
   F:=ideal sum for i to numgens B -1 list random(K) * B_i;
   saturate(phi J,F)
);

RationalMap ^ ZZ := (Phi,j) -> (
   if j == 0 then (
         if (target Phi#"map" === source Phi#"map") then return rationalMap(target Phi#"map") else error "expected non-zero integer";
   );
   if j < 0 then (
         Psi:=inverse Phi;
         return(Psi^(-j))
   );
   Psi2:=Phi; for i from 1 to j-1 do Psi2 = Psi2 * Phi; 
   return Psi2;
);

inverse (RationalMap,Option) := (Phi,opt) -> (
   o := toList opt;
   if not(#o == 2 and first o === MathMode) then error "MathMode is the only available option for inverse(RationalMap)";
   if not instance(last o,Boolean) then error "option MathMode accepts true or false";
   if Phi#"inverseRationalMap" =!= null then return Phi#"inverseRationalMap";
   if last o then return inverseMap(Phi,MathMode=>true,Verbose=>false);
   Psi := inverseMap Phi;
   if not isInverseMapFast(Phi,Psi) then error "failed to get the inverse map; the map may not be birational";
   if Phi#"inverseRationalMap" === null and Psi#"inverseRationalMap" === null then forceInverseMap(Phi,Psi); 
   Psi
);

inverse (RationalMap) := (Phi) -> inverse(Phi,MathMode=>false);

RationalMap ! := (Phi) -> (
     toMap Phi;
     try inverse Phi;
     degrees Phi;
     degree Phi;
     isDominant(Phi,MathMode=>true,Verbose=>false);
     isBirational(Phi,MathMode=>true,Verbose=>false);
     image Phi;
     return Phi;
);

MultihomogeneousRationalMap ! := (Phi) -> (  
     ideal Phi;
     degrees Phi;
     degree Phi;
     isDominant(Phi,MathMode=>true,Verbose=>false);
     isBirational(Phi,MathMode=>true,Verbose=>false);
     image Phi;
     return Phi;
);

RationalMap (*) := (Phi) -> (
     if Phi#"projectiveDegrees" === {} then setKeyValue(Phi,"projectiveDegrees",projectiveDegrees Phi);
     if Phi#"degree" === null then setKeyValue(Phi,"degree",degreeMap Phi);
     if Phi#"isDominant" === null then setKeyValue(Phi,"isDominant",isDominant Phi);
     if Phi#"isBirational" === null then setKeyValue(Phi,"isBirational",isBirational Phi);
     if Phi#"idealImage" === null and isPolynomialRing target Phi and degree Phi != 0 then forceImage(Phi,approximateImage(Phi,Verbose=>false));
     return Phi;
);

approximateImage = method(TypicalValue => Ideal, Options => {Verbose => true}); -- not exported yet
approximateImage (RationalMap) := o -> (Phi) -> ( 
    -- Experimental method to compute the image of a rational map
    -- The output represents a scheme containing the image and having the same dimension and degree.
    if Phi#"idealImage" =!= null then return Phi#"idealImage";
    if not isPolynomialRing target Phi then error "not implemented yet: approximateImage of a rational map with target different from a projective space";
    n := Phi#"dimTarget";
    d0 := degreeMap Phi;
    if d0 == 0 then error "not implemented yet: approximateImage of a rational map of degree 0";
    pr0 := first projectiveDegrees(Phi,NumDegrees=>0);
    d := lift(pr0/d0,ZZ);
    local I;
    for i from 1 to 3 do (
        if o.Verbose then <<"-- calculating kernel(...,"<<i<<")..."<<endl;
        I = image(i,Phi); 
        if I != 0 then break;
    );
    if dim I -1 == n and degree I == d then return I;
    u := unique flatten degrees I;
    if u === {1} or u === {2} then (
        u = first u;
        if o.Verbose then <<"-- calculating saturate kernel(...,"<<u+1<<")..."<<endl;
        I = saturate image(u+1,Phi); 
    ); 
    local f;
    while dim I -1 != n or degree I != d do (
        f = map(source Phi,(target Phi)/I,matrix Phi);
        if o.Verbose then <<"-- searching for another generator..."<<endl;
        I = trim(I + lift(kernel(f,SubringLimit=>1),target Phi));
    );
    return I;
);

restrictionMapInt = method()

restrictionMapInt (MutableHashTable,Ideal) := (Phi,J) -> ( -- restriction of Phi to V(J)
    Pn1Pn2Pn3 := ambient source Phi;
    if not ((ring J === source Phi or ring J === Pn1Pn2Pn3) and isHomogeneous J) then error "expected homogeneous ideal in the coordinate ring of the source variety";
    rationalMap (map(Pn1Pn2Pn3/trim(lift(J,Pn1Pn2Pn3) + ideal source Phi),source Phi,vars Pn1Pn2Pn3) * map Phi) 
);

restrictionMapInt (MutableHashTable,RingElement) := (Phi,F) -> restrictionMapInt(Phi,ideal F);

restrictionMapInt (MutableHashTable,Ring) := (Phi,S) -> (
    if not (ambient S === ambient source Phi and isSubset(ideal source Phi,ideal S) and isHomogeneous ideal S) then error "expected homogeneous coordinate ring of a subvariety of the source variety";
    Psi := restrictionMapInt(Phi,ideal(S));
    rationalMap (map(S,source Psi,vars ambient S) * map Psi)
);

RationalMap | Ideal := (Phi,J) -> restrictionMapInt(Phi,J);

RationalMap | RingElement := (Phi,F) -> restrictionMapInt(Phi,F);

RationalMap | Ring := (Phi,S) -> restrictionMapInt(Phi,S);

MultihomogeneousRationalMap | Ideal := (Phi,J) -> restrictionMapInt(Phi,J);

MultihomogeneousRationalMap | RingElement := (Phi,F) -> restrictionMapInt(Phi,F);

MultihomogeneousRationalMap | Ring := (Phi,S) -> restrictionMapInt(Phi,S);

restrictionMapInt2 = method()

restrictionMapInt2 (MutableHashTable,Ideal) := (Phi,J) -> ( -- restriction of Phi to Phi^(-1)(V(J)) ---> V(J)
    Pn1Pn2Pn3 := ambient source Phi;
    Pm := ambient target Phi;
    J':= trim(lift(Phi^* J,Pn1Pn2Pn3) + ideal source Phi);
    J  = trim(lift(J,Pm) + ideal target Phi);
    rationalMap map(Pn1Pn2Pn3/J',Pm/J,lift(matrix Phi,Pn1Pn2Pn3))
);

restrictionMapInt2 (MutableHashTable,RingElement) := (Phi,F) -> restrictionMapInt2(Phi,ideal F);

restrictionMapInt2 (MutableHashTable,Ring) := (Phi,S) -> (
    if not (ambient S === ambient target Phi and isSubset(ideal target Phi,ideal S) and isHomogeneous ideal S) then error "expected homogeneous coordinate ring of a subvariety of the target variety";
    Psi := restrictionMapInt2(Phi,ideal(S));
    Psi * rationalMap(target Psi,S)
);

RationalMap || Ideal := (Phi,J) -> restrictionMapInt2(Phi,J);

RationalMap || RingElement := (Phi,F) -> restrictionMapInt2(Phi,F);

RationalMap || Ring := (Phi,S) -> restrictionMapInt2(Phi,S);

MultihomogeneousRationalMap || Ideal := (Phi,J) -> restrictionMapInt2(Phi,J);

MultihomogeneousRationalMap || RingElement := (Phi,F) -> restrictionMapInt2(Phi,F);

MultihomogeneousRationalMap || Ring := (Phi,S) -> restrictionMapInt2(Phi,S);

compareRationalMapsInt = method()

compareRationalMapsInt (MutableHashTable,MutableHashTable) := (Phi,Psi) -> ( 
   if not areEqualMaps(Phi#"map",Psi#"map") then return false;
           if Phi#"isDominant" =!= Psi#"isDominant" then (
                    if Phi#"isDominant" =!= null and Psi#"isDominant" =!= null then error("internal error encountered");
                    if Phi#"isDominant" === null then setKeyValue(Phi,"isDominant",Psi#"isDominant");
                    if Psi#"isDominant" === null then setKeyValue(Psi,"isDominant",Phi#"isDominant");
           );
           if Phi#"isBirational" =!= Psi#"isBirational" then (
                    if Phi#"isBirational" =!= null and Psi#"isBirational" =!= null then error("internal error encountered");
                    if Phi#"isBirational" === null then setKeyValue(Phi,"isBirational",Psi#"isBirational");
                    if Psi#"isBirational" === null then setKeyValue(Psi,"isBirational",Phi#"isBirational");
           );
           if Phi#"projectiveDegrees" =!= Psi#"projectiveDegrees" then (
                    if Phi#"projectiveDegrees" =!= {} and Psi#"projectiveDegrees" =!= {} then error("internal error encountered");
                    if Phi#"projectiveDegrees" === {} then setKeyValue(Phi,"projectiveDegrees",Psi#"projectiveDegrees");
                    if Psi#"projectiveDegrees" === {} then setKeyValue(Psi,"projectiveDegrees",Phi#"projectiveDegrees");
           );
           if Phi#"degree" =!= Psi#"degree" then (
                    if Phi#"degree" =!= null and Psi#"degree" =!= null then error("internal error encountered");
                    if Phi#"degree" === null then setKeyValue(Phi,"degree",Psi#"degree");
                    if Psi#"degree" === null then setKeyValue(Psi,"degree",Phi#"degree");
           );
           -- if Phi#"idealImage" =!= null and Psi#"idealImage" =!= null then (if Phi#"idealImage" != Psi#"idealImage" then error("internal error encountered"));
           if Phi#"idealImage" === null and Psi#"idealImage" =!= null then setKeyValue(Phi,"idealImage",Psi#"idealImage");
           if Phi#"idealImage" =!= null and Psi#"idealImage" === null then setKeyValue(Psi,"idealImage",Phi#"idealImage");
           if Phi#"maps" =!= null and Psi#"maps" =!= null then (if #(Phi#"maps") =!= #(Psi#"maps") then error("internal error encountered"));
           if Phi#"maps" === null and Psi#"maps" =!= null then setKeyValue(Phi,"maps",Psi#"maps");
           if Phi#"maps" =!= null and Psi#"maps" === null then setKeyValue(Psi,"maps",Phi#"maps");
           if Phi#"blowUpIdeal" === null and Psi#"blowUpIdeal" =!= null then Phi#"blowUpIdeal" = Psi#"blowUpIdeal";
           if Phi#"blowUpIdeal" =!= null and Psi#"blowUpIdeal" === null then Psi#"blowUpIdeal" = Phi#"blowUpIdeal";
           if class Phi === RationalMap and class Psi === RationalMap then (
              if Phi#"inverseRationalMap" === null and Psi#"inverseRationalMap" =!= null then Phi#"inverseRationalMap" = Psi#"inverseRationalMap";
              if Phi#"inverseRationalMap" =!= null and Psi#"inverseRationalMap" === null then Psi#"inverseRationalMap" = Phi#"inverseRationalMap";
           );
           if class Phi === MultihomogeneousRationalMap and class Psi === MultihomogeneousRationalMap then (
              if Phi#"baseLocus" === null and Psi#"baseLocus" =!= null then Phi#"baseLocus" = Psi#"baseLocus";
              if Phi#"baseLocus" =!= null and Psi#"baseLocus" === null then Psi#"baseLocus" = Phi#"baseLocus";
           );
   return true;
);

RationalMap == RationalMap := (Phi,Psi) -> compareRationalMapsInt(Phi,Psi);

MultihomogeneousRationalMap == MultihomogeneousRationalMap := (Phi,Psi) -> compareRationalMapsInt(Phi,Psi); 

MultihomogeneousRationalMap == RationalMap := (Phi,Psi) -> compareRationalMapsInt(Phi,Psi);

RationalMap == MultihomogeneousRationalMap := (Phi,Psi) -> compareRationalMapsInt(Phi,Psi);

RationalMap == ZZ := (Phi,n) -> (
    if n == 1 then return compareRationalMapsInt(Phi,rationalMap(target Phi#"map")) else error "encountered integer other than 1 in comparison with a rational map";
);

ZZ == RationalMap := (n,Phi) -> Phi == n;

ideal (RationalMap) := (Phi) -> (
    trim sum apply(maps Phi,F -> ideal toMatrix F)
);
ideal (MultihomogeneousRationalMap) := (Phi) -> (
    if Phi#"baseLocus" === null then Phi#"baseLocus" = multisaturate sum apply(maps Phi,F -> ideal toMatrix F);
    return Phi#"baseLocus";
);

isDominantInt = method(Options => {MathMode => false, Verbose => true})

isDominantInt (MutableHashTable) := o -> (Phi) -> ( 
   if Phi#"isDominant" =!= null then (if o.MathMode and o.Verbose then <<certificate; return Phi#"isDominant");
   if o.MathMode then (
       setKeyValue(Phi,"isDominant",isDominantMath(map Phi,Phi#"dimTarget",Phi#"dimSource",o.Verbose));
       return Phi#"isDominant";
   );
   if Phi#"dimTarget" < Phi#"dimSource" then (
       setKeyValue(Phi,"isDominant",false);
       return Phi#"isDominant";
   ); 
   if class Phi === RationalMap then (
       phi := Phi#"map";
       for i from 1 to Phi#"dimTarget" - Phi#"dimSource" do phi = genericRestriction phi;
       return (first projectiveDegrees(phi,NumDegrees=>0) != 0);
   );
   if class Phi === MultihomogeneousRationalMap then (
       Z := sub((ideal source Phi#"map") + randomLinearSubspace(ambient source Phi#"map",Phi#"dimAmbientSource" - Phi#"dimSource"),source Phi#"map");
       return (dim inverseImage(Phi#"map",Z,MathMode=>false) - (# heft ambient target Phi#"map") == Phi#"dimTarget" - Phi#"dimSource");
   );
);

isDominant (RationalMap) := o -> (Phi) -> isDominantInt(Phi,MathMode=>o.MathMode,Verbose=>o.Verbose);

isDominant (MultihomogeneousRationalMap) := o -> (Phi) -> isDominantInt(Phi,MathMode=>o.MathMode,Verbose=>o.Verbose);

isDominant (RingMap) := o -> (phi) -> (
   checkRationalMap phi;
   isDominant(rationalMapWithoutChecking phi,MathMode=>o.MathMode,Verbose=>o.Verbose)
);

isDominantMath = (phi,n,m,MathVerb) -> (
   -- phi:X--->Y multihomogeneous map, n = dim X, m = dim Y
   X := target phi; Y := source phi;
   R := ambient X;
   PM := ambient Y;
   M := numgens PM -1;
   if n < m then (if MathVerb then <<certificate; return false);
   -- if there exists Z subset Y (with dim Z = 0) s.t dim phi^(-1)(Z) = n-m, then phi is dominant
   Z := ideal(Y) + randomLinearSubspace(PM,M-m);
   while (-1 + dim(Z) != 0) do Z=ideal(Y) + randomLinearSubspace(PM,M-m);
   Z = sub(Z,Y);
   if dim inverseImage(phi,Z,MathMode=>true) - (# heft R) == n-m then (if MathVerb then <<certificate; return true);
   isDom := kernel(phi,SubringLimit=>1) == 0; 
   if MathVerb then <<certificate;
   return isDom;
);

isBirationalInt = method(Options => {MathMode => false, BlowUpStrategy => "Eliminate", Verbose => true});

isBirationalInt (MutableHashTable) := o -> (Phi) -> (
   if Phi#"isBirational" =!= null then (if o.MathMode and o.Verbose then <<certificate; return Phi#"isBirational");
   if Phi#"dimTarget" != Phi#"dimSource" then (
          setKeyValue(Phi,"isBirational",false);
          if o.MathMode and o.Verbose then <<certificate;
          return Phi#"isBirational";
   );
   if o.MathMode then if not isDominant(Phi,MathMode=>true,Verbose=>false) then (
        setKeyValue(Phi,"isBirational",false); 
        if o.Verbose then <<certificate; 
        return Phi#"isBirational");      
   isB := first projectiveDegrees(Phi,NumDegrees=>0,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose) == degree ideal target Phi;
   if o.MathMode then (setKeyValue(Phi,"isBirational",isB); return Phi#"isBirational") else return isB;
);

isBirational (RationalMap) := o -> (Phi) -> isBirationalInt(Phi,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);

isBirational (MultihomogeneousRationalMap) := o -> (Phi) -> isBirationalInt(Phi,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);

isBirational (RingMap) := o -> (phi) -> (
   checkRationalMap phi;
   isBirational(rationalMapWithoutChecking phi,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose)
);

inverseMapInt = method(Options => {MathMode => false, BlowUpStrategy => "Eliminate", Verbose => true});

inverseMapInt (RationalMap) := o -> (Phi) -> (
   if Phi#"dimTarget" != Phi#"dimSource" then error "expected a birational map";
   if # select((flatten degrees ideal source Phi)|(flatten degrees ideal target Phi)|(flatten degrees ideal matrix Phi),g -> g > 1) == 0 then (
       try eta := inverse map Phi else error "linear rational map not invertible";
       if not o.MathMode then return (eta,null) else (
           if (isInverseMap(Phi#"map",eta) and isInverseMap(eta,Phi#"map")) then (if o.Verbose then <<certificate; return (eta,null);)
           else error "linear rational map not invertible";
       );
   );
   if not(isPolynomialRing source Phi and Phi#"dimTarget" > 0) then return inverseMapInt(Phi,null,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);
   GAll := try invertBirationalMapRS(matrix Phi,ideal target Phi) else {matrix{{(numgens ambient source Phi):0_(target Phi)}}}; 
   G := first GAll;
   if not (min flatten degrees ideal G > 0 and compress G === G) then return inverseMapInt(Phi,null,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);
   psiAll := apply(GAll,g -> map(target Phi,source Phi,g));
   psi := first psiAll;
   if not o.MathMode then return (psi,psiAll);
   if isInverseMap(Phi#"map",psi) then (
        if o.Verbose then <<certificate; return (psi,psiAll);
   ) else (
        return inverseMapInt(Phi,null,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);
   );
);

inverseMapInt (RationalMap,Nothing) := o -> (Phi,nothing) -> (
   if Phi#"dimTarget" != Phi#"dimSource" then error "expected a birational map";
   Bl := graphIdealInt(Phi,BlowUpStrategy=>o.BlowUpStrategy);
   n := Phi#"dimAmbientTarget"; 
   Sub := map(target Phi,ring Bl,matrix{{(n+1):0_(ambient target Phi)}}|(vars ambient target Phi));
   T := transpose mingens kernel transpose Sub submatrix(jacobian Bl,{0..n},);
   psi := try map rationalMap(target Phi,source Phi,submatrix(T,{0},)) else error "not able to obtain an inverse rational map; the map may not be birational";
   psiAll := apply(numRows T,i -> map(target Phi,source Phi,submatrix(T,{i},)));
   if not o.MathMode then return (psi,psiAll);
   if (if n - Phi#"dimTarget" <= Phi#"dimAmbientSource" - Phi#"dimSource" then isInverseMap(map Phi,psi) else isInverseMap(psi,map Phi)) then (
        if o.Verbose then <<certificate; return (psi,psiAll);
   ) else (
        error "not able to obtain an inverse rational map; the map may not be birational";
   );
);

inverseMap (RationalMap) := o -> (Phi) -> (
    if Phi#"inverseRationalMap" =!= null then (if o.MathMode and o.Verbose then <<certificate; return Phi#"inverseRationalMap");
    if Phi#"isBirational" === false or Phi#"isDominant" === false then error "expected a birational map";
    if Phi#"isBirational" === null then if Phi#"dimTarget" != Phi#"dimSource" then (setKeyValue(Phi,"isBirational",false); error "expected a birational map";);
    U := inverseMapInt(Phi,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);
    Psi := new RationalMap from {
            "map" => first U,
            "maps" => last U,
            "isDominant" => if o.MathMode then true else null,
            "idealImage" => if o.MathMode then trim ideal(0_(target Phi#"map")) else null,
            "isBirational" => if o.MathMode then true else null,
            "inverseRationalMap" => if o.MathMode then Phi else null,
            "projectiveDegrees" => if o.MathMode then reverse Phi#"projectiveDegrees" else {},
            "degree" => if o.MathMode then 1 else null,
            "dimAmbientTarget" => Phi#"dimAmbientSource",
            "dimTarget" => Phi#"dimSource",
            "dimAmbientSource" => Phi#"dimAmbientTarget",
            "dimSource" => Phi#"dimTarget",
            "blowUpIdeal" => null
           };
    if o.MathMode then (     
         if Phi#"isBirational" =!= true then setKeyValue(Phi,"isBirational",true);  
         if Phi#"isDominant" =!= true then setKeyValue(Phi,"isDominant",true); 
         if Phi#"degree" =!= 1 then setKeyValue(Phi,"degree",1);
         if Phi#"idealImage" === null then setKeyValue(Phi,"idealImage",trim ideal(0_(source Phi#"map")));        
         setKeyValue(Phi,"inverseRationalMap",Psi);
         return Phi#"inverseRationalMap";
    ) else return Psi;
);

inverseMap (RationalMap,Nothing) := o -> (Phi,nothing) -> ( -- code copied from above -- undocumented 
    if Phi#"inverseRationalMap" =!= null then (if o.MathMode and o.Verbose then <<certificate; return Phi#"inverseRationalMap");
    if Phi#"isBirational" === false or Phi#"isDominant" === false then error "expected a birational map";
    if Phi#"isBirational" === null then if Phi#"dimTarget" != Phi#"dimSource" then (setKeyValue(Phi,"isBirational",false); error "expected a birational map";);
    U := inverseMapInt(Phi,null,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);
    Psi := new RationalMap from {
            "map" => first U,
            "maps" => last U,
            "isDominant" => if o.MathMode then true else null,
            "idealImage" => if o.MathMode then trim ideal(0_(target Phi#"map")) else null,
            "isBirational" => if o.MathMode then true else null,
            "inverseRationalMap" => if o.MathMode then Phi else null,
            "projectiveDegrees" => if o.MathMode then reverse Phi#"projectiveDegrees" else {},
            "degree" => if o.MathMode then 1 else null,
            "dimAmbientTarget" => Phi#"dimAmbientSource",
            "dimTarget" => Phi#"dimSource",
            "dimAmbientSource" => Phi#"dimAmbientTarget",
            "dimSource" => Phi#"dimTarget",
            "blowUpIdeal" => null
           };
    if o.MathMode then (     
         if Phi#"isBirational" =!= true then setKeyValue(Phi,"isBirational",true);  
         if Phi#"isDominant" =!= true then setKeyValue(Phi,"isDominant",true); 
         if Phi#"degree" =!= 1 then setKeyValue(Phi,"degree",1);
         if Phi#"idealImage" === null then setKeyValue(Phi,"idealImage",trim ideal(0_(source Phi#"map")));        
         setKeyValue(Phi,"inverseRationalMap",Psi);
         return Phi#"inverseRationalMap";
    ) else return Psi;
);

inverseMap (MultihomogeneousRationalMap) := o -> (Phi) -> error "not implemented yet: inverse map of a birational map from a multi-projective variety";

inverseMap (RingMap) := o -> (phi) -> (
   checkRationalMap phi;
   map inverseMap(rationalMapWithoutChecking phi,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose)
);

invertBirationalMapRS = (F,a)  -> ( 
   -- Notation as in the paper [Russo, Simis - On Birational Maps and Jacobian Matrices] 
   -- Computes the inverse map of a birational map via the Russo and Simis's algorithm
   -- input: 1) row matrix, representing a birational map P^n-->P^m 
   --        2) ideal, representing the image of the map 
   -- output: row matrix, representing the inverse map   
   n:=numgens ring F-1;
   x:=local x;
   R:=coefficientRing(ring F)[x_0..x_n];
   I:=sub(F,vars R);
   S:=ring a;
   phi:=syz I;
   local q;
   for j to numgens source phi-1 do if max degrees ideal matrix phi_j == {1} then q=j+1;
   phi1:=submatrix(phi,{0..q-1});
   RtensorS:=tensor(R,S);
   phi1=sub(phi1,RtensorS);
   Y:=sub(vars S,RtensorS);
   theta:=transpose submatrix(jacobian ideal(Y*phi1),{0..n},);
   theta=sub(theta,S);
   S':=S/a; theta':=sub(theta,S');
   Z:=kernel theta';
   basisZ:=mingens Z; 
   if numgens source basisZ == 0 then error("it has not been possible to determine the inverse rational map");
   apply(numgens source basisZ,i -> transpose matrix sub(basisZ_i,S))
);

approximateInverseMap (RingMap,ZZ) := o -> (phi,d) -> (
    -- input: a birational map phi:X --->Y 
    -- output: a map Y--->X in some sense related to the inverse of phi
    checkRationalMap phi;
    if dim target phi != dim source phi then error "expected a birational map";
    n:=numgens ambient target phi -1;
    c:=2;
    if o.CodimBsInv =!= null then (if (try (class o.CodimBsInv === ZZ and o.CodimBsInv >= 2 and o.CodimBsInv <= n+1) else false) then c=o.CodimBsInv else (<<"--warning: option CodimBsInv ignored"<<endl));
    phiRes:=local phiRes;
    M:=ceiling((n+1)/(c-1));
    B:=trim sum for i from 1 to M list (
         if o.Verbose then <<"-- approximateInverseMap: step "<<i<<" of "<<M<<endl;
         phiRes=phi;
         for i0 from 1 to c-1 do phiRes=genericRestriction phiRes; 
         if d<=0 then kernel(phiRes,SubringLimit=>(c-1)) else kernel(phiRes,d)  
       );
   if not(numgens B <= n+1 and min flatten degrees B == max flatten degrees B) then error("unable to define an inverse map: found "|toString(numgens B)|" generators of degrees "|toString(flatten degrees B));
   if numgens B < n+1 then B=B+ideal((n+1-numgens(B)) : 0_(ring B));
   psi:=if isPolynomialRing target phi then map(source phi,target phi,gens B) else toMap(map(source phi,ambient target phi,gens B),Dominant=>ideal(target phi));
   if o.MathMode then (
          if isPolynomialRing target phi then (
                 try psi=compose(psi,toMap((vars target phi)*(last coefficients matrix compose(phi,psi))^(-1)));
                 if isInverseMapFast(phi,psi) then (if o.Verbose then <<certificate; return psi) else error("MathMode: approximateInverseMap returned "|toExternalString(psi)|" but this is not the inverse map");
          ) else (
                 if source psi =!= target phi then error("MathMode: approximateInverseMap returned "|toExternalString(psi)|" but this map has an incorrect target variety")
                 else if isInverseMapFast(phi,psi) then (if o.Verbose then <<certificate; return psi) else error("MathMode: approximateInverseMap returned "|toExternalString(psi)|" but this is not the inverse map");
          );
   );
   return psi;
);

approximateInverseMap (RingMap) := o -> (phi) -> approximateInverseMap(phi,-1,CodimBsInv=>o.CodimBsInv,MathMode=>o.MathMode,Verbose=>o.Verbose);

approximateInverseMap (RationalMap,ZZ) := o -> (Phi,d) -> (
    if Phi#"inverseRationalMap" =!= null then (if o.MathMode and o.Verbose then <<certificate; return Phi#"inverseRationalMap");
    if Phi#"isBirational" === false or Phi#"isDominant" === false then error "expected a birational map";
    if Phi#"isBirational" === null then if Phi#"dimTarget" != Phi#"dimSource" then (setKeyValue(Phi,"isBirational",false); error "expected a birational map";);
    Psi := rationalMap approximateInverseMap(Phi#"map",d,CodimBsInv=>o.CodimBsInv,MathMode=>o.MathMode,Verbose=>o.Verbose);
    if (not o.MathMode) then return Psi;
    setKeyValue(Psi,"isBirational",true);
    if Phi#"projectiveDegrees" =!= {} then setKeyValue(Psi,"projectiveDegrees",reverse Phi#"projectiveDegrees"); 
    setKeyValue(Psi,"inverseRationalMap",Phi);
    if Phi#"isBirational" =!= true then setKeyValue(Phi,"isBirational",true);  
    if Phi#"isDominant" =!= true then setKeyValue(Phi,"isDominant",true); 
    if Phi#"degree" =!= 1 then setKeyValue(Phi,"degree",1);
    if Phi#"idealImage" === null then setKeyValue(Phi,"idealImage",trim ideal(0_(source Phi#"map")));        
    setKeyValue(Phi,"inverseRationalMap",Psi);       
    return Phi#"inverseRationalMap";
);

approximateInverseMap (RationalMap) := o -> (Phi) -> approximateInverseMap(Phi,-1,CodimBsInv=>o.CodimBsInv,MathMode=>o.MathMode,Verbose=>o.Verbose);

isInverseMap (RationalMap,RationalMap) := (Phi,Psi) -> (
   if Phi#"inverseRationalMap" =!= null and Psi#"inverseRationalMap" =!= null then if Phi#"inverseRationalMap" === Psi and Psi#"inverseRationalMap" === Phi then return true;
   if Phi#"dimTarget" != Psi#"dimTarget" then return false;
   T := if Phi#"dimAmbientTarget" - Phi#"dimTarget" <= Psi#"dimAmbientTarget" - Psi#"dimTarget" then isInverseMap(map Phi,map Psi) else isInverseMap(map Psi,map Phi);
   if not T then return false;
   if Phi#"inverseRationalMap" === null and Psi#"inverseRationalMap" === null then forceInverseMap(Phi,Psi); 
   return true;
);

isInverseMap (RingMap,RingMap) := (phi,psi) -> (
   checkRationalMap phi;
   checkRationalMap psi;
   if (source phi =!= target psi or target phi =!= source psi) then return false; 
   try phipsi:=toMatrix(phi*psi) else return false;
   if unique apply(flatten entries phipsi,degree) === {{0}} then return (phipsi != 0 and dim(ring phipsi)-1 == 0);
   x:=gens target phi; 
   i:=0; while x_i == 0 do i=i+1;
   (q,r):=quotientRemainder((flatten entries phipsi)_i,x_i);
   if r != 0 then return false; 
   if q == 0 then return false;
   phipsi - q*(vars target phi) == 0
);

isInverseMapFast = method(TypicalValue => Boolean);
isInverseMapFast (RationalMap,RationalMap) := (Phi,Psi) -> (
   if Phi#"inverseRationalMap" =!= null and Psi#"inverseRationalMap" =!= null then if Phi#"inverseRationalMap" === Psi and Psi#"inverseRationalMap" === Phi then return true;
   if Phi#"dimTarget" != Psi#"dimTarget" then return false;
   if (source Phi =!= target Psi or target Phi =!= source Psi) then return false; 
   K := coefficientRing Phi;
   if K === QQ then (
       K = ZZ/(nextPrime random(1000,11000000));
       x := local x; y := local y;
       Pn := K[x_0..x_(numgens ambient source Phi -1)];
       Pm := K[y_0..y_(numgens ambient target Phi -1)];
       I := sub(ideal source Phi,vars Pn);
       J := sub(ideal target Phi,vars Pm);
       F := sub(lift(matrix Phi,ambient source Phi),vars Pn);
       G := sub(lift(matrix Psi,ambient target Phi),vars Pm);
       R := Pn/I; S := Pm/J;
       Phi = rationalMapWithoutChecking map(R,S,F);
       Psi = rationalMapWithoutChecking map(S,R,G);
   ); 
   c := if K === ZZ/(char K) then char K else if instance(K,GaloisField) then K#order else 0;
   if c < 50 then return isInverseMap(Phi,Psi);
   if Phi#"dimAmbientTarget" - Phi#"dimTarget" <= Psi#"dimAmbientTarget" - Psi#"dimTarget" then (
       p := point source Phi;
       return (Psi Phi p == p);
   ) else (
       q := point target Phi;
       return (Phi Psi q == q);
   );
);
isInverseMapFast (RingMap,RingMap) := (phi,psi) -> isInverseMapFast(rationalMapWithoutChecking phi,rationalMapWithoutChecking psi);

forceInverseMap (RationalMap,RationalMap) := (Phi,Psi) -> (
     if Phi#"inverseRationalMap" =!= null or Psi#"inverseRationalMap" =!= null then error "not permitted to reassign inverse rational map";
     if source Phi =!= target Psi or target Phi =!= source Psi then error "incompatible target and source";
     if Phi#"dimTarget" != Phi#"dimSource" then setKeyValue(Phi,"isBirational",false);
     if Psi#"dimTarget" != Psi#"dimSource" then setKeyValue(Psi,"isBirational",false);
     if Phi#"isBirational" === false or Psi#"isBirational" === false or Phi#"isDominant" === false or Psi#"isDominant" === false then error "expected two birational maps";
     if Phi#"projectiveDegrees" =!= reverse Psi#"projectiveDegrees" then (
          if Phi#"projectiveDegrees" =!= {} and Psi#"projectiveDegrees" =!= {} then error "incompatible multidegrees";
          if Phi#"projectiveDegrees" === {} then setKeyValue(Phi,"projectiveDegrees",reverse Psi#"projectiveDegrees");
          if Psi#"projectiveDegrees" === {} then setKeyValue(Psi,"projectiveDegrees",reverse Phi#"projectiveDegrees");
     );
     if Phi#"isBirational" =!= true then setKeyValue(Phi,"isBirational",true);  
     if Psi#"isBirational" =!= true then setKeyValue(Psi,"isBirational",true);  
     setKeyValue(Phi,"inverseRationalMap",Psi);
     if Psi#"inverseRationalMap" === null then setKeyValue(Psi,"inverseRationalMap",Phi);
);

compose (RingMap,RingMap) := (f,g) -> (
    if source f =!= target g then error "rational maps not composable: incompatible target and source";
    L := toMatrix (f * g);
    if L == 0 then error "rational maps may not be composable: got the empty map by composing chosen representatives";
    D := try gcd flatten entries compress L else 1_(target f);
    local Q;
    M := if D != 0 and D != 1 then apply(flatten entries L,l -> (Q = quotientRemainder(l,D); assert(last Q == 0); first Q)) else flatten entries L;
    return map(target f,source g,M);
);

composeInt = method()

composeInt (MutableHashTable,MutableHashTable) := (Phi,Psi) -> (
    if target Phi === source Psi then (     
        Eta:=rationalMap(compose(map Phi,map Psi),Dominant=>"notSimplify");
        if Phi#"isDominant" === true then (
            if Psi#"isDominant" === true then setKeyValue(Eta,"isDominant",true);
            if Phi#"degree" =!= null and Psi#"degree" =!= null then setKeyValue(Eta,"degree",(Phi#"degree")*(Psi#"degree"));  
        );
        return Eta;
    );
    if ambient target Phi === ambient source Psi then try return composeInt(Phi,restrictionMapInt(Psi,target Phi));
    error "rational maps not composable: incompatible target and source";
);

compose (RationalMap,RationalMap) := (Phi,Psi) -> composeInt(Phi,Psi);

compose (MultihomogeneousRationalMap,MultihomogeneousRationalMap) := (Phi,Psi) -> composeInt(Phi,Psi);

compose (MultihomogeneousRationalMap,RationalMap) := (Phi,Psi) -> composeInt(Phi,Psi);

compose (RationalMap,MultihomogeneousRationalMap) := (Phi,Psi) -> composeInt(Phi,Psi);

RationalMap * RationalMap := (Phi,Psi) -> compose(Phi,Psi);

MultihomogeneousRationalMap * MultihomogeneousRationalMap := (Phi,Psi) -> compose(Phi,Psi);

MultihomogeneousRationalMap * RationalMap := (Phi,Psi) -> compose(Phi,Psi);

RationalMap * MultihomogeneousRationalMap := (Phi,Psi) -> compose(Phi,Psi);

areEqualMaps = method(TypicalValue => Boolean)
areEqualMaps (RingMap,RingMap) := (phi,psi) -> (
   if (target phi =!= target psi or source phi =!= source psi) then (
      areIso:=try ( ( sub(ideal target phi,vars ambient target psi) == ideal target psi ) and 
                    ( sub(ideal source phi,vars ambient source psi) == ideal source psi ) and 
                    ( minors(2,sub(toMatrix phi,vars ring toMatrix psi)||(toMatrix psi)) == 0 ) and 
                    ( sub(toMatrix phi,vars ring toMatrix psi) != 0 )
                  ) else false; 
      if areIso then error "expected maps with the same source and target; however the input maps are isomorphic" else error "expected maps with the same source and target";
   );
   minors(2,(toMatrix phi)||(toMatrix psi)) == 0 
);

maps = method(TypicalValue => List) -- Simis, Cremona Transformations and some Related Algebras, prop. 1.1

maps (RingMap) := (phi) -> (
   checkMultihomogeneousRationalMap phi;
   T:=entries transpose mingens kernel transpose syz toMatrix phi;
   apply(T,t -> map(target phi,source phi,t))
);

maps (RationalMap) := (Phi) -> mapsInt(Phi);

maps (MultihomogeneousRationalMap) := (Phi) -> mapsInt(Phi);

mapsInt = method()

mapsInt (MutableHashTable) := (Phi) -> (
   if Phi#"maps" === null then (
            if (isPolynomialRing source Phi) then (
                 if codim ideal matrix Phi > 1 then (
                       setKeyValue(Phi,"maps",{Phi#"map"});
                 ) else (
                       setKeyValue(Phi,"maps",{compose(map(source Phi,source Phi,vars source Phi),map Phi)});
                       setKeyValue(Phi,"map",first Phi#"maps");
                 );
            ) else (
                 setKeyValue(Phi,"maps",maps Phi#"map");
                 try apply(Phi#"maps",F -> checkMultihomogeneousRationalMap F) else error "internal error encountered";
                 setKeyValue(Phi,"map",first Phi#"maps");
            );
   );
   Phi#"maps"
);

projectiveDegreesInt = method(Options => {MathMode => false, NumDegrees => infinity, BlowUpStrategy => "Eliminate", Verbose => true});

projectiveDegreesInt (MutableHashTable) := o -> (Phi) -> (
   if o.NumDegrees < 0 then return {};
   n := Phi#"dimAmbientTarget";
   m := Phi#"dimAmbientSource";
   r := Phi#"dimTarget";
   ll := {(r - min(r,o.NumDegrees))..r};
   if Phi#"projectiveDegrees" =!= {} then (if o.MathMode and o.Verbose then <<certificate; return (Phi#"projectiveDegrees")_ll);
   if o.MathMode then (
       Bl := graphIdealInt(Phi,BlowUpStrategy=>o.BlowUpStrategy);
       mdeg := multidegree Bl;
       d := getMultidegree(mdeg,n,m,r);
       setKeyValue(Phi,"projectiveDegrees",d);
       if o.Verbose then <<certificate;
       return d_ll;
   ) else (
       phi := Phi#"map";
       if class Phi === RationalMap then (
           L := {projDegree(phi,0,r,{})}; 
           L = L | for i from 1 to min(r,o.NumDegrees) list (phi = genericRestriction phi; projDegree(phi,0,r-i,{}));
           return reverse L;
       );
       if class Phi === MultihomogeneousRationalMap then (
           return apply(deepSplice ll,j -> projDegree(phi,r-j,r,n));
       );
   );
);

projectiveDegrees (RationalMap) := o -> (Phi) -> projectiveDegreesInt(Phi,MathMode=>o.MathMode,NumDegrees=>o.NumDegrees,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);

projectiveDegrees (MultihomogeneousRationalMap) := o -> (Phi) -> projectiveDegreesInt(Phi,MathMode=>o.MathMode,NumDegrees=>o.NumDegrees,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);

projectiveDegrees (MutableHashTable,ZZ) := o -> (Phi,i) -> ( -- undocumented
   if i < 0 or i > Phi#"dimTarget" then error("expected integer between 0 and "|toString(Phi#"dimTarget")); 
   if Phi#"projectiveDegrees" =!= {} then return (Phi#"projectiveDegrees")_(Phi#"dimTarget" - i);
   if o.MathMode then error "option MathMode=>true not available for projectiveDegrees(RationalMap,ZZ); you can use the option with projectiveDegrees(RationalMap)"; 
   n := Phi#"dimAmbientTarget"; if class n === ZZ then n = {n};
   projDegree(Phi#"map",i,Phi#"dimTarget",n)
);

projectiveDegrees (RingMap) := o -> (phi) -> (
   checkRationalMap phi;
   projectiveDegrees(rationalMapWithoutChecking phi,MathMode=>o.MathMode,NumDegrees=>o.NumDegrees,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose)
);

projectiveDegrees (RingMap,ZZ) := o -> (phi,i) -> (
   checkRationalMap phi; 
   projectiveDegrees(rationalMapWithoutChecking phi,i,MathMode=>o.MathMode,NumDegrees=>o.NumDegrees,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose)
);

degrees (RationalMap) := (Phi) -> projectiveDegrees(Phi,MathMode=>true,Verbose=>false);

degrees (MultihomogeneousRationalMap) := (Phi) -> projectiveDegrees(Phi,MathMode=>true,Verbose=>false);

multidegree (RationalMap) := (Phi) -> degrees(Phi);

multidegree (MultihomogeneousRationalMap) := (Phi) -> degrees(Phi);

projDegree = method()
projDegree (RingMap,ZZ,ZZ,List) := (phi,i,k,n) -> (
   -- Notation as in [Harris J., Algebraic Geometry, A First Course], p. 240.
   -- phi: X ---> Y \subset P^m, 
   -- i integer, 0 <= i <= k, k=dim X, n = {n_1,n_2,...} if X \subset P^n_1 x P^n_2 x ...
   Y := source phi;
   m := numgens ambient Y -1;
   L := sub(randomLinearSubspace(ambient Y,m-k+i),Y);
   Z := inverseImage(phi,L,MathMode=>false); 
   if #n <= 1 then (
      if dim Z == i+1 then return degree Z else return 0;
   ) else (
      Z = trim(lift(Z,ambient target phi) + ideal target phi);
      return getMultidegree(multidegree Z,n);
   );
);

degreeMapInt = method(Options => {MathMode => false, BlowUpStrategy => "Eliminate", Verbose => true});

degreeMapInt (MutableHashTable) := o -> (Phi) -> (
   if Phi#"degree" =!= null then return (if o.MathMode and o.Verbose then <<certificate; Phi#"degree");
   if (class Phi === RationalMap and (not o.MathMode) and isPolynomialRing source Phi) then (
        p := Phi randomLinearSubspace(source Phi,0);   
        hP := hilbertPolynomial(inverseImage(map Phi,p,MathMode=>false),Projective=>false);
        if degree hP > {0} then return 0 else return sub(hP,ZZ);
   );
   if (class Phi === MultihomogeneousRationalMap and (not o.MathMode) and isPolynomialRing source Phi) then (
        z := local z; 
        return degreeMapInt(parametrize(Phi,z),MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);
   );
   if (class Phi === RationalMap and (not o.MathMode) and Phi#"dimTarget" >= 1 and char coefficientRing Phi > 0 and coefficientRing Phi === ZZ/(char coefficientRing Phi)) then (
        q := Phi (point source Phi);   
        F := trim lift(inverseImage(map Phi,q,MathMode=>false),ambient source Phi);
        if dim F -1 > 0 then return 0 else return degree F;
   );
   pr0 := first projectiveDegrees(Phi,NumDegrees=>0,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>false);
   if Phi#"degree" =!= null then (if o.MathMode and o.Verbose then <<certificate; return Phi#"degree");
   if (pr0 == 0 or pr0 == 1) then (
         if o.MathMode then setKeyValue(Phi,"degree",pr0); 
         if o.MathMode and o.Verbose then <<certificate;
         return pr0;
   );
   if isPrime pr0 then (
         f := rationalMap Phi;
         val := if dim image(f,1) - 1 > f#"dimTarget" then 1 else pr0;
         if o.MathMode then setKeyValue(Phi,"degree",val);
         if o.MathMode and o.Verbose then <<certificate;
         return val;
   );
   d := degree (lift(image Phi,ambient target Phi) + ideal target Phi);
   val1 := lift(pr0/d,ZZ);
   if o.MathMode then setKeyValue(Phi,"degree",val1);  
   if o.MathMode and o.Verbose then <<certificate;
   return val1;
);

degreeMap (RationalMap) := o -> (Phi) -> degreeMapInt(Phi,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);

degreeMap (MultihomogeneousRationalMap) := o -> (Phi) -> degreeMapInt(Phi,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);

degreeMap (RingMap) := o -> (phi) -> (
   checkRationalMap phi;
   degreeMap(rationalMapWithoutChecking phi,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose)
);

degree (RationalMap) := (Phi) -> degreeMap(Phi,MathMode=>true,Verbose=>false);

degree (MultihomogeneousRationalMap) := (Phi) -> degreeMap(Phi,MathMode=>true,Verbose=>false);

parametrize (Ideal) := (L) -> (
   K:=coefficientRing ring L; t:=local t; local T;
   if not isField K then error "the coefficient ring needs to be a field";
   L = trim L;
   if dim L -1 < 0 then (T=K[t]/ideal(t); return rationalMap map(T,ring L,toList((numgens ring L):(first gens T))));
   if not (isPolynomialRing ring L and isHomogeneous L) then error "expected homogeneous ideal in a polynomial ring";
   if L == 0 then return rationalMap(ring L);
   if unique degrees L == {{1}} then (
       N:=mingens kernel transpose sub(last coefficients(gens L,Monomials=>gens ring L),K);
       T=K[t_0..t_(numgens source N -1)];
       return rationalMap map(T,ring L,(vars T)*transpose(N));
    );
    if degree L == 2 then (
        p:=point((ring L)/L);
        try(
            f:=rationalMap inverseMap rationalMap(gens p,Dominant=>1);
            f=(parametrize source f)*f;
            if image f != L then error "failed to get parameterization";
            return f;
           );
    );
    error "failed to get a parameterization, you can try using the method parametrize(MultiprojectiveVariety) from the package MultiprojectiveVarieties";
);

parametrize (QuotientRing) := (R) -> (
   if not (isPolynomialRing(ambient R) and isHomogeneous(ideal R)) then error "expected coordinate ring of a projective variety";
   f:=rationalMap(parametrize ideal R,Dominant=>true);
   f * rationalMap(target f,R)
);

parametrize (PolynomialRing) := (R) -> rationalMap R;

parametrizeProductOfProjectiveSpaces = method(TypicalValue => RingMap)
parametrizeProductOfProjectiveSpaces (PolynomialRing,Symbol) := (R,x) -> (
   n := apply(multigens R,g -> #g-1);
   K := coefficientRing R;
   S := K[x_0..x_(sum n)];
   x = gens S;
   v := {toList(1 .. n_0)};
   for i from 1 to #n-1 do v = append(v,toList((1 + last last v) .. (n_i + last last v)));
   map(S,R,flatten apply(v,e -> x_(prepend(0,e))))
);

parametrize (MultihomogeneousRationalMap,Symbol) := (Phi,x) -> (
   if not isPolynomialRing source Phi then error "not implemented yet";
   rationalMap((parametrizeProductOfProjectiveSpaces(source Phi,x)) * (map Phi))
);

parametrize (MultihomogeneousRationalMap) := (Phi) -> parametrize(Phi,getSymbol "x");

flatten (RationalMap) := (Phi) -> (
    Pn := ambient source Phi;
    X := source Phi;
    Pm := ambient target Phi;
    Y := target Phi;  
    f := parametrize ideal image basis(1,ideal X);
    f':= f||X; 
    g := inverse rationalMap(parametrize ideal image basis(1,ideal Y),Dominant=>1);
    g':= rationalMap(g|Y,Dominant=>true); 
    f' * Phi * g'
);

flatten (MultihomogeneousRationalMap) := (Phi) -> (
    g := inverse rationalMap(parametrize ideal image basis(1,ideal target Phi),Dominant=>1);
    g':= rationalMap(g|(target Phi),Dominant=>true); 
    Phi * g'
);

lift (MultihomogeneousRationalMap) := o -> (Phi) -> (
   Psi := rationalMap Phi;
   if isPolynomialRing source Psi then return Psi;
   F := lift(matrix Psi,ambient source Psi) | gens(ideal source Psi);
   try return rationalMap(F) else error "cannot lift given rational map";
);

lift (RationalMap) := o -> (Phi) -> lift Phi~;

lift (RingMap) := o -> (phi) -> (
   checkRationalMap phi;
   map lift rationalMapWithoutChecking phi
);

GraphIdealSat = method(TypicalValue => Ideal);
GraphIdealSat (RingMap) := (phi) -> (
   Pn:=ambient target phi;
   K:=coefficientRing Pn;
   n:=numgens Pn -1;
   X:=ideal target phi;
   B:=ideal toMatrix phi;
   Pm:=ambient source phi;
   m:=numgens Pm - 1;
   Y:=ideal source phi;
   x:=local x; y:=local y;
   degs:=apply(degrees Pn,d -> append(d,0)) | toList((m+1):append(toList(#(heft Pn):0),1));
   R:=K[x_0..x_n,y_0..y_m,Degrees=>degs];
   p1:=map(R,Pn,{x_0..x_n});
   E:=p1 lift(B,Pn);
   Z:=p1(X) + ideal(matrix{{y_0..y_m}} * p1(lift(syz(gens B),Pn)));
   --   Z:=p1(X) + minors(2,(gens E)||matrix{{y_0..y_m}});
   (ii,Tii):=(0,infinity); for i to m do if (B_i != 0 and # terms B_i<Tii) then (ii,Tii)=(i,# terms B_i);
   saturate(Z,ideal(E_ii))
);

GraphIdealElim = method(TypicalValue => Ideal);
GraphIdealElim (RingMap) := (phi) -> (
   -- see also p. 65 in [Computations in algebraic geometry with Macaulay 2 - Editors: D. Eisenbud, D. Grayson, M. Stillman, and B. Sturmfels]
   Pn:=ambient target phi;
   n:=numgens Pn -1;
   m:=numgens ambient source phi -1;
   K:=coefficientRing Pn;
   t:=local t; x:=local x; y:=local y;
   R':=K[t,x_0..x_n,y_0..y_m,MonomialOrder=>Eliminate 1];
   pr:=map(R',Pn,{x_0..x_n});
   F:=flatten entries pr lift(toMatrix phi,Pn);
   J':=pr(ideal target phi) + ideal apply(m+1,j->y_j-t*F_j);
   degs:=apply(degrees Pn,d -> append(d,0)) | toList((m+1):append(toList(#(heft Pn):0),1));
   R:=K[x_0..x_n,y_0..y_m,Degrees=>degs];
   J:=(map(R,R',0|vars R)) ideal selectInSubring(1,gens gb J');
   trim J
);
                     
graphIdealInt = method(Options => {BlowUpStrategy => "Eliminate"})
graphIdealInt (MutableHashTable) := o -> (Phi) -> (
   if o.BlowUpStrategy =!= "Eliminate" and o.BlowUpStrategy =!= "Saturate" then error "expected value for option BlowUpStrategy to be \"Saturate\" or \"Eliminate\"";
   if Phi#"blowUpIdeal" === null then if class Phi === RationalMap then if Phi#"inverseRationalMap" =!= null then if (Phi#"inverseRationalMap")#"blowUpIdeal" =!= null then (
       Bl := (Phi#"inverseRationalMap")#"blowUpIdeal";
       z := reverse multigens ring Bl;
       R := (coefficientRing ring Bl)[flatten z,Degrees => {(#first z):{1,0},(#last z):{0,1}}];
       s := map(R,ring Bl,flatten reverse multigens R);
       Phi#"blowUpIdeal" = s Bl;
   );
   if Phi#"blowUpIdeal" === null then (
       if o.BlowUpStrategy === "Eliminate" then Phi#"blowUpIdeal" = GraphIdealElim map Phi;
       if o.BlowUpStrategy === "Saturate" then Phi#"blowUpIdeal" = GraphIdealSat map Phi;
   );
   return Phi#"blowUpIdeal"; 
);

graphInt = method(Options => {BlowUpStrategy => "Eliminate"})
graphInt (MutableHashTable) := o -> (Phi) -> (
  bl := graphIdealInt(Phi,BlowUpStrategy=>o.BlowUpStrategy);
  Z := (ring bl)/bl;
  gg := multigens Z;
  p2 := rationalMap(map(Z,target Phi,last gg),Dominant=>"notSimplify");
  if Phi#"isDominant" =!= null then setKeyValue(p2,"isDominant",Phi#"isDominant");
  if Phi#"degree" =!= null then setKeyValue(p2,"degree",Phi#"degree");
  if Phi#"isBirational" =!= null then setKeyValue(p2,"isBirational",Phi#"isBirational");
  if #gg == 2 then (
       p1 := rationalMap(map(Z,source Phi,first gg),Dominant=>"notSimplify");
       setKeyValue(p1,"isDominant",true);
       setKeyValue(p1,"degree",1);
       setKeyValue(p1,"isBirational",true);
       return (p1,p2);
  );
  return toSequence append(for i to #gg -2 list rationalMap(gg_i,Dominant=>"notSimplify"),p2);
);

graph (RationalMap) := o -> (Phi) -> graphInt(Phi,BlowUpStrategy=>o.BlowUpStrategy);

graph (MultihomogeneousRationalMap) := o -> (Phi) -> graphInt(Phi,BlowUpStrategy=>o.BlowUpStrategy);

graph (RingMap) := o -> (phi) -> (
  checkRationalMap phi;
  apply(graphInt(rationalMapWithoutChecking phi,BlowUpStrategy=>o.BlowUpStrategy),map)
);

exceptionalLocus (RationalMap) := o -> (Phi) -> (
   B := ideal inverse(Phi,MathMode=>o.MathMode);
   if o.MathMode then Phi^** B else Phi^* B
);

exceptionalLocus (MultihomogeneousRationalMap) := o -> (Phi) -> error "not implemented yet: exceptional locus of a birational map from a multi-projective variety";

changeCoefficientRing = method()
changeCoefficientRing (MutableHashTable,Ring) := (Phi,KK) -> (
   Pn := ambient source Phi;
   Pm := ambient target Phi;
   if not isField KK then error "expected a field";
   if (char Pn =!= char KK and char Pn =!= 0) then error "characteristic not valid";
   I := ideal source Phi;
   J := ideal target Phi;
   F := lift(matrix Phi,Pn);
   Pn' := KK[gens Pn,Degrees=>(degrees Pn)];
   Pm' := KK[gens Pm,Degrees=>(degrees Pm)];
   I' := sub(I,Pn');
   J' := sub(J,Pm');
   F' := sub(F,Pn');
   -- try assert(sub(I',Pn) == I and sub(J',Pm) == J and sub(F',Pn) - F == 0) else error "cannot extend coefficient ring";
   rationalMap(Pn'/I',Pm'/J',F')
);
RationalMap ** Ring := (Phi,KK) -> changeCoefficientRing(Phi,KK);
MultihomogeneousRationalMap ** Ring := (Phi,KK) -> changeCoefficientRing(Phi,KK);

ChowRing := local ChowRing;

genChowRing = method(TypicalValue => RingElement);
genChowRing (ZZ) := (n) -> (
   if class ChowRing_n =!= QuotientRing then (H := local H; ChowRing_n = ZZ[H]/H^(n+1));
   first gens ChowRing_n
);

SegreClass (Ideal) := o -> (I) -> (
   if not isHomogeneous I then error "expected a homogeneous ideal";
   if not ((isPolynomialRing ring I or isQuotientRing ring I) and isPolynomialRing ambient ring I and isHomogeneous ideal ring I) then error("expected ideal in a graded quotient ring or in a polynomial ring");   
   I = trim I;
   degs := unique flatten degrees I;
   phi := if # degs == 1 then toMap I else toMap(I,max degs);
   SegreClass(phi,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose)
);

SegreClass (RingMap) := o -> (phi) -> (
   checkRationalMap phi;
   I:=ideal toMatrix phi;
   d1:=max flatten degrees I;
   r:=dim I -1; n:=dim ring I -1;
   N:=numgens ambient ring I -1;
   d:=projectiveDegrees(phi,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>false);
   h:=genChowRing N;
   if o.MathMode and o.Verbose then <<certificate;
   sum(r+1,k->(-1)^(n-k-1)*sum(n-k+1,i->(-1)^i*binomial(n-k,i)*d1^(n-k-i)*d_i)*h^(N-k))
);

SegreClass (RationalMap) := o -> (Phi) -> SegreClass(map Phi,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);

ChernSchwartzMacPherson (Ideal) := o -> (X) -> ( 
   Pn:=ring X;
   if not (isPolynomialRing Pn and isHomogeneous X) then error "expected homogeneous ideal in a polynomial ring";
   n:=numgens Pn -1;
   H:=genChowRing n;
   csm := (I) -> (
      if numgens I == 1 then (
           g:=projectiveDegrees(map(Pn,Pn,transpose jacobian I),MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>false);
           return (1+H)^(n+1)-sum(n+1,j->g_j*(-H)^j*(1+H)^(n-j));
      );
      I1:=ideal I_0; I2:=ideal submatrix'(gens I,{0});
      csm(I1) + csm(I2) - csm(I1*I2) 
   );
   csmX:=csm trim X;
   if o.MathMode and o.Verbose then <<certificate;
   csmX
);

EulerCharacteristic (Ideal) := o -> (I) -> (
   if not (isPolynomialRing ring I and isHomogeneous I) then error "expected homogeneous ideal in a polynomial ring";
   C := ChernFultonClass(I,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);
   H := first gens ring C;
   coefficient(H^(numgens ring I -1),C)
);

ChernFultonClass = method(TypicalValue => RingElement, Options=>{MathMode => false, BlowUpStrategy => "Eliminate", Verbose => true}); -- non-exported method

ChernFultonClass (Ideal) := o -> (I) -> ( -- p. 11 of [Aluffi, Journal of Symbolic Computation 35 (2003)]
      s := SegreClass(I,MathMode=>o.MathMode,BlowUpStrategy=>o.BlowUpStrategy,Verbose=>o.Verbose);
      s*(1+first gens ring s)^(numgens ring I)
);

expressionVar = method(TypicalValue => String)

expressionVar (ZZ,ZZ) := (Dim,DimAmbient) -> (
   if DimAmbient < 0 then return "empty scheme";
   if Dim < 0 then return ("empty subscheme of PP^"| toString(DimAmbient));
   if Dim === DimAmbient then return ("PP^" | toString(DimAmbient));
-- if Dim === 0 then return ("one-point scheme in PP^"| toString(DimAmbient));
   if Dim === 1 then return ("curve in PP^"| toString(DimAmbient));
   if Dim === 2 then return ("surface in PP^"| toString(DimAmbient));
   if DimAmbient - Dim === 1 then return ("hypersurface in PP^"| toString(DimAmbient));
   if Dim === 3 then return ("threefold in PP^"| toString(DimAmbient));
   return(toString(Dim) | "-dimensional subvariety of "| "PP^" | toString(DimAmbient));
);

expressionVar (ZZ,List) := (Dim,DimAmbient) -> (
   if # DimAmbient <= 0 or min DimAmbient < 0 then return "empty scheme";
   str := "PP^"|toString(DimAmbient_0);
   for i from 1 to #DimAmbient-1 do str = str | " x PP^" | toString(DimAmbient_i);
   if Dim < 0 then return ("empty subscheme of "| str);
   if Dim === sum DimAmbient then return str;
-- if Dim === 0 then return ("one-point scheme in "| str);
   if Dim === 1 then return ("curve in "| str);
   if Dim === 2 then return ("surface in "| str);
   if (sum DimAmbient) - Dim === 1 then return ("hypersurface in "| str);
   if Dim === 3 then return ("threefold in "| str);
   return(toString(Dim) | "-dimensional subvariety of "| str);
);

expressionVar (ZZ,Sequence) := (Dim,DimAmbient) -> (
   if # DimAmbient == 0 then return "empty scheme";
   str := "PP"|(toString DimAmbient);   
   if Dim < 0 then return ("empty subscheme of "| str);
   if Dim === # DimAmbient - 1 then return str;
   if Dim === 1 then return ("curve in "| str);
   if Dim === 2 then return ("surface in "| str);
   if (# DimAmbient - 1) - Dim === 1 then return ("hypersurface in "| str);
   if Dim === 3 then return ("threefold in "| str);
   return(toString(Dim) | "-dimensional subvariety of "| str);
);

expressionVar (Ideal,ZZ,ZZ) := (I,k,n) -> ( -- assume V(I) absolutely irreducible, linearly normal, etc...
  I = trim I;  d:=degree I; degs := flatten degrees I; 
  try assert(isPolynomialRing ring I and isHomogeneous I and k == max(dim I -1,-1) and n == numgens ring I -1) else error "internal error encountered";
  if k < 0 or k >= n then return expressionVar(k,n);
  if k == 0 then (if d == 1 then return("one-point scheme in PP^"|toString(n)) else return("0-dimensional subscheme of degree "|toString(d)|" in PP^"|toString(n)));
  dimSing := if (select(degs,ee->ee>1)=={2} and n<=9) or (max degs<=2 and n<=5) or (numgens I == 1 and d<=8-n and n<=5) then max(dim(minors(n-k,jacobian I,Strategy=>Cofactor)+I)-1,-1) else null; -- for efficiency, the singular locus is calculated only in special cases
  if dimSing === null then if (unique degs == {1}) then dimSing = -1;
  singStr:=if dimSing =!= null and dimSing =!= -1 then "singular " else "";
  cutOut:=""; if #degs>1 then cutOut = if # unique degs == 1 then " cut out by "|toString(#degs)|" hypersurfaces of degree "|toString(first degs) else " cut out by "|toString(#degs)|" hypersurfaces of degrees "|toString(toSequence degs);
  if d == n-k+1 and d > 2 and min degs != 1 then (
      if dimSing === -1 then (
           if d == 4 and k == 2 and n == 5 and unique degs == {2} and #degs == 6 then (
             if isDominant(toMap I,MathMode=>true,Verbose=>false) then return "Veronese surface in PP^5";
           );
           if k==1 then return ("rational normal curve of degree "|toString(d)|" in PP^"|toString(n));
           if k==2 then return ("smooth rational normal scroll surface of degree "|toString(d)|" in PP^"|toString(n));
           if k==d then return ("PP^1 x PP^"|toString(k-1)|" in PP^"|toString(n));
           if k>2 then return "smooth rational normal scroll of dimension "|toString(k)|" and degree "|toString(d)|" in PP^"|toString(n);
      ) else return(singStr|toString(k)|"-dimensional variety of minimal degree in PP^"|toString(n)|cutOut);
  );
  if k == 1 then (
         g:=genus(I);
         if d == 1 and g == 0 then return("line in PP^"|(toString n));
         if d == 2 and g == 0 then if dimSing === -1 then return("irreducible conic curve in PP^"|(toString n)) else return(singStr|"conic curve in PP^"|(toString n));
         if d == 3 then if dimSing === -1 then return("smooth cubic curve of genus "|toString(g)|" in PP^"|(toString n)|cutOut) else return(singStr|"cubic curve of arithmetic genus "|toString(g)|" in PP^"|(toString n)|cutOut);
         if dimSing === -1 then return("smooth curve of degree "|toString(d)|" and genus "|toString(g)|" in PP^"|(toString n)|cutOut) else return(singStr|"curve of degree "|toString(d)|" and arithmetic genus "|toString(g)|" in PP^"|(toString n)|cutOut);
  );
  if k == 2 then (
         if d == 1 then return("plane in PP^"|(toString n));
         if d == 2 then if dimSing === -1 then return("smooth quadric surface in PP^"|(toString n)) else return(singStr|"quadric surface in PP^"|(toString n));
         if d == 3 then if dimSing === -1 then return("smooth cubic surface in PP^"|(toString n)|cutOut) else return(singStr|"cubic surface in PP^"|(toString n)|cutOut);
         if dimSing === -1 then return("smooth surface of degree "|toString(d)|" and sectional genus "|toString((genera I)_1)|" in PP^"|(toString n)|cutOut) else return(singStr|"surface of degree "|toString(d)|" and sectional genus "|toString((genera I)_1)|" in PP^"|(toString n)|cutOut);
  );
  if numgens I == 1 and dimSing =!= null then (
       if d == 1 then return("hyperplane in PP^"|(toString n));
       if d == 2 then if dimSing === -1 then return("smooth quadric hypersurface in PP^"|(toString n)) else return("quadric hypersurface of rank "|toString(n-dimSing)|" in PP^"|(toString n));
       if d == 3 then if dimSing === -1 then return("smooth cubic hypersurface in PP^"|(toString n)) else (if dimSing<k-3 then return("factorial cubic hypersurface in PP^"|(toString n)) else return("singular cubic hypersurface in PP^"|(toString n)));
       if dimSing === -1 then return("smooth hypersurface of degree "|toString(d)|" in PP^"|(toString n)) else (if dimSing<k-3 then return("factorial hypersurface of degree "|toString(d)|" in PP^"|(toString n)) else return("singular hypersurface of degree "|toString(d)|" in PP^"|(toString n)));
  );
  if numgens I == 1 and dimSing === null then (
       return(singStr|"hypersurface of degree "|toString(d)|" in PP^"|(toString n));
  );
  if numgens I == n-k then (
       if unique degs == {1} then return("linear "|toString(k)|"-dimensional subspace of PP^"|(toString n));
       if dimSing === -1 then return("smooth complete intersection of type "|toString(toSequence degs)|" in PP^"|(toString n));
       if dimSing =!= null then if dimSing<k-3 then return("factorial complete intersection of type "|toString(toSequence degs)|" in PP^"|(toString n)); 
       return(singStr|"complete intersection of type "|toString(toSequence degs)|" in PP^"|(toString n));
  );
  if dimSing === -1 then return("smooth "|toString(k)|"-dimensional variety of degree "|toString(d)|" in PP^"|(toString n)|cutOut) else return(singStr|toString(k)|"-dimensional variety of degree "|toString(d)|" in PP^"|(toString n)|cutOut);
);

expressionVar (Ideal,ZZ,List) := (I,k,n) -> ( 
  I = trim I;  degs := degrees I; 
  try assert(isPolynomialRing ring I and isHomogeneous I and k == max(dim I - (#n),-1) and (sum n) + (#n) == numgens ring I) else error "internal error encountered";
  if k <= 0 or k >= sum n then return expressionVar(k,n);
  if # degs == 1 then return(expressionVar(k,n)|" defined by a multiform of degree "|toString(first degs));
  cutOut:=""; if #degs>1 then cutOut = if # unique degs == 1 then " cut out by "|toString(#degs)|" hypersurfaces of degree "|toString(first degs) else " cut out by "|toString(#degs)|" hypersurfaces of degrees "|toString(toSequence degs); 
  return(expressionVar(k,n)|cutOut);
);

expressionVar (Ideal,ZZ,Sequence) := (I,k,n) -> ( 
  I = trim I;  degs := degrees I; 
  try assert(isPolynomialRing ring I and isHomogeneous I and k == max(dim I - 1,-1) and #n == numgens ring I) else error "internal error encountered";
  if k <= 0 or k >= #n-1 then return expressionVar(k,n);
  if # degs == 1 then return(expressionVar(k,n)|" defined by a form of degree "|toString(first degs));
  cutOut:=""; if #degs>1 then cutOut = if # unique degs == 1 then " cut out by "|toString(#degs)|" hypersurfaces of degree "|toString(first degs) else " cut out by "|toString(#degs)|" hypersurfaces of degrees "|toString(toSequence degs); 
  return(expressionVar(k,n)|cutOut);
);

expressionVar (Ideal) := (I) -> (
   if degreeLength ring I == 1 then (if max flatten degrees ring I >= 2 then return expressionVar(I,dim I - 1,toSequence flatten degrees ring I) else return expressionVar(I,dim I - 1,numgens ring I - 1));
   k := max(dim I - (# multigens ring I),-1);
   n := apply(multigens ring I,g->(#g -1));
   expressionVar(I,k,n)
);

? Ideal := (I) -> (if isPolynomialRing ring I then expressionVar I else expressionVar trim lift(I,ambient ring I)); -- for testing only

setKeyValue = method(TypicalValue => Nothing)
setKeyValue (MutableHashTable,String,Thing) := (Phi,str,val) -> (
--  <<("--setting key \""|str|"\", for "|toString(net Phi)|", real modification: "|toString(Phi#str =!= val)|newline);
    errorClass := () -> error("tried to set a wrong value on the key \""|str|"\"");
    errorChange := () -> error("tried to change the value for \""|str|"\" from "|toString(Phi#str)|" to "|toString(val));
    if str === "map" then (
         if class val =!= RingMap then errorClass();
         if target val =!= target Phi#"map" then errorChange();
         if ambient source val =!= ambient source Phi#"map" then errorChange();
         if source val =!= source Phi#"map" then (
             if not (isPolynomialRing source Phi#"map") then errorChange();
             if Phi#"isDominant" === false then Phi#"isDominant" = null;
             if Phi#"isBirational" === false then Phi#"isBirational" = null;
             Phi#"dimSource" = max(dim source val -1,-1);
             if Phi#"idealImage" =!= null then Phi#"idealImage" = trim sub(Phi#"idealImage",source val);     
             if Phi#"maps" =!= null then Phi#"maps" = apply(Phi#"maps",psi -> map(target Phi#"map",source val,toMatrix psi));
         );
         Phi#str = val;
         return;
    );
    if str === "maps" then (
         if class val =!= List then errorClass();
         for f in val do if not (target f === target Phi#"map" and source f === source Phi#"map") then errorClass();
         Phi#str = val;
         return;
    );
    if str === "isDominant" then (
         if class val =!= Boolean then errorClass();
         if Phi#str === null then (Phi#str = val) else (if Phi#str =!= val then errorChange());
         if val === true then if Phi#"degree" === 1 then if Phi#"isBirational" =!= true then setKeyValue(Phi,"isBirational",true);
         if val === true then if Phi#"idealImage" === null then setKeyValue(Phi,"idealImage",trim ideal(0_(source Phi#"map")));
         if val === false then if Phi#"isBirational" =!= false then setKeyValue(Phi,"isBirational",false);
         return;
    );
    if str === "idealImage" then (
         if class val =!= Ideal then errorClass();
         if ring val =!= source Phi#"map" then errorClass();
         if not isHomogeneous val then error "tried to set a nonhomogeneous ideal as image";
         Phi#str = val;
         if Phi#"isDominant" === null then setKeyValue(Phi,"isDominant",val == 0);
         return;
    );
    if str === "isBirational" then (
         if class val =!= Boolean then errorClass();
         if Phi#str === null then (Phi#str = val) else (if Phi#str =!= val then errorChange());
         if val === true then (if Phi#"degree" =!= 1 then setKeyValue(Phi,"degree",1); if Phi#"isDominant" =!= true then setKeyValue(Phi,"isDominant",true));
         return;
    );
    if str === "inverseRationalMap" then (
         if class val =!= RationalMap then errorClass();
         if Phi#str === null then Phi#str = val else errorChange();
         return;
    );
    if str === "projectiveDegrees" then (
         if class val =!= List then errorClass();
         if # val =!= 1 + Phi#"dimTarget" then errorClass();
         if Phi#str === {} then (Phi#str = val) else (if Phi#str =!= val then errorChange());
         if class Phi =!= RationalMap then return;
         if Phi#"dimTarget" === Phi#"dimAmbientTarget" then if Phi#"dimSource" === Phi#"dimAmbientSource" then if (Phi#"dimTarget" === Phi#"dimSource" and Phi#"dimTarget" > 0) then (if Phi#"isDominant" =!= (last val > 0) then setKeyValue(Phi,"isDominant",last val > 0); if Phi#"degree" =!= (last val) then setKeyValue(Phi,"degree",last val));
         if Phi#"inverseRationalMap" =!= null then if (Phi#"inverseRationalMap")#"projectiveDegrees" =!= (reverse val) then setKeyValue(Phi#"inverseRationalMap","projectiveDegrees",reverse val);
         return;
    );    
    if str === "degree" then (
         if class val =!= ZZ then errorClass();
         if Phi#str === null then (Phi#str = val) else (if Phi#str =!= val then errorChange());
         if val === 1 then if Phi#"isDominant" === true then if Phi#"isBirational" =!= true then setKeyValue(Phi,"isBirational",true);
         return;
    );
    error("key not found");
);

toMatrix = (phi) -> ( -- phi RingMap
   submatrix(matrix phi,{0..(numgens source phi -1)})
);

random1 = (R) -> (
   K:=coefficientRing R;
   if class K =!= FractionField then random(1,R) else sum for s to numgens R -1 list (sum for b to abs random(ZZ) list random(b,ring numerator 1_K)) * (gens R)_s
);

randomLinearSubspace = (R,i) -> (
   -- input: polynomial ring R, integer i
   -- output: ideal of a random i-dimensional linear subspace of Proj(R)
   n:=numgens R -1;
   if i == n then return ideal R;
   if i <=-1 then return ideal 1_R;
   L:=trim ideal for j to n-1-i list random1(R);
   -- return if dim L - 1 == i then L else randomLinearSubspace(R,i);
   L
);

genericRestriction = (phi) -> (
   -- restriction of a rational map X \subset P^n ---> Y \subset P^m to a general hyperplane section of X 
   Pn:=ambient target phi;
   n:=numgens Pn -1;
   K:=coefficientRing Pn;
   x:=local x;
   H:=K[x_0..x_(n-1)];
   j:=map(H,Pn,random(toList(x_0..x_(n-1))|{random1 H}));
   j=map(H/j(ideal target phi),target phi,toMatrix j);
   phi':=j*phi;
   phi'
);

multigens = method()
multigens (PolynomialRing) := (R) -> (
   gR := gens R;
   apply(entries diagonalMatrix toList((# heft R):1), d -> select(gR, u -> degree u == d))
);
multigens (QuotientRing) := (R) -> (
   apply(multigens ambient R,U -> flatten entries sub(matrix{U},R))
);

multisaturate = method();
multisaturate Ideal := (cacheValue "Multisaturation") (I -> (
   if I.cache#?"isMultisaturated" and I.cache#"isMultisaturated" then return I;
   m := multigens ring I;
   for i to #m -1 do I = saturate(I,ideal(m_i));
   I.cache#"isMultisaturated" = true;
   return I;
));

getMultidegree = method()

getMultidegree (RingElement,List,ZZ,ZZ) := (mdeg,n,m,r) -> (
   -- input: mdeg: multidegree of a subvariety of P^(n_1) x ... x P^(n_k) x P^m
   --        n: {n_1,...,n_k}
   --        r == (sum n) + m - (first degree mdeg) -- the dimension of the subvariety
   -- output: multidegree of the same variety as embedded in Seg(P^(n_1) x ... x P^(n_k)) x P^m
   k := #n;
   N := product apply(k,i -> n_i+1) -1; 
   T1 := (gens ring mdeg)_{(0 .. k-1)}; T2 := last gens ring mdeg;
   mon := (product apply(k,i -> T1_i^(n_i))) * T2^m;
   -- T := local T; R := ZZ[T_0,T_1];
   d := local d;
   for i from 0 to max(0,r-m) -1 do d_i = 0;
   for i from max(0,r-m) to min(r,N) do d_i = coefficient(mon,mdeg * (sum T1)^i * T2^(r-i));
   for i from min(r,N) + 1 to r do d_i = 0;
   -- mdeg' := sum for i from max(0,r-m) to min(r,N) list d_i * T_0^(N-i) * T_1^(m-r+i); <<mdeg'<<endl;
   reverse for i to r list d_i
);

getMultidegree (RingElement,ZZ,ZZ,ZZ) := (mdeg,n,m,r) -> getMultidegree(mdeg,{n},m,r);

getMultidegree (RingElement,List) := (mdeg,n) -> first getMultidegree(mdeg,n,0,(sum n) - (first degree mdeg));

point (Ideal,Boolean) := (I,b) -> (  -- see also: code(randomKRationalPoint,Ideal)
   R := ring I;
   if not (isPolynomialRing R and isHomogeneous I) then error "expected a homogeneous ideal in a polynomial ring";
   if degrees R =!= toList((numgens R):{1}) then error "expected a standard graded ring, but you may use the method point(MultiprojectiveVariety) from the package MultiprojectiveVarieties";
   c := codim I; 
   n := numgens R -1;
   if c >= n then error "expected a positive dimensional scheme";
   local p;
   if c == 0 then (p = randomLinearSubspace(R,0); if (not b) or dim p == 1 then return p else error "failed to find rational points");
   if char R == 0 then error "expected a finite ground field";
   local par;
   if c == 1 then (
       L := {}; 
       maxAttempts := 20; attempt := 0;
       while #L == 0 and attempt < maxAttempts do (attempt = attempt+1; par = parametrize randomLinearSubspace(R,1); L = select(decompose par^* I,q -> dim q == 1 and degree q == 1));
       if #L == 0 and attempt >= maxAttempts then error("reached maximum number of "|toString(maxAttempts)|" attempts to find point");
       p = par first L;
   ); 
   if c > 1 and n - c >= 2 then (
       par = parametrize randomLinearSubspace(R,c+1);
       p = par point(par^* I,false);
   );
   if c > 1 and n - c < 2 then (
       f := (rationalMap gens randomLinearSubspace(R,c-2))|I;
       I' := kernel(map f,SubringLimit=>1);
       p = trim lift(f^*(point(I',false)),ambient source f);
   );
   if b then (if not (unique degrees p == {{1}} and dim p == 1 and degree p == 1 and isSubset(I,p)) then error "failed to find rational points");
   return p;
);

point (Ideal) := (I) -> point(I,true);

point (PolynomialRing) := (R) -> point ideal R;

point (QuotientRing) := (R) -> (
   if not (isPolynomialRing ambient R and isHomogeneous ideal R) then error "expected coordinate ring of a projective variety";
   sub(point ideal R,R)
);

segre (MultihomogeneousRationalMap) := (Phi) -> segre source Phi;

segre (RationalMap) := (Phi) -> segre source Phi;

segre (PolynomialRing) := (R) -> rationalMap(gens product apply(multigens R,ideal));

segre (QuotientRing) := (R) -> (
   phi := rationalMap(gens product apply(multigens R,ideal),Dominant=>1);
   phi * (rationalMap inverseMap parametrize target phi)
);

segre (Ideal,Ideal) := (I,J) -> ( -- undocumented
   -- returns the map V(I) x V(J) ---> P^N
   if not (isPolynomialRing ring I and isPolynomialRing ring J) then error "expected ideals in polynomial rings";
   if not (isHomogeneous I and isHomogeneous J) then error "expected homogeneous ideals";
   K := coefficientRing ring I;
   if K =!= coefficientRing ring J then error "common coefficient ring not found";
   a := local a; b := local b;
   n := numgens ring I -1; m := numgens ring J -1;
   R := K[a_0..a_n]; S := K[b_0..b_m];
   T := R**S;
   E := sub(sub(I,vars R),T) + sub(sub(J,vars S),T);
   segre(T/E)
);

checkRationalMap0 = (phi) -> ( -- phi RingMap
   if coefficientRing target phi =!= coefficientRing source phi then error "different coefficient rings in source and target are not permitted";
   if not isField coefficientRing target phi then error("the coefficient ring needs to be a field");
   if not ((isPolynomialRing source phi or isQuotientRing source phi) and (isPolynomialRing target phi or isQuotientRing target phi) and isPolynomialRing ambient source phi and isPolynomialRing ambient target phi and isHomogeneous ideal source phi and isHomogeneous ideal target phi) then error("source and target of the ring map need to be quotients of polynomial rings by homogeneous ideals");
   if not (isHomogeneous ideal toMatrix phi) then error("the map needs to be defined by homogeneous polynomials of the same degree");
   D:=degrees ideal compress toMatrix phi; if #D != 0 then if not (min D == max D) then error("the map needs to be defined by homogeneous polynomials of the same degree");
);

checkRationalMap = (phi) -> ( -- phi RingMap
   if not (degrees ambient source phi == toList((numgens ambient source phi):{1})) then error "expected standard grading on source ring map";
   if not (degrees ambient target phi == toList((numgens ambient target phi):{1})) then error "expected standard grading on target ring map";
   checkRationalMap0 phi;
);

checkMultihomogeneousRationalMap = (phi) -> ( -- phi RingMap
   if not (degrees ambient source phi == toList((numgens ambient source phi):{1})) then error "expected standard grading on source ring map";
   if not ((flatten multigens ambient target phi == gens ambient target phi) or (degreeLength ambient target phi == 1 and min flatten degrees ambient target phi >= 1)) then error ("given grading on target ring map is not permitted");
   checkRationalMap0 phi;
);

checkLinearSystem0 = (F) -> ( -- F row matrix
   if not isField coefficientRing ring F then error("the coefficient ring needs to be a field");
   if not ((isPolynomialRing ring F or isQuotientRing ring F) and isPolynomialRing ambient ring F and isHomogeneous ideal ring F) then error("the base ring must be a quotient of a polynomial ring by a homogeneous ideal");
   if not (numgens target F == 1) then error("expected a row matrix");
   if numgens source F == 0 then return;
   if not (isHomogeneous ideal F) then error("expected homogeneous elements of the same degree");
   D:=degrees ideal compress F; if #D != 0 then if not (min D == max D) then error("expected homogeneous elements of the same degree");
);

---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
----------------------------- Examples ------------------------------------------------
---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------

load "./Cremona/examples.m2"

specialCremonaTransformation (Ring,ZZ) := (K,a) -> (
   if not isField K then error "expected a field";
   if a<1 or a>12 then error("expected integer between 1 and 12, see Table 1 of "|"https://arxiv.org/abs/1509.06028");
   (F,degs,str) := Examples(K,a);
   phi := map(ring F,ring F,F);
   new RationalMap from {
        "map" => phi,
        "maps" => {phi},
        "isDominant" => true,
        "idealImage" => trim ideal(0_(ring F)),
        "isBirational" => true,
        "inverseRationalMap" => null,
        "projectiveDegrees" => degs,
        "degree" => 1,
        "dimAmbientTarget" => numgens ring F -1,
        "dimTarget" => numgens ring F -1,
        "dimAmbientSource" => numgens ring F -1,
        "dimSource" => numgens ring F -1,
        "blowUpIdeal" => null
   }
);

specialCremonaTransformation (ZZ) := (j) -> specialCremonaTransformation(QQ,j);

specialCremonaTransformation (ZZ,Ring) := (j,K) -> specialCremonaTransformation(K,j);
   
quadroQuadricCremonaTransformation (Ring,ZZ,ZZ) := (K,n,i) -> (
   if not isField K then error "expected a field";
   if n <= 0 or i <= 0 then error "expected positive integers";
   if position({(2,1),(2,2),(2,3),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(4,8),(4,9),(4,10),(4,11),(4,12),(4,13),(4,14),(4,15),(4,16),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),(5,8),(5,9),(5,10),(5,11),(5,12),(5,13),(5,14),(5,15),(5,16),(5,17),(5,18),(5,19),(5,20),(5,21),(5,22),(5,23),(5,24),(5,25),(5,26),(5,27),(5,28),(5,29),(5,30),(5,31),(5,32),(5,33),(5,34),(5,35),(5,36),(5,37),(5,38),(5,39),(8,1),(11,1),(14,1),(20,1),(26,1)},pa -> pa == (n,i)) === null then error("quadro-quadric Cremona transformation of P^"|toString(n)|" not available");
   F := quadroquadric(K,n,i);
   Phi := rationalMap(ring F,ring F,F);
   setKeyValue(Phi,"maps",{map Phi});
   setKeyValue(Phi,"isDominant",true);
   setKeyValue(Phi,"isBirational",true);
   setKeyValue(Phi,"degree",1);
   if n <= 5 then degrees Phi;
   return Phi;
);

quadroQuadricCremonaTransformation (ZZ,ZZ) := (n,i) -> quadroQuadricCremonaTransformation(QQ,n,i);

quadroQuadricCremonaTransformation (ZZ,ZZ,Ring) := (n,i,K) -> quadroQuadricCremonaTransformation(K,n,i);

specialQuadraticTransformation (Ring,ZZ) := (K,a) -> (
   if not isField K then error "expected a field";
   if a<1 or a>11 then error("expected integer between 1 and 11, see Table 1 of "|"https://arxiv.org/abs/1411.1227");
   if a == 2 then return specialCremonaTransformation(K,9);
   if a == 3 then return specialCremonaTransformation(K,10);
   (F,Z,degs,str) := examplesQuadratic(K,a);
   Phi := rationalMap(ring F,Z,F);
   setKeyValue(Phi,"maps",{map Phi});
   setKeyValue(Phi,"isBirational",true);
   setKeyValue(Phi,"projectiveDegrees",degs);
   setKeyValue(Phi,"degree",1);
   return Phi;
);

specialQuadraticTransformation (ZZ) := (j) -> specialQuadraticTransformation(QQ,j);

specialQuadraticTransformation (ZZ,Ring) := (j,K) -> specialQuadraticTransformation(K,j);

specialCubicTransformation (Ring,ZZ) := (K,a) -> (
   if not isField K then error "expected a field";
   if a<1 or a>9 then error("expected integer between 1 and 9, see Table 2 of "|"https://arxiv.org/abs/1901.01203");
   if a == 1 then return specialCremonaTransformation(K,1);
   if a == 2 then (
       cre3 := specialCremonaTransformation(K,3);
       x := gens source cre3;
       return (rationalMap((parametrize(ideal(x_0-x_1+x_2))) * cre3,Dominant=>2))!;
   );
   if a == 3 then return specialCremonaTransformation(K,3);
   if a == 4 then return specialCremonaTransformation(K,11);
   if a == 5 then return specialCremonaTransformation(K,12);
   (F,Z,degs,str) := examplesCubic(K,a-5);
   Phi := rationalMap map(ring F,Z,F);
   setKeyValue(Phi,"maps",{map Phi});
   setKeyValue(Phi,"isBirational",true);
   setKeyValue(Phi,"projectiveDegrees",degs);
   setKeyValue(Phi,"degree",1);
   return Phi;
);

specialCubicTransformation (ZZ) := (j) -> specialCubicTransformation(QQ,j);

specialCubicTransformation (ZZ,Ring) := (j,K) -> specialCubicTransformation(K,j);

---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
---------------------------- end Examples ---------------------------------------------
---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------

AbstractRationalMap = new Type of MutableHashTable;
VerbAbsRatMap = false;

abstractRationalMap (PolynomialRing,PolynomialRing,FunctionClosure,ZZ) := (Pn,Pm,f,d) -> ( 
   K := coefficientRing Pn;
   if K =!= coefficientRing Pm then error "different coefficient rings in source and target are not permitted";
   if not isField K then error "the coefficient ring needs to be a field";
   if numgens Pn <= 1 or numgens Pm <= 1 then error "expected polynomial rings with at least 2 variables";
   if not (degrees Pn == toList((numgens Pn):{1}) and degrees Pm == toList((numgens Pm):{1})) then error "expected standard grading";
   try (
         p := for i to numgens Pn -1 list random K;
         q := f p;
         assert(class q === List and #q == numgens Pm and ring matrix{q} === K);
   ) else error("unable to interpret the input FunctionClosure as a rational map from PP^"|toString(numgens Pn -1)|" to PP^"|toString(numgens Pm -1)|" over "|toString(K)|"; expected a function that takes a list of "|toString(numgens Pn)|" elements of "|toString(K)|" and returns a list of "|toString(numgens Pm)|" elements of "|toString(K)|", e.g.: x -> "|toString((for i to min(numgens Pn,numgens Pm)-1 list "x_"|toString(i)|"^2")|toList(numgens(Pm)-min(numgens Pn,numgens Pm):0)));
   new AbstractRationalMap from {
        "source" => Pn,
        "target" => Pm,
        "function" => f,
        "verbose" => VerbAbsRatMap,
        "degForms" => null,
        "rationalMap" => null,
        "hintDegForms" => d
   }
);

abstractRationalMap (PolynomialRing,PolynomialRing,FunctionClosure) := (Pn,Pm,f) -> abstractRationalMap(Pn,Pm,f,1);

abstractRationalMap (RationalMap) := (Phi) -> (
   if not (isPolynomialRing source Phi and isPolynomialRing target Phi) then error "expected a rational map between projective spaces";
   F := matrix Phi;
   x := gens source Phi;
   f := p -> flatten entries sub(F,apply(#x,i -> x_i => p_i));
   Psi := abstractRationalMap(source Phi,target Phi,f);
   Psi#"rationalMap" = Phi;
   Psi#"degForms" = projectiveDegrees(Phi,Phi#"dimTarget" - 1);
   Psi#"hintDegForms" = Psi#"degForms";
   Psi
);

expression AbstractRationalMap := (Phi) -> expression("rational map from PP^"|toString(numgens source Phi -1)|" to PP^"|toString(numgens target Phi -1));

net AbstractRationalMap := (Phi) -> (
   s := "-- rational map --"||("source: "|nicePrint(source Phi))||("target: "|nicePrint(target Phi));
   if Phi#"degForms" =!= null then s=s||"defining forms: given by a function (degree = "|toString(Phi#"degForms")|")" else s=s||"defining forms: given by a function"; 
   net(s)
);
texMath AbstractRationalMap := texMath @@ net;

AbstractRationalMap#{WebApp,AfterPrint} = AbstractRationalMap#{WebApp,AfterNoPrint} = 
AbstractRationalMap#{Standard,AfterPrint} = AbstractRationalMap#{Standard,AfterNoPrint} = (Phi) -> (
  << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << class Phi << " (" << expression Phi << ")" << endl;
);

source AbstractRationalMap := (Phi) -> Phi#"source";

target AbstractRationalMap := (Phi) -> Phi#"target";

coefficientRing AbstractRationalMap := (Phi) -> coefficientRing source Phi;

compose (AbstractRationalMap,AbstractRationalMap) := (Phi,Psi) -> (
   if target Phi =!= source Psi then error "rational maps not composable: incompatible target and source";
   new AbstractRationalMap from {
        "source" => source Phi,
        "target" => target Psi,
        "function" => (Psi#"function") @@ (Phi#"function"),
        "verbose" => Phi#"verbose" or Psi#"verbose",
        "degForms" => null,
        "rationalMap" => null,
        "hintDegForms" => (if Phi#"degForms" =!= null then Phi#"degForms" else Phi#"hintDegForms") * (if Psi#"degForms" =!= null then Psi#"degForms" else Psi#"hintDegForms")
   }
);

AbstractRationalMap * AbstractRationalMap := (Phi,Psi) -> compose(Phi,Psi); 

rationalMap (AbstractRationalMap) := o -> (Phi) -> (
   d := degForms Phi;
   if Phi#"rationalMap" =!= null then return Phi#"rationalMap";
   Pn := source Phi;
   Pm := target Phi;
   f := Phi#"function";
   K := coefficientRing Phi;
   n := numgens Pn -1;
   m := numgens Pm -1;
   err := 10;
   N := ceiling((m+1) * binomial(n+d,d) / m) + err;
   if Phi#"verbose" then <<"-- picking "<<N<<" random points on PP^"<<n<<endl;
   B := matrix apply(N,i -> prepend(1_K,flatten entries random(K^1,K^n)));
   if Phi#"verbose" then <<"-- calculating the images of the "<<N<<" points in PP^"<<m<<endl;
   try (
      V := matrix apply(entries B,b -> f b);
      assert(m+1 == numColumns V);
      assert(ring V === K);
   ) else error "something went wrong while applying the FunctionClosure";
   a := local a;
   R := K[flatten for i to m list toList(a_(i,0)..a_(i,binomial(n+d,d)-1))];
   x := local x;
   PP := R[x_0..x_n];
   F := matrix{for i to m list (matrix{toList(a_(i,0)..a_(i,binomial(n+d,d)-1))} * transpose gens (ideal vars PP)^d)_(0,0)};
   M := apply(N,i -> sub(F,apply(n+1,j -> x_j => B_(i,j))) || submatrix(V,{i},));
   eqs := sum(M,m -> trim minors(2,m));
   if Phi#"verbose" then <<"-- obtained "<<numgens eqs<<" linear equations with "<<toString((m+1) * binomial(n+d,d))<<" unknowns over "<<toString(K)<<endl;
   eqs = trim eqs;
   if Phi#"verbose" then <<"-- number of independent equations: "<<numgens eqs<<endl;
   if numgens eqs >= (m+1) * binomial(n+d,d) then error "interpolation failed: too many independent equations";
   W := transpose sub(last coefficients(gens eqs,Monomials=>vars R),K);
   S := entries transpose mingens kernel W;
   if Phi#"verbose" then <<"-- number of independent solutions: "<<#S<<endl;
   if #S == 0 then error "something went wrong while calculating forms";
   S0 := first S;
   g := gens R;
   PP = K[x_0..x_n];
   G := sub(sub(sub(F,apply(#g,i -> g_i => S0_i)),PP),vars Pn);
   phi := rationalMap(Pn,Pm,G);
   if #S > 1 then maps phi;
   Phi#"degForms" = max flatten degrees ideal compress matrix phi;
   Phi#"rationalMap" = phi
);

degForms = method()

degForms (AbstractRationalMap) := (Phi) -> (
   if Phi#"degForms" =!= null then return Phi#"degForms";
   Phi' := if numgens source Phi == 2 
           then new AbstractRationalMap from {
                  "source" => Phi#"source",
                  "target" => Phi#"target",
                  "function" => Phi#"function",
                  "verbose" => Phi#"verbose",
                  "degForms" => Phi#"degForms",
                  "rationalMap" => Phi#"rationalMap",
                  "hintDegForms" => Phi#"hintDegForms"}
           else (abstractRationalMap parametrize randomLinearSubspace(source Phi,1)) * Phi;
   assert(numgens source Phi' == 2);
   MAXd := infinity;
   gap := 2;
   d := (Phi#"hintDegForms") - gap;   
   c := true;
   local psi;
   while c and d <= MAXd do (
      d = d + gap;
      if Phi#"verbose" then <<"-- searching degree of forms: trying "<<d<<endl; 
      try (
         Phi'#"degForms" = d;
         psi = rationalMap Phi';
         c = false;
      );
   );
   if c then error "unable to find degree of defining forms";
   if numgens source Phi == 2 then Phi#"rationalMap" = psi;
   Phi#"degForms" = max flatten degrees ideal compress matrix psi
);

projectiveDegrees (AbstractRationalMap,ZZ) := o -> (Phi,i) -> (
   if o.MathMode then error "the option MathMode is not available for projectiveDegrees(AbstractRationalMap,ZZ)";
   n := numgens source Phi -1;
   if i < 0 or i > n then error("expected integer between 0 and "|toString(n)); 
   if i == n then return 1;
   if i == n-1 then return degForms Phi;
   if Phi#"rationalMap" =!= null then return projectiveDegrees(Phi#"rationalMap",i);
   error "not implemented yet: i-th projective degree of an abstract rational map from PP^n with i < n-1";
);

inverseMap (AbstractRationalMap) := o -> (Phi) -> (
   if o.MathMode then error "the option MathMode is not available with inverseMap(AbstractRationalMap)";
   phi := rationalMap Phi;
   if not isBirational phi then error "expected a birational map";
   if phi#"inverseRationalMap" =!= null then return abstractRationalMap(phi#"inverseRationalMap");
   f := a -> flatten entries coefficients parametrize(phi^* trim minors(2,(vars target phi)||matrix{a}));
   abstractRationalMap(target Phi,source Phi,f,if phi#"projectiveDegrees" =!= {} then projectiveDegrees(phi,1) else 1)
);

AbstractRationalMap SPACE List := (Phi,q) -> (
   try assert(#q == numgens source Phi and (ring matrix{q} === coefficientRing Phi or ring matrix{q} === ZZ)) else error("expected a coordinate point on Proj("|toString(source Phi)|")");
   (Phi#"function") q
);

AbstractRationalMap SPACE Sequence := (Phi,q) -> (
   toSequence (Phi#"function") toList q
);

harmonicallyConjugate = method()
harmonicallyConjugate (RingElement,RingElement) := (Q,L) -> (
   -- input: Q,L a quadratic and a linear form on P^1
   -- output: L', s.t. L*L' is harmonically conjugate to Q
   if not (ring Q === ring L) then error "expected same ring";
   if not isPolynomialRing ring Q then error "expected a polynomial ring";
   if not (degree Q === {2} and degree L === {1} and numgens ring Q === 2) then error "expected a quadratic and a linear form on P^1";
   K := coefficientRing ring Q;
   t := gens ring Q;
   q := flatten entries sub(last coefficients(Q,Monomials=>gens (ideal t)^2),K);
   (alpha,beta,gamma) := (q_0,q_1/2,q_2);
   l := flatten entries sub(last coefficients(L,Monomials=>gens ideal t),K);
   p := (l_1,-l_0);
   (gamma*p_1+beta*p_0)*t_1+(beta*p_1+alpha*p_0)*t_0
);

secantCone = method()
secantCone (List,Ideal) := (x,I) -> (
   try assert(#x == numgens ring I and matrix{x} != 0) else error("expected coordinate list of a point of PP^"|toString(numgens ring I -1));
   PN := ring I;
   N := numgens PN -1;
   K := coefficientRing PN;
   s := local s; t := local t; u := local u; v := local v; 
   R := K[u,v,s_0..s_N,t_0..t_N,MonomialOrder=>Eliminate (N+3)];
   S := (map(R,PN,{s_0..s_N})) I;
   T := (map(R,PN,{t_0..t_N})) I;
   Inc := ideal(sub(matrix{x},R) - u *  matrix{{s_0..s_N}} - v *  matrix{{t_0..t_N}}) + S + T;
   z := local z;
   R = K[u,v,t_0..t_N,z_0..z_N,MonomialOrder=>Eliminate (N+3)];
   Inc = sub(ideal selectInSubring(1,gens gb Inc),R) + ideal(u * sub(matrix{x},R) + v * matrix{{t_0..t_N}} - matrix{{z_0..z_N}});
   R = K[z_0..z_N];
   J := sub(ideal selectInSubring(1,gens gb Inc),R);
   trim (map(PN,R,vars PN)) J
);

abstractRationalMap (Ideal,String) := (X,str) -> (
   if str =!= "OADP" then error("currently the second argument must be the string: \"OADP\"");
   if not isPolynomialRing ring X then error "expected ideal in a polynomial ring";
   if not isHomogeneous X then error "expected a homogeneous ideal";
   if not isField coefficientRing ring X then error "the coefficient ring needs to be a field";
   f := p -> (
         L := secantCone(p,X);
         f := parametrize L;
         assert(numgens source f == 2);
         ab := f^*(L+X);
         x := f^*(trim minors(2,(vars ring X)||matrix{p}));
         Tx := f ideal harmonicallyConjugate(ab_0,x_0);
         flatten entries coefficients parametrize Tx
       ); 
   abstractRationalMap(ring X,ring X,f)
);

sub (RationalMap,PolynomialRing,PolynomialRing) := (Phi,Pn,Pm) -> (
   if not(coefficientRing Pn === coefficientRing Phi and coefficientRing Pm === coefficientRing Phi) then error "expected same coefficient ring";
   if numgens Pn != Phi#"dimAmbientTarget"+1 then error ("the ambient projective space of the source has dimension "|toString(Phi#"dimAmbientTarget"));
   if numgens Pm != Phi#"dimAmbientSource"+1 then error ("the ambient projective space of the target has dimension "|toString(Phi#"dimAmbientSource"));
   I := sub(ideal source Phi,vars Pn);
   J := sub(ideal target Phi,vars Pm);
   R := if numgens I > 0 then Pn/I else Pn;
   S := if numgens J > 0 then Pm/J else Pm;
   F := flatten entries sub(lift(matrix Phi,ambient source Phi),vars Pn);
   mF := if Phi#"maps" =!= null then (for i to #(Phi#"maps")-1 list flatten entries sub(lift(matrix (Phi#"maps")_i,ambient source Phi),vars Pn)) else null;
   Z := if Phi#"idealImage" =!= null then sub(ideal sub(lift(gens Phi#"idealImage",ambient target Phi),vars Pm),S) else null;
   Bl := if Phi#"blowUpIdeal" =!= null then sub(Phi#"blowUpIdeal",vars tensor(Pn,Pm,MonomialOrder=>{GRevLex,Position=>Up})) else null;
   phi := new RationalMap from {
           "map" => map(R,S,F),
           "maps" => if mF =!= null then for i to #mF-1 list map(R,S,mF_i) else null,
           "isDominant" => Phi#"isDominant",
           "idealImage" => null,
           "isBirational" => Phi#"isBirational",
           "inverseRationalMap" => null,
           "projectiveDegrees" => Phi#"projectiveDegrees",
           "degree" => Phi#"degree",
           "dimAmbientTarget" => Phi#"dimAmbientTarget",
           "dimTarget" => Phi#"dimTarget",
           "dimAmbientSource" => Phi#"dimAmbientSource",
           "dimSource" => Phi#"dimSource",
           "blowUpIdeal" => Bl
         };
   if Z =!= null then forceImage(phi,Z);
   if Phi#"inverseRationalMap" =!= null then (
       Psi := Phi#"inverseRationalMap";
       G := flatten entries sub(lift(matrix Psi,ambient source Psi),vars Pm);
       mG := if Psi#"maps" =!= null then (for i to #(Psi#"maps")-1 list flatten entries sub(lift(matrix (Psi#"maps")_i,ambient source Psi),vars Pm)) else null;
       Z' := if Psi#"idealImage" =!= null then sub(ideal sub(lift(gens Psi#"idealImage",ambient target Psi),vars Pn),R) else null;
       Bl' := if Psi#"blowUpIdeal" =!= null then sub(Psi#"blowUpIdeal",vars tensor(Pm,Pn,MonomialOrder=>{GRevLex,Position=>Up})) else null;
       psi := new RationalMap from {
               "map" => map(S,R,G),
               "maps" => if mG =!= null then for i to #mG-1 list map(S,R,mG_i) else null,
               "isDominant" => Psi#"isDominant",
               "idealImage" => null,
               "isBirational" => Psi#"isBirational",
               "inverseRationalMap" => null,
               "projectiveDegrees" => Psi#"projectiveDegrees",
               "degree" => Psi#"degree",
               "dimAmbientTarget" => Psi#"dimAmbientTarget",
               "dimTarget" => Psi#"dimTarget",
               "dimAmbientSource" => Psi#"dimAmbientSource",
               "dimSource" => Psi#"dimSource",
               "blowUpIdeal" => Bl'
              };
       if Z' =!= null then forceImage(psi,Z');
       forceInverseMap(phi,psi);
   );
   return phi;
);

toExternalString RationalMap := (Phi) -> (
    n := Phi#"dimAmbientTarget"; m := Phi#"dimAmbientSource";
    K := coefficientRing Phi;
    x := local x; y := local y;
    Pn := K[x_0..x_n]; Pm := K[y_0..y_m];
    Phi = sub(Phi,Pn,Pm);
    Z := Phi#"idealImage";
    Psi := Phi#"inverseRationalMap";
    if Psi =!= null then Z' := Psi#"idealImage";
    str := ///(o -> (
n := ///|toString(n)|///; m := ///|toString(m)|///;
K := ///|toString(K)|///;
x := local x; y := local y;
Pn := K[x_0..x_n]; Pm := K[y_0..y_m];
I := ///|toString ideal source Phi|///;
J := ///|toString ideal target Phi|///;
R := if numgens I > 0 then Pn/I else Pn;
S := if numgens J > 0 then Pm/J else Pm;
x = gens R; y = gens S;
F := ///|toString entries Phi|///;///|
(if Psi =!= null then newline|///G := ///|toString entries inverse Phi|///;/// else "")|///
mF := ///|toString(if Phi#"maps" =!= null then apply(Phi#"maps",f -> flatten entries toMatrix f))|///;///|
(if Psi =!= null then newline|///mG := ///|toString(if Psi#"maps" =!= null then apply(Psi#"maps",g -> flatten entries toMatrix g))|///;/// else "")|///
Z := ///|(if Z =!= null and numgens Z == 0 then ///trim ideal(0_S)/// else toString Z)|///;///|
(if Psi =!= null then newline|///Z':= ///|(if Z' =!= null and numgens Z' == 0 then ///trim ideal(0_R)/// else toString Z')|///;/// else "")|///
PnPm := tensor(Pn,Pm,MonomialOrder=>{GRevLex,Position=>Up});
x = (gens PnPm)_{0 .. n};
y = (gens PnPm)_{n+1 .. n+1+m};
Bl := ///|toString Phi#"blowUpIdeal"|///;
phi := new RationalMap from {
        "map" => map(R,S,F),
        "maps" => ///|(if Phi#"maps" =!= null then ///for i to #mF-1 list map(R,S,mF_i)/// else ///null///)|///,
        "isDominant" => ///|toString(Phi#"isDominant")|///,
        "idealImage" => null,
        "isBirational" => ///|toString(Phi#"isBirational")|///,
        "inverseRationalMap" => null,
        "projectiveDegrees" => ///|toString(Phi#"projectiveDegrees")|///,
        "degree" => ///|toString(Phi#"degree")|///,
        "dimAmbientTarget" => ///|toString(n)|///,
        "dimTarget" => ///|toString(Phi#"dimTarget")|///,
        "dimAmbientSource" => ///|toString(m)|///,
        "dimSource" => ///|toString(Phi#"dimSource")|///,
        "blowUpIdeal" => Bl
       };
if Z =!= null then forceImage(phi,Z);///;
    if Psi =!= null then (
           str = str|///
PmPn := tensor(Pm,Pn,MonomialOrder=>{GRevLex,Position=>Up});
y = (gens PmPn)_{0 .. m};
x = (gens PmPn)_{m+1 .. m+1+n};
Bl' := ///|toString Psi#"blowUpIdeal"|///;
psi := new RationalMap from {
        "map" => map(S,R,G),
        "maps" => ///|(if Psi#"maps" =!= null then ///for i to #mG-1 list map(S,R,mG_i)/// else ///null///)|///,
        "isDominant" => ///|toString(Psi#"isDominant")|///,
        "idealImage" => null,
        "isBirational" => ///|toString(Psi#"isBirational")|///,
        "inverseRationalMap" => null,
        "projectiveDegrees" => ///|toString(Psi#"projectiveDegrees")|///,
        "degree" => ///|toString(Psi#"degree")|///,
        "dimAmbientTarget" => ///|toString(m)|///,
        "dimTarget" => ///|toString(Psi#"dimTarget")|///,
        "dimAmbientSource" => ///|toString(n)|///,
        "dimSource" => ///|toString(Psi#"dimSource")|///,
        "blowUpIdeal" => Bl'
       };
if Z' =!= null then forceImage(psi,Z');
forceInverseMap(phi,psi);///;
    );
    str|///
phi))()///
);

isMorphism (RationalMap) := (Phi) -> (
   B := ideal Phi; 
   dimB := max(dim B - (# heft ambient source Phi),-1);
   dimB == -1
);

isMorphism (MultihomogeneousRationalMap) := (Phi) -> (
   B := ideal Phi; 
   dimB := max(dim B - (# heft ambient source Phi),-1);
   dimB == -1
);

isIsomorphism (RationalMap) := (Phi) -> (
   if Phi#"dimTarget" != Phi#"dimSource" or Phi#"isBirational" === false or Phi#"isDominant" === false then return false;
   if not isMorphism Phi then return false;
   isMorphism inverse Phi
);

load "./Cremona/documentation.m2"

load "./Cremona/tests.m2"

end 

