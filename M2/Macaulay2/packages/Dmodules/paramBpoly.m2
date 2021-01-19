-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

local factorBFunctionZmodP 
local loadVTree
local CALLglobalBFunctionParam
local saveVTree
local paramGB
local gbWparam
local setBSwitch
local inWparam
local takeCareOf
local deleteDollars
--local GroundField

--keys
local children
children = symbol children
local ringOpts
ringOpts = symbol ringOpts
local tempI
tempI = symbol tempI 
local tempN
tempN = symbol tempN
local tempT
tempT = symbol tempT
local tempBF
tempBF = symbol tempBF
local poly
poly = symbol poly
local inv
inv = symbol inv
local iList
iList = symbol iList
local tempV'
tempV' = symbol tempV'
local parentNODE
parentNODE = symbol parentNODE
local Itype
Itype = symbol Itype
local tempV''
tempV'' = symbol tempV''
local level
level = symbol level
local coeff
coeff = symbol coeff 
local cSet
cSet = symbol cSet 

-- attempts to compute the b-polys for a poly with parameters
-- IN>  f: RingElement (an element in a Weyl algebra)
-- IN>  fname: String (base name for the .v3 and .tex files)
-- OUT> a "bSet" (also write the tree of b-polys into .v3 file 
--     	    	      	   	     and prints the bSet to .tex)

-----------------------------------------------------------------------------
-- next comes a ton of small functions needed further...
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- "lucky" prime & QQflag (says when to use Q)
----------------------------------------
DBIGPRIME = 32749;
QQflag := false;

-- get gens from the paramGB output
gensGB := GB -> matrix{GB#0}
-- Matrix to List
toL := u -> (flatten entries (u))
 
-- QtoZ (transforms Q-polys into Z-polys)
QtoZ := (p, Z) -> (
     c := product(listForm p / (u -> denominator u#1));
     sum(listForm p, (u -> promote(numerator (c*u#1), 
		    coefficientRing Z)*Z_(u#0)))
     );

-- QtoZmodP (transforms Q-polys into Z/P-polys)
QtoZmodP := (p, Z) -> (
     K2 := coefficientRing Z;
     ZmodP := coefficientRing K2; 
     sum(listForm p, (u -> (
		    c := product(listForm u#1 / (v -> denominator v#1));
		    (QtoZ(c*u#1, K2) * (1_K2//c))*Z_(u#0)
		    )))
     );

-- ZtoQ (transforms Z-polys into Q-polys)
ZtoQ := (p, Q) -> (
	       sum(listForm p, (u -> promote(u#1, coefficientRing Q)*Q_(u#0)))
	       );
-- (Z/p to Q)
ZmodP2Q := c -> (
     if class c =!= ZZ then c = substitute(c,ZZ);
     F1 := DBIGPRIME//100;
     F2 := 100;
     
     b := DBIGPRIME;
     i := 1;
     while (abs(i * c % b)) > F1 and i < F2 do i = i+1;
     j := 1;
     while (abs(j * (b-c) % b)) > F1 and j < F2 do j = j+1;
     if j < i then ((j * (b-c) % b) / (-j)) else ((i * c % b) / i) 
     );

-- (applies ZmodP2Q to coeffs of polynomial)
ZmodP2Qpoly := f -> (
     if f == 0 then f else(
     	  A := QQ(monoid [options ring f]);   
     	  sum(listForm f / (u -> (
			 ZmodP2Q(substitute(u#1, ZZ))
			 * A_(u#0))))
     	  )
     );

promoteIdeal := (I, R) -> ( 
     if I == 0 then ideal 0_R
     else ideal mingens ideal ((toL gens I) / (u->promote(u, R)))     
     );	  

liftIdeal := (I, R) -> ( 
     K := (ideal ((toL gens I) / (u->lift(u, R)))); 
     ideal mingens (K + promoteIdeal(ideal presentation ring I, R)) 
     );	  

-- liftList
liftList := (u, A) -> (u / (v -> lift(v, A)) );

--least common multiplier
lcm := l -> (
     if #l == 0 then null
     else ( 
	  I := ideal l#0;
	  i := 1;
	  while i<#l do (
	       I = intersect(I, ideal l#i); i = i + 1
	       );
	  I
	  )
     );

----------------------------------------------------------------------------
-- END of "ton of small functions"
---------------------------------------------------------------------------


-------------------------------------------------------------------           
-- calculateAss : (I) -> (list)
-------------------------------------------------------------------
-- I = ideal
-- list = the list of minimal primes associated to I
calculateAss := (I) -> (
     pInfo(2,"calculating minimal primes associated to I = " | 
	  toString I | "...");  
     R := ring I;
     local r;
     if isQuotientRing(R) then (	  
	  J := ideal presentation R;
	  S := ring J;
	  J = J + liftIdeal(I, S); 
	  r = (calculateAss J) / (u -> promoteIdeal(u, R)); 
	  ) 
     else (
	  local l;
	  if QQflag then (
	       --for QQ
	       Z := ZZ(monoid [(entries vars R)#0]);
	       
	       I' := toL(gens I) / (u->QtoZ(u,Z));
	       I' = ideal I';
	       decomp := minimalPrimes(I');
	       l = decomp / (u -> (
			 Ide := ideal (toL(gens u) / (u->ZtoQ(u,R)));
		    	 Ide
		    ));
	       )
	  else ( 
	       -- for ZZ/DBIGPRIME
	       l = minimalPrimes(I);
	       );
	  r = select(l, u -> u != (ideal 1_R));
	  );
     r
     )

factorPolyDropExponents := f -> (
     exp := drop(factor f, -1);
     exp = apply(exp, u -> u#0);
     toString exp
     );


----------------------------------------------------------------------------
-- Data structures needed for b-1.m2
----------------------------------------------------------------------------

-- tree of varieties:
--
-- NODE := (	
--     	    	I: ideal, 
--     	    	Itype: integer (COMPUTED, NOTCOMPUTED 
--                     or the reference for a NODE n such that n#tempI <= I),
--     	    	level: integer,
--     	   	bf: b-polynomial, 
--     	    	inv: elements inverted when computing bf,
--     	    	children: list of NODEs below this NODE,   
--     	    	parentNODE: parent, 
--     	    	t: time used by b-poly algorithm
--     	   ); 

-- Itypes:
COMPUTED := -1;
NOTCOMPUTED := -2;

createVTree := f -> (
     R := ring f;
     new HashTable from {
	  coeff => coefficientRing R,
	  ringOpts => options R,
	  poly => f,
	  tempN => {} 
	  }  
     ); 

saveVTree = method()
saveVTree(HashTable, String) := (V, filename) -> ( 
     filename << toString V#coeff << endl << toString V#ringOpts << endl
     << toString V#poly << endl << toString V#tempN	<< endl << close )

deleteDollars = method()
deleteDollars(String) := (s) -> (
     cs := characters s;
     ret := "";
     scan(cs, c->(if c!="$" then ret=ret|c));
     ret 
     ) 
loadVTree = method()
loadVTree(String) := filename -> (
     f := lines get filename;
     f = apply(f, deleteDollars);
     K := value f#0;
     opts := value f#1;
     A := K(monoid [opts]);
     new HashTable from {
	  coeff => K,
	  ringOpts => opts,
	  poly => value f#2,
	  tempN => value f#3 
	  }
     )

toString(Option) := o -> (toString o#0 | "=>" | toString o#1);

search4ABigGuy := (V, I, pNODE) -> (
     dad := V#tempN#pNODE;
     grandpa := dad#parentNODE;
     pInfo(3, {"grandpa: ideal =", I, " parent = ", pNODE});
     if grandpa < 0 then NOTCOMPUTED
     else (
	  pInfo(3, "grandpa children: " | 
	       toString ((V#tempN#grandpa)#children / (i->
			 (i, isSubset((V#tempN#i)#tempI, I)))));
	  temp := select(1, (V#tempN#grandpa)#children, 
--	       i-> (i < parentNODE) and isSubset((V#tempN#i)#tempI, I) ); 
	       i-> (i < pNODE) and isSubset((V#tempN#i)#tempI, I) ); 
	  if #temp > 0 then temp#0
	  else search4ABigGuy(V, I, grandpa)
	  ) 
     );

add2VTree := (V, II, p) -> (
     pInfo(3, "add2VTree"|toString {V,II,p});
     new HashTable from {
	  coeff => V#coeff,
	  ringOpts => V#ringOpts,
	  poly => V#poly,
	  tempN  => append(V#tempN, new HashTable from {
	       	    tempI => II,
		    Itype => (
			 if p < 0 then NOTCOMPUTED 
		    	 else search4ABigGuy(V, II, p)
			 ),
		    level => if p < 0 then 0 else (V#tempN#p)#level + 1,
		    parentNODE => p
		    })  
	  } 
     );

modifyNODE := (V, num, l) -> (
     ---------(V, num, {bf, inv, children, t})
     kidNumbers := l#2 / (J -> (
	       V = add2VTree(V, J, num);
	       #(V#tempN) - 1     -- the number of the last element
	       )); 
     new HashTable from {
	  coeff => V#coeff,
	  ringOpts => V#ringOpts,
	  poly => V#poly,
	  tempN => take(V#tempN, num) | {new HashTable from {
	       	    tempI => (V#tempN#num)#tempI,
		    Itype => COMPUTED,
		    level => (V#tempN#num)#level,
		    tempBF => l#0,
		    inv => l#1,
		    children => kidNumbers,
		    parentNODE => (V#tempN#num)#parentNODE,
		    tempT => l#3
	       	    }} | drop(V#tempN, num + 1)   
	  } 
     );

modifyCSET := (V, num, l) -> (
     old := V#tempN#num;
     new HashTable from {
	  coeff => V#coeff,
	  ringOpts => V#ringOpts,
	  poly => V#poly,
	  tempN  => take(V#tempN, num) | {new HashTable from {
	       	    tempI => old#tempI,
		    Itype => old#Itype,
		    level => old#level,
		    tempBF => old#tempBF,
		    inv => l,
		    children => old#children,
		    parentNODE => old#parentNODE,
		    tempT => old#tempT
	       	    }} | drop(V#tempN, num + 1)   
	  } 
     );


cutCrapOut := l -> (
     i := 0;
     while i < #l do (
	  l' := drop(l, {i,i});
	  if any(l', j -> isSubset(j, l#i)) then (
		    << "throwing away " << l#i << endl;
		    l = l';
		    )
	  else i = i + 1;
     	  );
     l
     );

refineVTree := (V, num) -> (
     temp := V#tempN#num;
     if temp#Itype < 0 and #temp#children != 0 then (
	  scan(temp#children, u -> (
		    V = refineVTree(V, u);
	       	    temp2 := V#tempN#u;
		    if temp2#Itype < 0 and temp2#tempBF == temp#tempBF then (
		    	 << "patching node #" << num << endl;
			 p := position(temp#inv, v-> v == temp2#tempI);
			 if p===null then error "VTree is damaged!";
			 V = modifyCSET(V, num, drop(temp#inv, {p,p}) | temp2#inv);
			 temp = V#tempN#num;
			 );
	       ));
     	  V = modifyCSET(V, num, cutCrapOut(temp#inv));   
	  ); 
     V
     );

appendCSet := (c, l) -> (
     i := 0;
     isDifferent := true;
     while isDifferent do (
	  if (c#i)#tempV' == l#tempV' then isDifferent = false
	  else i = i + 1; 
	  );
     if isDifferent then take(c,i) | {l} | drop(c,i)
     else take(c,i) | {new HashTable from{
		    tempV'' => cutCrapOut (l#tempV'' | (c#i)#tempV''),
		    tempV' => l#tempV'
		    }} | drop(c,i+1)
     );

chooseMinList := l -> (
     select(l, u->(
	       all(l, v->(
			 (not isSubset(v,u) )
			 or v == u
			 ))
	       ))
     );
chooseMinListList := (l1, l2) -> (
     chooseMinList select(l1, u->any(l2, v-> isSubset(v,u)))
     );
 
VTree2bSet2 := V -> (
     --Itype = symbol Itype;
     if any(V#tempN, u -> (u#Itype == NOTCOMPUTED)) then
     error "VTree is not complete"; 
     
     -- for each b(s) \in B(n,d)
     -- make the list of ideals for which b(s) is a generic b-function 
     r := {};
     scan(V#tempN, node->( 
	       if node#Itype == COMPUTED then 
	       (
		    t := position(r, u ->u#tempBF == node#tempBF);
		    r = (
			 if t =!= null 
		    	 then take(r, t) | { 
			      new HashTable from {
			      	   tempBF => node#tempBF,
			      	   iList => (r#t)#iList | {
					if node#tempI == 0 then ideal 0_(V#coeff)
			      		else node#tempI  
			      		}
				   }
			      } | drop (r, t+1)
		    	 else r | { 
			      new HashTable from {
				   tempBF => node#tempBF,
			      	   iList => {
					if node#tempI == 0 then ideal 0_(V#coeff)
			      		else node#tempI  
					} 
				   }
			      }
			 ); 
     	       	    )
	       ));      
     i := 0;
     while i < #r do (
	  l1 := (r#i)#iList;
	  l2 := flatten(drop(r, {i,i}) / ( u -> u#iList));
	  c := { new HashTable from {
	       	    tempV' => {"not empty"},
		    tempV'' => {ideal 0_(V#coeff)} 
		    }};
	  while #((last c)#tempV') > 0 do (
	       -- choose min ideals in arg1 containing 
	       -- at least one ideal from arg2 
	       temp := chooseMinListList(l1, (last c)#tempV'');
	       c = c | { new HashTable from {
			 tempV' => temp,
			 tempV''=> chooseMinListList(l2, temp)
			 }}; 
	       );
     	  r = take(r,i) | {  new HashTable from {
			      	   tempBF => (r#i)#tempBF,
			      	   iList => (r#i)#iList,
				   cSet => take(c,{1,#c-2}) 
				   } 
			      } | drop(r, i+1); 
     	  i = i + 1; 
	  );
     r
     ); 

isNonEmpty := h -> ( #h#tempV'' == 0 or not isSubset( intersect(h#tempV''), h#tempV') );


------------------------------------
-- TeX output
------------------------------------     
Sequence2String := s -> (
          if # s === 1 then concatenate("(",toString s#0,")")
          else concatenate("(",between(",",toString \ s),")")
          )


VSet2tex := (l, TeXfile) -> (
     j := 0;
     while j < #l do(
	  vv := l#j;
	  TeXfile << "$V" 
	  << Sequence2String (first entries gens vv / ZmodP2Qpoly)
	  << (if j < #l - 1 then " \\cup $ " else "$");
	  j = j + 1;
	  );  
     );

factorBFunctionZmodP = method()
factorBFunctionZmodP RingElement := Product => f -> (
     R := ring f;
     f = select(factor f, u->first degree u#0 > 0);
     f = select(f, u->first degree u#0 > 0);
     S := symbol S;
     QR := QQ[S];
     result := apply(f, u->(
	       if first degree u#0 != 1 then error "internal error: incorrect b-function";
	       coeff := listForm u#0 / (v->v#1);
	       pInfo(666, {"coeff = ", coeff});
     	       Power(QR_0 + (if #coeff> 1 then ZmodP2Q(coeff#1//coeff#0) else 0), u#1)
	       ));
     result
     );-- end factorBFunctionZmodP

BSet2tex := (s, filename) -> (
     TeXfile := openOut filename;
     TeXfile << "\\documentclass{article}\n\\begin{document}\n"; 
     s / (u -> ( 
	       TeXfile << "\n\n$$ b(s)=";
	       TeXfile << (
		    -- b-function
		    S := symbol S; 
		    R := (ZZ/DBIGPRIME)[S];
		    toString factorBFunctionZmodP sum(u#tempBF, 
			v->(if class v#1 === ZZ 
			     -- hashtable was loaded from file
			     then v#1
			     -- original hashtable
			     else substitute(v#1,ZZ)) 
			* R_(v#0))
	       ) << ",$$";
	       
	       i := 0;
	       while i < #u#cSet do( 
		    VSet2tex((u#cSet#i)#tempV', TeXfile);
	       	    if #(u#cSet#i)#tempV'' > 0 then TeXfile << "$\\setminus$";
	       	    VSet2tex((u#cSet#i)#tempV'', TeXfile);
	       	    if i < #u#cSet - 1 then TeXfile << "$\\cup$";
		    i = i + 1;
		    );	    
	       ));
     TeXfile << "\\end{document}" << close;
     );

------------------------------------------------------------------------------
-- ***************************************************************************
-- globalBFunction (2 versions) adapted for b-1
-- ***************************************************************************
------------------------------------------------------------------------------
getChangeMat := g->g#1
getInv := GB -> (
     l := flatten entries getChangeMat GB;
     l = l / (u -> (entries transpose (coefficients u)#1)#0);
     l = (flatten l) / (u -> lift(u, coefficientRing ring GB#0#0));
     select(l / (u -> denominator u), u -> not isUnit u)
     ); 
-- a local function used by gbW and inW  

homGBparam := (I, w) -> (
     W := ring I;
     createHomWeylAlgebra W;
     HW := W.HomWeylAlgebra;
     dpairs := W.monoid.Options.WeylAlgebra;
     -- Do some sanity checking
     if dpairs === {} 
     then error "expected a Weyl algebra";
     if any(dpairs, v -> class v =!= Option)
     then error "expected non-homogenized Weyl algebra";
     if #w =!= numgens W
     then error ("expected weight vector of length " | numgens W);
     -- Make the new weight vector
     wts := prepend(-1,w);

     -- Homogenize I
     I1 := W.WAtoHWA I;
     homogenize(gens I1, HW_0, wts)
     );

-- computes the GB of ideal "I" with respect to weight vector "w"  
gbWparam = method()
gbWparam(Ideal, List) := (I, w) -> (
     I2 := homGBparam(I, w);
     -- Do the computation *************************************************
     ideal compress (ring I).HWAtoWA gens paramGB I2
     );-- end gbW

-- computes in_w(I). The result is a WA ideal (as opposed to 
-- an ideal in the associated commutative ring) 
inWparam = method()
inWparam(Ideal,List) := (I, w) -> (
     I2 := homGBparam(I, w);
     GB := paramGB (I2);    
     I3 := leadTerm(1, gensGB GB);
     (ideal compress (ring I).HWAtoWA I3, getInv(GB)) 
     );

local globalBFunctionParam;
local globalBFunctionParam2;
----------------------------------------------------------------------------
-- This function calls 2 versions of globalBFunction with parameters
BSWITCH := 1
setBSwitch = method()
setBSwitch ZZ := n -> (
     if n!=1 and n!=2 then error "invalid BSWITCH";
     BSWITCH = n;     
     )
CALLglobalBFunctionParam = method()
CALLglobalBFunctionParam(Thing) := a -> (
     if BSWITCH == 1 then globalBFunctionParam a
     else globalBFunctionParam2 a 
     );

---------------------------------------------------------------------------
 
makeMonic := f -> ( (1 / (leadCoefficient f)) * f );
ZZtoQQ := u -> (
     c := substitute(lift(u, ZZ/DBIGPRIME), ZZ);
     b := DBIGPRIME;
     i := 1;
     while (abs(i * c % b)) > 100 and i < 100 do i = i+1;
     ((i * c % b) / i) 
     );

ZZmakeQQ := f -> (
     Q := QQ(monoid [first entries vars ring f]);
     try sum(listForm f / (u-> ZZtoQQ(u#1) * Q_(u#0)))
     else f
     );

makeQQ := f -> (
     s := symbol s;
     R := QQ[s];
     if not member(QQ, R.baseRings) then
     error "QQ is not a base ring of R";
     sum(listForm f, u -> lift(u#1, QQ) * s^(sum u#0))
     );

---------------------------------------------------------------------
-- 1st version of globalB with parameters 
---------------------------------------------------------------------
bFunctionParam := (I, w) -> (
     if not (ring I).?ThetaRing then
     createThetaRing (ring I);

     if not (ring I).ThetaRing.?IntRing then
     createIntRing (ring I).ThetaRing;
     
     createDpairs (ring I);

     W := ring I;
     T := W.ThetaRing;
     TI := T.IntRing;
     dpV := W.dpairVars;
     dpI := W.dpairInds;
     
     -- sanity check
     if (#(dpI#2) != 0) then
     error "expected no central variables in Weyl algebra";
     if (#w != ((numgens W) // 2)) then
     error "expected weight vector of length " | ((numgens W) // 2);
     
     w = apply(numgens W, i -> (
	       p := position(dpI#1, u -> u == i); 
	       if p =!= null  then w#p
	       else (
		    p = position(dpI#0, u -> u == i);
		    -w#p
		    )  
	       ));
     -- compute in_(-w,w) (I) 
     temp := inWparam(I, w); 
     inv := temp#1;
     inI := temp#0;
     n := #(dpI#0);
     eulerOp := sum(n, i -> w_(dpI#1#i)*(dpV#0#i)*(dpV#1#i));
     	  
     (     
	  -- NON-GENERIC
	  dpI' := {select(dpI#0, i -> w#i != 0), 
	       select(dpI#1, i -> w#i != 0)};
	  dpI'' := {select(dpI#0, i -> w#i == 0), 
	       select(dpI#1, i -> w#i == 0)};
	  -- want: eliminate all u_i, v_i as well as x_i, dx_i of weight 0
	  u := symbol u;
	  v := symbol v;	  
	  UV := (coefficientRing W)(monoid [ (dpI'#0) / (i -> u_i), 
	       (dpI'#1) / (i -> v_i), 
	       (dpI''#0) / (i -> W_i), (dpI''#1) / (i -> W_i),
	       (dpI'#0) / (i -> W_i), (dpI'#1) / (i -> W_i),
	       WeylAlgebra => W.monoid.Options.WeylAlgebra,
	       MonomialSize => 16,
	       MonomialOrder => Eliminate (2 * #dpI#0) ]);
	  WtoUV := map(UV, W, matrix { apply(numgens W, i -> (
			      if member(i, dpI'#0) then 
			      (UV_(u_i) * substitute(W_i, UV))
			      else if member(i, dpI'#1) 
			      then (substitute(W_i, UV) * UV_(v_i))
			      else substitute(W_i, UV)
			      )) 
		    });
	  
	  GB :=  paramGB ((WtoUV inI) 
	       	    + ideal apply(#dpI'#0, 
			 i -> (UV_(u_(dpI'#0#i)) * UV_(v_(dpI'#1#i)) - 1)
		    	 ));
	  inv = inv | getInv(GB);
	  intGB := gensGB GB;
	  intIdeal := ideal substitute(selectInSubring(1, intGB), W);
	  );
     	  -- compute J = intIdeal \cap K[\theta]
     	  genJ := (flatten entries gens intIdeal) / W.WtoT;
	  elimIdeal := (T.RtoIR ideal genJ)
     	  + ideal(TI_(numgens TI - 1) - T.RtoIR W.WtoT eulerOp);
     	  
	  GB = paramGB(elimIdeal);
	  inv = inv | getInv(GB);
	  elimIdealGB := gensGB GB;
	  -- take the generator of J and cook up the b-function
	  bfcn := (mingens ideal selectInSubring(1,elimIdealGB))_(0,0);
	  s := symbol s;
	  TItoS := map((coefficientRing TI)[s], TI, 
	       matrix{toList(numgens T:0) | {s}});
	  bfcn = TItoS bfcn;
	  --(if QQflag 
	  --     then makeQQ makeMonic bfcn
	  --     else ZZmakeQQ makeMonic bfcn, 
	  --     
	  --     inv)
	  (bfcn, inv)     	
     );-- end of bFunctionParam

globalBFunctionParam = f -> (
     W := ring f;
     createDpairs W;
     dpI := W.dpairInds;
     
     -- sanity check
     if (#(W.dpairInds#2) != 0) then
     error "expected no central variables in Weyl algebra";
     if any(listForm f, m -> any(dpI#1, i -> m#0#i != 0)) then
     error "expected no differentials in the polynomial";
     
     t := symbol t;
     dt := symbol dt;     
     WT := (coefficientRing W)(monoid [ t, dt, (entries vars W)#0,
	       MonomialSize => 16,
	       WeylAlgebra => W.monoid.Options.WeylAlgebra | {t => dt}]);
     t = WT_t;
     dt= WT_dt;
     w := {1} | toList (((numgens W) // 2):0);
     f' := substitute(f,WT);
     If := ideal ({t - f'} 
     	  | (dpI#1 / (i->(
	       	    	 DX := WT_(i+2);
	       	    	 (DX * f' - f' * DX) * dt + DX
	       	    	 )))
	  );
     temp := bFunctionParam(If, w);
     bfunc := temp#0;
     inv := temp#1;
     s := (ring bfunc)_0;
     (makeMonic substitute(bfunc, { s => -s - 1 }), inv)
     );--end of globalbFunctionParam

-------------------------------------------------------------
-- 2nd version of globalB with parameters 
--------------------------------------------------------------
AnnFsParam := f -> (
     pInfo(1, "computing AnnFsParam... ");
     W := ring f;
     K := last (coefficientRing W).baseRings;
     np := numgens K; 
     ord := W.monoid.Options.MonomialOrder;
     -- number of parameters
     n := numgens W;
              	       
     createDpairs W;
     dpI := W.dpairInds;
     dpV := W.dpairVars;
     I := ideal dpV#1;
     
     t := symbol t;
     dt := symbol dt;
     WAopts := W.monoid.Options.WeylAlgebra | {t => dt};
     WT := (coefficientRing W)(monoid [ t, dt, (entries vars W)#0, 
	  WeylAlgebra => WAopts,
	  MonomialOrder => Eliminate 2 ]);
     u := symbol u;
     v := symbol v;
     WTUV := (coefficientRing W)(monoid [ u, v, t, dt, (entries vars W)#0,
	  WeylAlgebra => WAopts,
	  MonomialOrder => Eliminate 2 ]);
     WtoWTUV := map(WTUV, W, (vars WTUV)_{4..n+3});
     -- twist generators of I into generators of KI
     f' := substitute(f,WTUV);
     twistList := apply( toList(3..n+3), 
	  i -> WTUV_i + (WTUV_i*f' - f'*WTUV_i)*dt);
     twistMap := map(WTUV, WTUV, matrix{{u,v,t-f'}|twistList});
     tempKI := twistMap(ideal t + WtoWTUV I);
     wts := {1,-1,1,-1} | toList(n:0);
     KI := ideal homogenize(gens tempKI, u, wts);
     
     g := (entries gens KI)#0 | { u * v - 1 };
     GB := paramGB(ideal g);
     inv := getInv GB;
     preGens := flatten entries substitute(
	  selectInSubring(1, gensGB GB), WT);
     s := symbol s;
     WS := (coefficientRing W)(monoid [(entries vars W)#0, s,
	  WeylAlgebra => W.monoid.Options.WeylAlgebra]);
     WTtoWS := g -> (
	  e := first exponents leadMonomial g;
	  if e#0 > e#1 then g = dt^(e#0-e#1) * g
	  else g = t^(e#1-e#0) * g;
	  g' := 0_WS;
	  while (d := first exponents leadMonomial g; d#0 * d#1 != 0) do(
	       c := leadCoefficient g;
	       g' = g' + c * (-s-1)^(d#1) * WS_(drop(d, 2) | {0}); -- >%-0	
	       g = g - c * (t*dt)^(d#1) * WT_({0,0} | drop(d, 2));
	       ); 
	  g' + substitute(g, WS)
	  );
     pInfo(666, {"AnnFSparam = ", (preGens / WTtoWS)});
     (ideal (preGens / WTtoWS), inv) 
     )

globalBFunctionParam2 = f -> (
     W := ring f;
     
     temp := AnnFsParam f;
     AnnI := temp#0;
     inv := temp#1;
     
     Ws := ring AnnI;
     ns := numgens Ws;
     
     
     elimWs := (coefficientRing Ws)(monoid [(entries vars Ws)#0,
	  WeylAlgebra => Ws.monoid.Options.WeylAlgebra,
	  MonomialOrder => Eliminate (ns-1)]);
     ff := substitute(f,elimWs);
     elimAnnI := substitute(AnnI, elimWs);
     H := (gens elimAnnI) | matrix{{ff}};
     
     gbH := paramGB(H);
     inv = inv | getInv gbH; 
     
     bpolys := selectInSubring(1, gensGB gbH);
     if (bpolys == 0) then error "module not specializable";
     if (rank source bpolys > 1) then error "ideal principal but not
     realized as such.  Need better implementation";
     bpoly := bpolys_(0,0);
     
     Ks := (coefficientRing W)(monoid [Ws_(ns-1)]);
     bpoly = substitute(bpoly, Ks);
     (bpoly, inv)
     )

---------------------------------------------
-- takes care of a node in a VTree
---------------------------------------------
takeCareOf = method()
takeCareOf(HashTable, ZZ) := (V, num) -> (
     pInfo(1, "computing node #" | toString num | "...");
     node := V#tempN#num;
     K := frac(V#coeff / node#tempI);
     A := K(monoid [ V#ringOpts ]); 
     n := numgens A;
     f := sum(listForm V#poly / (u->promote(u#1,K) * A_(u#0)));
     
     timeSpent := timing (bf := CALLglobalBFunctionParam f);
     inv := bf#1;
     bf = bf#0;
          
     assocPrimes := select(
     	  if inv != {} 
     	  then calculateAss liftIdeal(lcm(inv), V#coeff) 
     	  else {},
	  I ->  (
	       TK := frac(V#coeff / I);
     	       TA := TK(monoid [ V#ringOpts ]); 
	       sum(listForm V#poly / (u->promote(u#1,TK) * TA_(u#0))) !=0
	       )
     	  );
     modifyNODE(V, num, {apply(listForm bf, u->(u#0,substitute(u#1,ZZ))), 
	       assocPrimes, assocPrimes, timeSpent#0})
     );

----------
-- MAIN
----------

paramBpoly = method(Options => {GroundField => 32749}) -- 0 stays for QQ
paramBpoly(RingElement, String) := List => o -> (f, fname) -> (
     QQflag = (o#GroundField == 0);
     if not QQflag then (
	  if isPrime o#GroundField 
     	  then DBIGPRIME = o#GroundField
     	  else error "need a prime";
	  )
     else error "algorithm is implemented over finite field so far"; 
     R := ring f;
     K := coefficientRing R;
     if coefficientRing K =!= QQ then 
     error "base ring = QQ expected";
	  
     Z := ((ZZ/DBIGPRIME)(monoid [(entries vars K)#0]))(monoid [
	  (entries vars R)#0, WeylAlgebra => R.monoid.Options.WeylAlgebra]);
     f = QtoZmodP(f,Z);  
     V := createVTree(f);
     V = add2VTree(V, ideal 0_(coefficientRing Z), -1);     
     local num;
     while (num = position(V#tempN, u -> u#Itype == NOTCOMPUTED)) =!= null 
     do ( 
     	  V = takeCareOf(V, num);
     	  --!!!saveVTree(V, (fname| ".v3"));
     	  );
     --!!!V = loadVTree((fname| ".v3"));
     -- transform into bSet
     bs := VTree2bSet2 V;
     BSet2tex(bs,(fname | ".tex"));
     ret := apply(bs, u->(
	       s := symbol s;
	       RZ := (ZZ/DBIGPRIME)[s];
	       factorBFunctionZmodP(
		    sum(u#tempBF, v->
			 (if class v#1 === ZZ 
			     -- hashtable was loaded from file
			     then v#1
			     -- original hashtable
			     else substitute(v#1,ZZ)) 
			* RZ_(v#0))) 
	       ));
     ret
     );


--VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
-- ex-"paramGB.m2" contents
--VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV  

--------------------------------------------------------------
-- Computes parametric GB
--------------------------------------------------------------

--some helpful funcs
--ring(List) := g -> ring g#0
--gens (List) := g -> matrix{g#0}

myLCM := l -> (
     if #l == 0 then null
     else ( 
	  I := ideal l#0;
	  i := 1;
	  while i<#l do (
	       I = intersect(I, ideal l#i); i = i + 1
	       );
	  first first entries mingens I
	  )
     );

in2 := f -> if f!=0 then leadTerm f else 0_(ring f) 

isdivisible := (f,g) -> (
     if (f==0) or (g==0) 
     then false 
     else(
     	  a := listForm f;
       	  b := listForm g;	
       	  all(a#0#0-b#0#0, u->(u>=0))
     	  )
     )

---------------------------------------------------------------
-- paramGB
-- input: I: ideal
--        ChangeMatrix=>false: option
-- output: { Groebner basis generators for I [, changematrix] }

paramGB = I -> (
    if class I === Ideal then I = gens I;
    R := ring I;
    WAflag := R.monoid.Options.WeylAlgebra =!= {};
    if WAflag then (
	 createDpairs R;
	 xVar := R.dpairInds#0;
	 dxVar := R.dpairInds#1;
	 );
    K := last (coefficientRing R).baseRings;
    isQuotFlag := isQuotientRing(K);  
    L := if isQuotFlag then ring presentation K else K;
    varP := first entries vars L;
    np := #varP;
    ord := R.monoid.Options.MonomialOrder;
    -- MES: I replaced the following by the one below it.
    newMonomialOrderOLD := ord -> (
	 if class ord === Nothing then ProductOrder {numgens R, np}
	 else if class ord === Eliminate then 
	 ProductOrder {ord#0, numgens R - ord#0, np}
	 else if class ord === ProductOrder then
	 ProductOrder ((toList ord) | {np}) 
	 );
    newMonomialOrder := ord -> (
	 append(ord, GRevLex=>np));
    Rgens := (options R).Variables;
    Lgens := (options L).Variables;
    
    -- make a new ring with parameters
    -- !!! The only point of using "xxx", "mmm" was the fact 
    -- !!! that M2 crashes if the old variables are used
    xxx := symbol xxx;
    mmm := symbol mmm;
    NewR := (coefficientRing L) [
	 xxx_0..xxx_(numgens R - 1),
	 mmm_0..mmm_(np-1),  
	 WeylAlgebra => 
	 (if WAflag 
	      then apply(#xVar, i -> (xxx_(xVar#i) => xxx_(dxVar#i)))
	      else {}
	      ),
	 MonomialOrder => newMonomialOrder R.monoid.Options.MonomialOrder
	 ];
    L2NewR := map( NewR, L, toList(0..np-1) / (i->mmm_i) );
    Zero := if isQuotFlag then L2NewR ideal presentation K
    else ideal 0_NewR;
    R2NewR := f -> (
	 l := listForm f;
	 mlcm := myLCM (l / (u -> lift(denominator u#1, L)));
	 l = l / (u -> ( u#0, lift(numerator u#1, L) * (
			mlcm // lift(denominator u#1, L)) ));
	 sum(l, (u -> (L2NewR u#1) * NewR_( toList u#0 | toList(np : 0) )))
	 );
    J := first entries I / R2NewR;
    g := first entries gens gb (ideal J + Zero);
    
    -- go back to the original ring
    NewR2R := f -> (
	 l := listForm f;
	 l = l / (u -> 
	      (drop(toList u#0, -np), u#1 * L_(drop(toList u#0, numgens R))));
	 sum(l, (u -> promote(u#1, K) * R_(u#0)))    
	 );
    g = g / NewR2R;      	  
    
    -- clean up
    i := 0;
    while i < #g do (
	 if g#i == 0_R
	 then (
	      g = drop(g, {i,i})
	      )
	 else i = i + 1; 
	 );
    i = 0;
    while i < #g do (
	 if any(drop(g, {i,i}), u->isdivisible(in2 g#i, in2 u)) 
	 then g = drop(g, {i,i})
	 else i = i + 1; 
	 );
    
    -- construct the matrix of invertibles
    inv := matrix { g / (f -> (
		   (1_K / (leadCoefficient f))*1_R
		   ))};
    
    -- make every generator monic
    g = g / ( u -> makeMonic u);
    
    {g, inv}
    );

















