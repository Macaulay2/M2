-- attempts to compute the b-polys for a poly with parameters
-- IN>  f: RingElement (an element in a Weyl algebra)
-- IN>  fname: String (base name for the .v3 and .tex files)
-- OUT> a "bSet" (also write the tree of b-polys into .v3 file 
--     	    	      	   	     and prints the bSet to .tex)

paramBpoly = method()
paramBpoly(RingElement, String) := (f, fname) -> (
     QQflag = false;
     fakeGBon = false;
     V = createVTree(f);
     V = add2VTree(V, ideal 0_(coefficientRing A), -1);     
     while (num = position(V.n, u -> u.Itype == NOTCOMPUTED)) =!= null do ( 
     	  V = takeCareOf(V, num);
     	  saveVTree(V, (fname| ".v3"));
     	  );
     V = loadVTree((fname| ".v3"));
     -- transform into bSet
     (fname | ".tex")  << toString V.poly << endl; 
     bs = VTree2bSet2 V;
     BSet2tex(bs, (fname | ".tex"));
     use ring f;
     apply(bs, u->(
	       RZ := (ZZ/bigPrime)[symbol s];
	       ZmodP2Qpoly(sum(u.bf, v->v#1*RZ_(v#0))) 
	       ))
     );

----------------------------------------------------------------------------------
-- GLOBAL "lucky" prime
bigPrime = 32749;

netP = method();
netP(RingElement) := p -> (
     toString p
     );
--Global things:
-- n = 1
-- d = 3
toL = u -> (flatten entries (u))
 
-- oakuMod : (A, f) -> (bf, s)
----------------------------
-- A = a WA with coefficients in a ring  
-- f = a polynomial in A 
-- bf = b-polynomial of f
-- s = the list of elements in the coefficient ring that were inverted

-- QtoZ (transforms Q-polys into Z-polys)
QtoZ := (p, Z) -> (
     c := product(listForm p / (u -> denominator u#1));
     sum(listForm p, (u -> lift(c*u#1, coefficientRing Z)*Z_(u#0)))
     );
-- ZtoQ (transforms Z-polys into Q-polys)
ZtoQ := (p, Q) -> (
	       sum(listForm p, (u -> promote(u#1, coefficientRing Q)*Q_(u#0)))
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
lcm = method()
lcm(List) := l -> (
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

-- old lcm: factors things
oldlcm = l -> (
     if #l == 0 then null
     else ( 
	  r := {};
	  i := 0;
	  while i<#l do (
	       t := factor l#i;
	       scan(t, u -> (
			 if (deg u#0) > 0 then (	
			      j := position(r, v -> (
			      	   	v == u#0));	  
			      if j === null then r = append(r, u#0);
			      )));  
	       i = i + 1;
	       );
	  r	  	  
	  )      
     );

-------------------------------------------------------------------           
-- calculateAss : (I) -> (list)
-------------------------------------------------------------------
-- I = ideal
-- list = the list of minimal primes associated to I
calculateAss = (I) -> (
     pInfo(2,"calculating minimal primes assosiated to I = " | toString I | "...");  
     R := ring I;
     --print (R, presentation R);
     local r;
     if isQuotientRing(R) then (	  
	  J := ideal presentation R;
	  S := ring J;
	  --<< "S = " << S << endl;
	  J = J + liftIdeal(I, S); 	  --print J;
	  r = (calculateAss J) / (u -> promoteIdeal(u, R)); 
	  ) 
     else (
	  local l;
	  if QQflag then (
	       --for QQ
	       --Z := (ZZ/bigPrime)[(entries vars R)#0];
	       Z := ZZ[(entries vars R)#0];
	       
	       I' := toL(gens I) / (u->QtoZ(u,Z));
	       I' = ideal I';
	       --print (I', ring I');
	       decomp := decompose(I');
	       l = decomp / (u -> (
			 Ide := ideal (toL(gens u) / (u->ZtoQ(u,R)));
		  	 --print (Ide, ring Ide);
		    	 Ide
		    ));
	       )
	  else ( 
	       -- for ZZ/bigPrime
	       l = decompose(I);
	       );
	  --<< "DEcomposition in ZZ: " << toString l << endl;
	  r = select(l, u -> u != (ideal 1_R));
	  );
     --<< "Ass( " << toString I << " ) = " << toString r << endl;
     r
     )

----------------------------------------------------------------
-- bSet : (f)->(table)
---------------------------------------------------------------
-- f = polynomial in a WA
-- table = the table of b-polynomials and the correspondent b-sets
     -- the "table" consists of 
     --    <b-polynomial> => { (a,b) , ... },
     -- where f != 0 for any f in the list a
     -- and f = 0 for any f in the ideal b
     -- (So that the RHS describes a constructible set.) 
factorPolyDropExponents := f -> (
     exp := drop(factor f, -1);
     exp = apply(exp, u -> u#0);
     toString exp
     );

takeCareOf = method()
takeCareOf(HashTable, ZZ) := (V, num) -> (
     pInfo(1, "computing node #" | toString num | "...");
     node := V.n#num;
     K := frac(V.coeff / node.I);
     A := K[ V.ringOpts ]; 
     n := numgens A;
     f := sum(listForm V.poly / (u->promote(u#1,K) * A_(u#0)));
     
     timeSpent := timing (bf := CALLglobalBFunctionParam f);
     inv := bf#1;
     bf = bf#0;
          
     use V.coeff;
     mIdeal := ideal vars V.coeff;          
     assocPrimes := select(
	  if inv != {} then calculateAss liftIdeal(lcm(inv), V.coeff) else {},
	  I -> not isSubset(mIdeal, I) 
	  );
     modifyNODE(V, num, {listForm bf, assocPrimes, assocPrimes, timeSpent#0})
     );

-----------------------------------------------------------------------------------
-- n = # of variables
-- d = degree
-----------------------------------------------------------------------------------

initializeVTree = (n, d)->(
     -- coefficient field
     -- p_(a1,...,an) is the coefficient of (x1)^a1 * ... * (xn)^an
     indices := select(toList(toList(n:0)..toList(n:d)), u->((sum u) <= d));
     K := if QQflag then QQ else ZZ/bigPrime;
     R := K[indices / (u -> p_u), MonomialSize =>16];
     --the WA that we use
     optWA := (toList(1..n)) / (i->(x_i=>dx_i));
     A := R[x_1..x_n, dx_1..dx_n, WeylAlgebra => optWA, MonomialSize =>16];
     f := sum( indices / (u -> p_u * A_(u | toList(n:0))) );
     V := createVTree(f);
     add2VTree(V, ideal 0_R, -1)
     )     


----------------------------------------------------------------------------------
-- Data structures needed for b-1.m2
----------------------------------------------------------------------------------

-- tree of varieties:
--
-- NODE := (	
--     	    	I: ideal, 
--     	    	Itype: integer (COMPUTED, NOTCOMPUTED or the reference for a NODE n such that n.I <= I),  
--     	    	level: integer,
--     	   	bf: b-polynomial, 
--     	    	inv: elements inverted when computing bf,
--     	    	children: list of NODEs below this NODE,   
--     	    	parentNODE: parent, 
--     	    	t: time used by b-poly algorithm
--     	   ); 

-- Itypes:
COMPUTED = -1;
NOTCOMPUTED = -2;

createVTree = method()
createVTree(RingElement) := f -> (
     R := ring f;
     new HashTable from {
	  symbol coeff => coefficientRing R,
	  symbol ringOpts => options R,
	  symbol poly => f,
	  symbol n  => {} 
	  }  
     ); 

toString(Option) := o -> (toString o#0 | "=>" | toString o#1);

saveVTree = method()
saveVTree(HashTable, String) := (V, filename) -> ( 
     filename << toString V.coeff << endl << toString V.ringOpts << endl
     << toString V.poly << endl << toString V.n	<< endl << close )

loadVTree = method()
loadVTree(String) := filename -> (
     f := lines get filename;
     K := value f#0;
     opts := value f#1;
     A = K[opts];
     new HashTable from {
	  symbol coeff => K,
	  symbol ringOpts => opts,
	  symbol poly => value f#2,
	  symbol n => value f#3 
	  }
     )

search4ABigGuy := (V, I, parentNODE) -> (
     grandpa := (V.n#parentNODE).parentNODE;
     pInfo(3, {"grandpa: ideal =", I, " parent = ", parentNODE});
     if grandpa < 0 then NOTCOMPUTED
     else (
	  pInfo(3, "grandpa children: " | 
	       toString ((V.n#grandpa).children / (i->(i, isSubset((V.n#i).I, I)))));
	  temp := select(1,  (V.n#grandpa).children, 
	       i-> (i < parentNODE) and isSubset((V.n#i).I, I) ); 
	  if #temp > 0 then temp#0
	  else search4ABigGuy(V, I, grandpa)
	  ) 
     );

add2VTree = method() 
add2VTree(HashTable, Ideal, ZZ) := 
(V, II, p) -> (
     new HashTable from {
	  symbol coeff => V.coeff,
	  symbol ringOpts => V.ringOpts,
	  symbol poly => V.poly,
	  symbol n  => append(V.n, new HashTable from {
	       	    symbol I => II,
		    symbol Itype => (
			 if p < 0 then NOTCOMPUTED 
		    	 else search4ABigGuy(V, II, p)
			 ),
		    symbol level => if p < 0 then 0 else (V.n#p).level + 1,
		    symbol parentNODE => p
		    })  
	  } 
     );

modifyNODE = method()
modifyNODE(HashTable, ZZ, List) :=  
--(V, num, {bf, inv, children, t})
(V, num, l) -> (
     
     kidNumbers := l#2 / (J -> (
	       V = add2VTree(V, J, num);
	       #(V.n) - 1     -- the number of the last element
	       )); 
     new HashTable from {
	  symbol coeff => V.coeff,
	  symbol ringOpts => V.ringOpts,
	  symbol poly => V.poly,
	  symbol n  => take(V.n, num) | {new HashTable from {
	       	    symbol I => (V.n#num).I,
		    symbol Itype => COMPUTED,
		    symbol level => (V.n#num).level,
		    symbol bf => l#0,
		    symbol inv => l#1,
		    symbol children => kidNumbers,
		    symbol parentNODE => (V.n#num).parentNODE,
		    symbol t => l#3
	       	    }} | drop(V.n, num + 1)   
	  } 
     );

modifyCSET = method()
modifyCSET(HashTable, ZZ, List) := (V, num, l) -> (
     old := V.n#num;
     new HashTable from {
	  symbol coeff => V.coeff,
	  symbol ringOpts => V.ringOpts,
	  symbol poly => V.poly,
	  symbol n  => take(V.n, num) | {new HashTable from {
	       	    symbol I => old.I,
		    symbol Itype => old.Itype,
		    symbol level => old.level,
		    symbol bf => old.bf,
		    symbol inv => l,
		    symbol children => old.children,
		    symbol parentNODE => old.parentNODE,
		    symbol t => old.t
	       	    }} | drop(V.n, num + 1)   
	  } 
     );


cutCrapOut = method()
cutCrapOut(List) := l -> (
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

refineVTree = method()
refineVTree(HashTable, ZZ) := (V, num) -> (
     temp := V.n#num;
     if temp.Itype < 0 and #temp.children != 0 then (
	  scan(temp.children, u -> (
		    V = refineVTree(V, u);
	       	    temp2 := V.n#u;
		    if temp2.Itype < 0 and temp2.bf == temp.bf then (
		    	 << "patching node #" << num << endl;
			 p := position(temp.inv, v-> v == temp2.I);
			 if p===null then error "VTree is damaged!";
			 V = modifyCSET(V, num, drop(temp.inv, {p,p}) | temp2.inv);
			 temp = V.n#num;
			 );
	       ));
     	  V = modifyCSET(V, num, cutCrapOut(temp.inv));   
	  ); 
     V
     );

appendCSet = method()
appendCSet(List, HashTable) := (c, l) -> (
     i := 0;
     isDifferent := true;
     while isDifferent do (
	  if (c#i).V' == l.V' then isDifferent = false
	  else i = i + 1; 
	  );
     if isDifferent then take(c,i) | {l} | drop(c,i)
     else take(c,i) | {new HashTable from{
		    symbol V'' => cutCrapOut (l.V'' | (c#i).V''),
		    symbol V' => l.V'
		    }} | drop(c,i+1)
     );

VTree2bSet = method()
VTree2bSet(HashTable) := V -> (
     if any(V.n, u -> u.Itype == NOTCOMPUTED) then
     error "VTree is not complete"; 
     
     --V = refineVTree(V, 0);
     r := {};
     scan(V.n, node->( 
	       --if node.parentNODE < 0 or 
	       --(node.Itype == COMPUTED and (V.n#(node.parentNODE)).bf != node.bf) 
	       if node.Itype == COMPUTED then 
	       (
		    t := position(r, u -> u.bf == node.bf);
		    r = (
			 if t =!= null 
		    	 then take(r, t) | {
			      new HashTable from {
			      	   symbol bf => node.bf,
			      	   symbol cSet => appendCSet( (r#t).cSet, 
				       	new HashTable from{
					     symbol V'' => node.inv,
					     symbol V' => 
					     if node.I == 0 then ideal 0_(V.coeff)
					     else node.I  
					     })
			      	   }
			      } | drop (r, t+1)
		    	 else r | { new HashTable from {
				   symbol bf => node.bf,
			      	   symbol cSet => { new HashTable from{
					     symbol V'' => node.inv,
					     symbol V' => if node.I == 0 then ideal 0_(V.coeff)
					     else node.I
					     }} 
				   }}
			 ); 
     	       	    ))); 
     r
     ); 

chooseMin = method()
chooseMin(List) := l -> (
     select(l, u->(
	       all(l, v->(
			 (not isSubset(v,u) )
			 or v == u
			 ))
	       ))
     );
chooseMin(List, List) := (l1, l2) -> (
     chooseMin select(l1, u->any(l2, v-> isSubset(v,u)))
     );
 
VTree2bSet2 = method()
VTree2bSet2(HashTable) := V -> (
     if any(V.n, u -> u.Itype == NOTCOMPUTED) then
     error "VTree is not complete"; 
     
     -- for each b(s) \in B(n,d)
     -- make the list of ideals for which b(s) is a generic b-function 
     r := {};
     scan(V.n, node->( 
	       if node.Itype == COMPUTED then 
	       (
		    t := position(r, u -> u.bf == node.bf);
		    r = (
			 if t =!= null 
		    	 then take(r, t) | { 
			      new HashTable from {
			      	   symbol bf => node.bf,
			      	   symbol iList => (r#t).iList | {
					if node.I == 0 then ideal 0_(V.coeff)
			      		else node.I  
			      		}
				   }
			      } | drop (r, t+1)
		    	 else r | { 
			      new HashTable from {
			      	   symbol bf => node.bf,
			      	   symbol iList => {
					if node.I == 0 then ideal 0_(V.coeff)
			      		else node.I  
					} 
				   }
			      }
			 ); 
     	       	    )
	       ));      
     i := 0;
     while i < #r do (
	  l1 := (r#i).iList;
	  l2 := flatten(drop(r, {i,i}) / ( u -> u.iList));
	  c := { new HashTable from {
	       	    symbol V' => {"not empty"},
		    symbol V'' => {ideal 0_(V.coeff)} 
		    }};
	  while #((last c).V') > 0 do (
	       -- choose min ideals in arg1 containing at least one ideal from arg2 
	       temp := chooseMin(l1, (last c).V'');
	       c = c | { new HashTable from {
			 symbol V' => temp,
			 symbol V''=> chooseMin(l2, temp)
			 }}; 
	       );
     	  r = take(r,i) | {  new HashTable from {
			      	   symbol bf => (r#i).bf,
			      	   symbol iList => (r#i).iList,
				   symbol cSet => take(c,{1,#c-2}) 
				   } 
			      } | drop(r, i+1); 
     	  i = i + 1; 
	  );
     r
     ); 

isNonEmpty = method()
isNonEmpty(HashTable) := h -> ( #h.V'' == 0 or not isSubset( intersect(h.V''), h.V') );
     
-- returns (l,0) if nothing was done, (l,1) otherwise     
simplifyCSet = method()
simplifyCSet(List) := l -> (
     n := #l;
     i := 0;
     local j;
     p := null; 
     while p === null and i < n do (
	  j = 0;
	  while p === null and j < n do(
	       if (l#j).V' != 0 and i != j then p = position((l#i).V'', u -> u == (l#j).V');
	       --print(i,j,p);
	       if p === null then j = j + 1;
	       );
	  if p === null then i = i + 1; 
	  );
     if p === null then (l,0)
     else (
	  newCSets := {};
	  scan(drop((l#i).V'', {p,p}), u->(
		    tempL := decompose (u + (l#j).V');
		    scan(tempL, v->(
			      newCSets = newCSets | {
				   new HashTable from{
					symbol V'' => cutCrapOut flatten (
					     (l#j).V'' / (w ->  decompose (w + v))),
					     symbol V' => v 
					     }
				   };
			      ));
		    ));  
	  -- if any(newCSets,  u -> isNonEmpty u) then print "Eureka!"; 
	  -- modify l#i, throw in additional (nonempty) CSets
	  l = drop(drop(l, {i,i}), if i > j then {j,j} else {j-1,j-1}) | {
	       new HashTable from{
		    symbol V'' => cutCrapOut (drop((l#i).V'', {p,p}) | (l#j).V''),
		    symbol V' => (l#i).V'
		    }
	       };
	  scan(newCSets, u -> if isNonEmpty u then l = appendCSet(l,u));
	  (l,1)
	  )
     );

compressCSet = method()
compressCSet(List) := l -> (
     n := #l;
     i := 0;
     local j;
     p := false; 
     while not p and i < n do (
	  j = i + 1;
	  while not p and j < n do(
	       if (l#j).V' != 0 and (l#i).V' != 0 then p = ((l#i).V' == (l#j).V');
	       --print(i,j,p);
	       if not p then j = j + 1;
	       );
	  if not p then i = i + 1; 
	  );
     if not p then (l,0)
     else (
	  l = drop(drop(l, {j,j}), {i,i}) | {
	       new HashTable from{
		    symbol V'' => cutCrapOut ((l#i).V'' | (l#j).V''),
		    symbol V' => (l#i).V'
		    }
	       };
	  (l,1)
	  )
     );

ZmodP2Q = c -> (
     F1 := bigPrime//100;
     F2 := 100;
     
     b := bigPrime;
     i := 1;
     while (abs(i * c % b)) > F1 and i < F2 do i = i+1;
     j := 1;
     while (abs(j * (b-c) % b)) > F1 and j < F2 do j = j+1;
     if j < i then ((j * (b-c) % b) / (-j)) else ((i * c % b) / i) 
     );

ZmodP2Qpoly = f -> (
     --print ("F", f);
     if f == 0 then f else(
     	  A := QQ[options ring f];   
     	  sum(listForm f / (u -> (
			 ZmodP2Q(substitute(u#1, ZZ))
			 * A_(u#0))))
     	  )
     );
     
toString Sequence := s -> (
          if # s === 1 then concatenate("(",toString s#0,")")
          else concatenate("(",between(",",toString \ s),")")
          )

BSet2tex = method()
VSet2tex = method()
VSet2tex(List, File) := (l, TeXfile) -> (
     j := 0;
     while j < #l do(
	  vv := l#j;
	  TeXfile << "$V" 
	  << toString toSequence (first entries gens vv / ZmodP2Qpoly)
	  << (if j < #l - 1 then " \\cup $ " else "$");
	  j = j + 1;
	  );  
     );
BSet2tex(List, String) := (s, filename) -> (
     TeXfile := filename << "\\documentclass{article}\n\\begin{document}\n"; 
     s / (u -> ( 
	       TeXfile << "\n\n$$ b(s)=";
	       TeXfile << (
		    -- b-function
		    R := QQ[symbol S];
		    toString factorBFunction sum(u.bf, v->ZmodP2Q(v#1) * R_(v#0))
	       ) << ",$$";
	       
	       i := 0;
	       while i < #u.cSet do( 
		    VSet2tex((u.cSet#i).V', TeXfile);
	       	    if #(u.cSet#i).V'' > 0 then TeXfile << "$\\setminus$";
	       	    VSet2tex((u.cSet#i).V'', TeXfile);
	       	    if i < #u.cSet - 1 then TeXfile << "$\\cup$";
		    i = i + 1;
		    );	    
	       ));
     TeXfile << "\\end{document}" << close;
     );

------------------------------------------------------------------------------
-- globalBFunction adapted for b-1
------------------------------------------------------------------------------

getInv := GB -> (
     l := flatten entries getChangeMat GB;
     l = l / (u -> (entries (coefficients u)#1)#0);
     --<<"getInv: "<< GB << " =========> " << l << endl;
     l = (flatten l) / (u -> lift(u, coefficientRing ring GB));
     select(l / (u -> denominator u), u -> not isUnit u)
     ); 
-- a local function used by gbW and inW  

homGBparam := (I, w) -> (
     if ((ring I).?HomWeylAlgebra == false) then
     createHomWeylAlgebra (ring I);

     W := ring I;
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
     wts = prepend(-1,w);

     -- Homogenize I
     --print W.WAtoHWA;
     I1 := W.WAtoHWA I;
     --<< "homGB : I1 = " << I1 << endl;
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
     -- **********************%%%%%%%***************************
     --<< "inW: homo I = " << I2 << endl;
     GB := paramGB (I2, ChangeMatrix =>true);    
     --print("inW: ", gens GB, ring GB);
     I3 := leadTerm(1, gens GB);
     (ideal compress (ring I).HWAtoWA I3, getInv(GB)) 
     );

----------------------------------------------------------------------------
-- This function calls 2 versions of globalBFunction with parameters
BSWITCH = 1
CALLglobalBFunctionParam = method()
CALLglobalBFunctionParam(Thing) := a -> (
     if BSWITCH == 1 then globalBFunctionParam a
     else globalBFunctionParam2 a 
     );
---------------
 
makeMonic := f -> ( (1 / (leadCoefficient f)) * f );
ZZtoQQ := u -> (
     c := substitute(lift(u, ZZ/bigPrime), ZZ);
     b := bigPrime;
     i := 1;
     while (abs(i * c % b)) > 100 and i < 100 do i = i+1;
     ((i * c % b) / i) 
     );

ZZmakeQQ := f -> (
     Q := QQ[first entries vars ring f];
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
bFunctionParam = method()
bFunctionParam(Ideal, List) := (I, w) -> (
     if ((ring I).?ThetaRing == false) then
     createThetaRing (ring I);

     if ((ring I).ThetaRing.?IntRing == false) then
     createIntRing (ring I).ThetaRing;
     
     if ((ring I).?dpairVars == false) then
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
     -- compute in_(-w,w) (I) **************%%%%%%%%%%%*******************************  
     temp := inWparam(I, w); 
     inv := temp#1;
     inI := temp#0;
     --<< "inI = " << inI << endl;
     n := #(dpI#0);
     eulerOp := sum(n, i -> w_(dpI#1#i)*(dpV#0#i)*(dpV#1#i));
     	  
     (     
	  -- NON-GENERIC
	  dpI' := {select(dpI#0, i -> w#i != 0), select(dpI#1, i -> w#i != 0)};
	  dpI'' := {select(dpI#0, i -> w#i == 0), select(dpI#1, i -> w#i == 0)};
	  -- want: eliminate all u_i, v_i as well as x_i, dx_i of weight 0
	  u := symbol u;
	  v := symbol v;	  
	  UV := (coefficientRing W)[ (dpI'#0) / (i -> u_i), (dpI'#1) / (i -> v_i), 
	       (dpI''#0) / (i -> W_i), (dpI''#1) / (i -> W_i),
	       (dpI'#0) / (i -> W_i), (dpI'#1) / (i -> W_i),
	       WeylAlgebra => W.monoid.Options.WeylAlgebra,
	       MonomialSize => 16,
	       MonomialOrder => Eliminate (2 * #dpI#0) ];
	  WtoUV := map(UV, W, matrix { apply(numgens W, i -> (
			      if member(i, dpI'#0) then 
			      (u_i * substitute(W_i, UV))
			      else if member(i, dpI'#1) then (substitute(W_i, UV) * v_i)
			      else substitute(W_i, UV)
			      )) 
		    });
	  -- *******************************%%%%%%%%%%******************************
	  GB :=  paramGB (((WtoUV inI) 
	       	    + ideal apply(#dpI'#0, i -> (u_(dpI'#0#i) * v_(dpI'#1#i) - 1)
		    	 )), ChangeMatrix => true);
	  inv = inv | getInv(GB);
	  intGB := gens GB;
	  --<< "intGB = " << intGB << endl;
	  --print ring GB;
	  intIdeal = ideal substitute(selectInSubring(1, intGB), W);
	  --<< "intIdeal = " << intIdeal << endl;
	  );
     	  -- compute J = intIdeal \cap K[\theta]
     	  use T;
     	  genJ := (flatten entries gens intIdeal) / W.WtoT;
	  --<< "Ideal J = " << genJ << " ring = " << ring ideal genJ << endl;
	  elimIdeal := (T.RtoIR ideal genJ)
     	  + ideal(TI_(numgens TI - 1) - T.RtoIR W.WtoT eulerOp);
     	  -- ***********************%%%%%%%%***************************************
	  GB = paramGB(elimIdeal, ChangeMatrix => true);
	  inv = inv | getInv(GB);
	  elimIdealGB := gens GB;
	  --<< "elimIdealGB = " << elimIdealGB << endl;
	  --<< ring GB << endl;
	  -- take the generator of J and cook up the b-function
	  --<< "SSS ... " <<  selectInSubring(1,elimIdealGB) << endl;
	  bfcn := (mingens ideal selectInSubring(1,elimIdealGB))_(0,0);
	  --print ("b-function", bfcn);
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

globalBFunctionParam = method()
globalBFunctionParam(RingElement) := f -> (
     W := ring f;
     if (W.?dpairVars == false) then
     createDpairs W;
     dpI := W.dpairInds;
     
     -- sanity check
     if (#(W.dpairInds#2) != 0) then
     error "expected no central variables in Weyl algebra";
     if any(listForm f, m -> any(dpI#1, i -> m#0#i != 0)) then
     error "expected no differentials in the polynomial";
     
     t := symbol t;
     dt := symbol dt;     
     WT := (coefficientRing W)[ t, dt, (entries vars W)#0,
	       MonomialSize => 16,
	       WeylAlgebra => W.monoid.Options.WeylAlgebra | {t => dt}];
     w := {1} | toList (((numgens W) // 2):0);
     f' := substitute(f,WT);
     If := ideal ({t - f'} 
     	  | (dpI#1 / (i->(
	       	    	 DX := WT_(i+2);
	       	    	 (DX * f' - f' * DX) * dt + DX
	       	    	 )))
	  );
     --<< "If = " << If << endl;
     temp := bFunctionParam(If, w);
     bfunc := temp#0;
     inv := temp#1;
     s := (ring bfunc)_0;
     (makeMonic substitute(bfunc, { s => -s - 1 }), inv)
     );--end of globalbFunctionParam

-------------------------------------------------------------
-- 2nd version of globalB with parameters 
--------------------------------------------------------------
AnnFsParam = method()
AnnFsParam(RingElement) := f -> (
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
     WT := (coefficientRing W)[ t, dt, (entries vars W)#0, 
	  WeylAlgebra => WAopts,
	  MonomialOrder => Eliminate 2 ];
     u := symbol u;
     v := symbol v;
     WTUV := (coefficientRing W)[ u, v, t, dt, (entries vars W)#0,
	  WeylAlgebra => WAopts,
	  MonomialOrder => Eliminate 2 ];
     WtoWTUV := map(WTUV, W, (vars WTUV)_{4..n+3});
     -- twist generators of I into generators of KI
     f' := substitute(f,WTUV);
     twistList := apply( toList(3..n+3), 
	  i -> WTUV_i + (WTUV_i*f' - f'*WTUV_i)*dt);
     twistMap := map(WTUV, WTUV, matrix{{u,v,t-f'}|twistList});
     tempKI := twistMap(ideal t + WtoWTUV I);
     wts := {1,-1,1,-1} | toList(n:0);
     KI = ideal homogenize(gens tempKI, u, wts);
     
     g := (entries gens KI)#0 | { u * v - 1 };
     GB := paramGB(ideal g);
     inv := getInv GB;
     preGens := flatten entries substitute(
	  selectInSubring(1, gens GB), WT);
     use WT;
     s := symbol s;
     WS := (coefficientRing W)[(entries vars W)#0, s,
	  WeylAlgebra => W.monoid.Options.WeylAlgebra];
     WTtoWS := g -> (
	  e := exponents leadMonomial g;
	  if e#0 > e#1 then g = dt^(e#0-e#1) * g
	  else g = t^(e#1-e#0) * g;
	  g' := 0_WS;
	  while (d := exponents leadMonomial g; d#0 * d#1 != 0) do(
	       c := leadCoefficient g;
	       g' = g' + c * (-s-1)^(d#1) * WS_(drop(d, 2) | {0}); -- >%-0	
	       g = g - c * (t*dt)^(d#1) * WT_({0,0} | drop(d, 2));
	       ); 
	  g' + substitute(g, WS)
	  );
     use W;
     (ideal (preGens / WTtoWS), inv) 
     )

globalBFunctionParam2 = method()
globalBFunctionParam2(RingElement) := f -> (
     W := ring f;
     
     temp := AnnFsParam f;
     AnnI := temp#0;
     inv := temp#1;
     
     Ws := ring AnnI;
     ns := numgens Ws;
     
     
     elimWs := (coefficientRing Ws)[(entries vars Ws)#0,
	  WeylAlgebra => Ws.monoid.Options.WeylAlgebra,
	  MonomialOrder => Eliminate (ns-1)];
     ff := substitute(f,elimWs);
     elimAnnI := substitute(AnnI, elimWs);
     H := (gens elimAnnI) | matrix{{ff}};
     
     gbH := paramGB(H, ChangeMatrix => true);
     inv = inv | getInv gbH; 
     
     bpolys := selectInSubring(1, gens gbH);
     if (bpolys == 0) then error "module not specializable";
     if (rank source bpolys > 1) then error "ideal principal but not
     realized as such.  Need better implementation";
     bpoly := bpolys_(0,0);
     
     Ks = (coefficientRing W)[Ws_(ns-1)];
     bpoly = substitute(bpoly, Ks);
     use W;
     (bpoly, inv)
     )





















