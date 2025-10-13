newPackage(
	"IncidenceCorrespondenceCohomology",
    	Version => "0.1", 
    	Date => "February 2, 2024",
    	Authors => {
	    {Name => "Annet Kyomuhangi", 
		  Email => "annet.kyomuhangi@gmail.com", 
	HomePage=>"https://people.busitema.ac.ug/staff/academic-staff/annet-kyomuhangi"}, 
	    {Name => "Emanuela Marangone", 
		  Email => "emanuela.marangone@umanitoba.ca", 
		  HomePage => "https://emanuelamarangone.wixsite.com/marangone"}, 
	    {Name => "Claudiu Raicu", 
		  Email => "craicu@nd.edu", 
		  HomePage => "https://www3.nd.edu/~craicu/"}, 
	    {Name => "Ethan Reed",
		  Email => "ereed4@nd.edu", 
		  HomePage => "https://sites.google.com/view/ethan-reed/home"}
	    },
    	Headline => "Cohomology on the incidence correspondence, bundle of principal parts, and Lefschetz properties",
	Keywords => {"Algebraic Geometry", "Commutative Algebra", "Flag Varieties"}
	--Certification => {
	  --   "journal name" => "",
	    -- "journal URI" => "",
	     --"article title" => "",
	    -- "acceptance date" => "",
	    -- "published article URI" => "",
	    -- "published article DOI" => "",
	    -- "published code URI" => "",
	    -- "release at publication" => "",	    
	    -- "version at publication" => "",
	    -- "volume number" => "",
	    -- "volume URI" => ""
	     --}
    	)


export {--option 
"UseConjecture",
"FindCharacter",
"Multidegree",
"GorensteinAlg",
"MonomialAlg",
--methods and functions
"splittingFdr",
"splittingPrincipalParts", 
"hanMonsky",
"hasWLP",
"monomialCIsWithoutWLP",
"hasSLP",
"recursiveDividedCohomology",
"nimDividedCohomology", 
"incidenceCohomology"}


---------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------
--Torus equivariant splitting of duals of bundles of principal parts on P^1
---------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------


--Input:
--prime p and integer n
--Output:
--smallest integer e such that n < p^e

boundq = (p, n) ->(
    counter := 0; q := 1;
    while n >= q do (counter = counter + 1; q = p*q);
    counter
    )

--Input:
--a list of integers
--Output:
--the list with the integers shifted by -n

singleshiftn = (list1, n) ->(for i from 0 to (#list1-1) list (list1#i-n))
    

--Input:
--a list of lists of three integers list3 and an integer n
--in the list, the first entry of each entry is an internal degree and the second and third entries of each entry are multidegrees
--Output:
--the list with the first entries (internal degrees) shifted by -n
--This function will be used in multidegreeSplit() to adjust factors due to tensoring by O(-n)

shiftn = (list3, n) ->(for i from 0 to (#list3-1) list ({list3#i#0-n, list3#i#1, list3#i#2}))
    

--Input: 
--list3 a list of lists of three integers, a power of a prime q = p^d, and integers a,det1
--in list3, the first entry of each entry is an internal degree and the second and third entries of each entry are multidegrees
--Output:
--the list obtained by adjusting the multidegrees by tensoring with F^q(D^aU)\otimes det(U)^det1

tensorfqa = (list3, q, a, det1) ->(
    --list of divided multidegrees
    dividedp := compositions(2, a);
    --list of frobenius multidegrees of D^aU
    fqa := for i from 0 to a list ({dividedp#i#0*q, dividedp#i#1*q});
    --this will be the new list to be returned
    newlist := flatten for i from 0 to (#fqa -1) list(
        for j from 0 to #list3-1 list ({list3#j#0, list3#j#1+fqa#i#0+det1, list3#j#2+fqa#i#1+det1})
	);
    newlist
    )

--Input:
--prime p (or p = 0), and integers d,r
--Optional Input
--Multidegrees boolean value defaulted to false
--Output: 
--the splitting type as a direct sum of line bundles for the bundle F_r^d over a field of characteristic p as introduced in [KMRR,24] 
--If Multidegrees is false, then the output is a list of the twists that appear for the splitting type
--If Multidegrees is true, then the output is a list whose entries are lists of three integers
--the first entry in each entry is the corresponding twist for the line bundle
--the second and third entry in each entry are the multidegrees arising from a torus action upon choosing coordinates for P^1
--This function implements the recursive description given in Theorem 3.2
splittingFdr = method(Options => {Multidegree => false})

splittingFdr(ZZ,ZZ,ZZ) := opts -> (p,d,r) ->(
    if opts.Multidegree then (return multidegreesplit(p,d,r))
    else (return splittingFdr2(p,d,r))
    )

--Input:
--prime p (or p = 0), and integers d,r
--Output:
--the splitting type as a direct sum of line bundles of the bundles F^d_r over a field of characteristic p
--the output is a list of the twists that appear in the splitting type

splittingFdr2 = (p,d,r) ->(
    --In characteristic 0, if d<=r the bundle is a direct sum of r copies of O(-(d-r+1))
    --If r>d, then the bundle is a direct sum of d+1 copies of O(0)
    if (p==0) then(
	if (r<=d) then( return  (for i from 1 to r list (-(d-r+1))));
	if (r>d) then( return  (for i from 0 to d list 0))
	);
    --In this case the bundle is 0, so the splitting type is empty
    if (r<=0) then return {};
    --In this case the bundle is D^dU \otimes O
    if (d<r) then(return (for i from 0 to d list 0));
    --In this case the splitting agrees with the characteristic 0 case
    if (d<p) then return (for i from 0 to r-1 list (-(d-r+1)));
    --The rest of the function is using Theorem 3.2
    --We first compute some relevant quantities
    e := boundq(p,r);
    q := p^e;
    q' := p^(e-1);
    --These are two of the remaining three remaining cases of the theorem
    if (d >= (q+r)) then (return singleshiftn(splittingFdr2(p, d-q, r), q));
    if (d >= (q)) then (return join((for i from q to d list -q),(for i from (d-r+1) to (q-1) list (r-q))));
    --This case is no longer in the theorem but can be derived from cases 2 and 4
    if (d >= q'+r) then return(singleshiftn(splittingFdr(p, d-q',r), q'));
    --The remaining case is more complicated
    a := r//q';
    r2 := (a+1)*q'-r;
    q2 := p^(boundq(p, r2));
    part1 := splittingFdr2(p, (d-a*q'), (r-a*q'));
    part2 := singleshiftn(splittingFdr2(p, (d-r+q2), r2), r2-q2);
    return flatten(join((for i from 1 to (a+1) list part1),(for i from 1 to a list part2)))
    )


--Input:
-- prime p(or p = 0), and integers d,r
--Output:
--the splitting type as a direct sum of line bundles for the bundles F^d_r over a field of characteristic p
--the output is a list whose entries are lists of three integers
--the first entry in each entry is the corresponding twist for the line bundle
--the second and third entry in each entry are the multidegrees arising from a torus action upon choosing coordinates for P^1
--This function implements the recursive description given in Theorem 3.2

multidegreesplit = (p, d, r) ->(
    --In characteristic 0,  Fdr = det(U) tensor (Sym^(b-1)U) tensor O(-(d-r+1)) or if r>d is just D^dU(0)
    if p==0 then(
	if (r<= d) then return (for i from 0 to (r-1) list {-(d-r+1), (d-i), (d-r+1+i)});
	if (r > d) then return (for i from 0 to d list {0, d-i, i})
	);
    --The remaining code is for char k = p>0
    --declare lists locally
    list1 := {};
    list2 := {};
    --these two cases are the base case for the recursion
    if(r<=0)then return {};
    if (d< r)then( 
	list1 = compositions(2,d);
	return (for i from 0 to #list1-1 list {0, list1#i#0, list1#i#1});
	);
    if (d<p)then return for i from 0 to r-1 list ({-(d-r+1),d-i,d-(r-1-i)});
    --the other cases depend on q' and q
    --where q'<=r<q
    e := boundq(p, r);
    q := p^e;
    q' := p^(e-1);
    if (d >= (q+r)) then (
	--print "case A";
	list1 = multidegreesplit(p, d-q, r);
	return shiftn(tensorfqa(list1, 0,0, q), q) 
	);
    --this case is not recursive
    if (d >= q) then (
	--print "case B";
	list1 = shiftn(tensorfqa({{0,0,0}}, 1, d-q, q), q);
	list2 = shiftn(tensorfqa({{0,0,0}}, 1, r-2-(d-q), d-r+1),q-r);
	return join(list1, list2)
	);
    if (d >= q'+r) then (
	--print "Case c";
	list1 = multidegreesplit(p, d-q', r);
	return shiftn(tensorfqa(list1, 0, 0, q'), q')
	);
    --Otherwise r<=d<=q'+r-1
    --this is for the final case for which we also need a such that
    -- aq' <= r <= (a+1)q'
    a := r//q';
    r2 := (a+1)*q'-r;
    q2 := p^(boundq(p, r2));
    list1 = multidegreesplit(p, (d-a*q'), (r-a*q'));
    list1 = shiftn(tensorfqa(list1,q',a,0),0);
    list2 = tensorfqa((multidegreesplit(p, (d-r+q2), ((a+1)*q'-r))), q', (a-1), q'-q2);
    list2 = shiftn(list2,((a+1)*q'-r-q2));
    return join(list1, list2)
    )


--Input:
--prime p, and integers m,k
--Optional Input
--Multidegrees boolean value defaulted to false
--Output: 
--the splitting type as a direct sum of line bundles for the bundle of principal parts P^k(O(m))
--over P^1 over a field of characteristic p 
--If Multidegrees is false, then the output is a list of the twists that appear for the splitting type
--If Multidegrees is true, then the output is a list whose entries are lists of three integers
--the first entry in each entry is the corresponding twist for the line bundle
--the second and third entry in each entry are the multidegrees arising from a torus action upon choosing coordinates for P^1
--This function is based off the following description of P^k(O(m)) in terms of Fdr
--If m >=k+1, then P^k(O(m)) = (F^m_{k+1})^*\otimes L_{m,m}
--If -1 <= m <= k, then P^k(O(m))= Sum_{i=0}^m(L_{i,m-i}(0))+Sum_{i=0}^{k-1-m}(L_{m+1+i, k-i}(-k-1))
--If m<=-2, then P^k(O(m)) = F^{k-1-m}_{k+1}\otimes L_{m+1, m+1}(-k-1)
splittingPrincipalParts = method(Options => {Multidegree => false})

splittingPrincipalParts(ZZ,ZZ,ZZ) := opts -> (p,m,k)->(
    list1:= {};
    if not opts.Multidegree then(
	if (m>= (k+1)) then(
	    list1 = splittingFdr2(p,m,k+1);
	    return (for i from 0 to (#list1-1) list -list1#i) 
	    );
	if ((m<=k)and(m>=-1)) then(
	    return join((for i from 0 to m list 0),(for i from 1 to (k-m) list (-k-1)))
	    );
	if (m<=-2) then(
	    list1 = splittingFdr2(p, (k-1-m),(k+1));
	    return (for i from 0 to (#list1-1) list (list1#i -k -1))
	    )
	);
    if opts.Multidegree then(
	if (m>=(k+1)) then(
	    list1 = multidegreesplit(p, m, k+1);
	    return (for i from 0 to (#list1-1) list ({-list1#i#0, (-list1#i#1 + m),(-list1#i#2 + m)}))
	    );
	if ((m<=k) and (m>=-1)) then (
	    return join((for i from 0 to m list ({0, i, m-i})), (for i from 0 to (k-1-m) list ({(-k-1), m+1+i, k-i})))
	    );
	if (m<=-2) then(
	    list1 =  multidegreesplit(p, (k-1-m), (k+1));
	    return (for i from 0 to (#list1-1) list ({list1#i#0-k-1, list1#i#1 +m+1 , list1#i#2 +m+1}))
	   )
	)
    )


-- The following code is used to test the correctness of splittingFdr  by computing the splitting type directly

--Input:
-- a list of integers {d_1, ..., d_k}
--Output:
--returns multinomial Sum(d_i) choose {d_1,...,d_k}
--This function also exists in the Schpect Module Package but is redefined here

multi = (numblist)->(
    n := sum(numblist);
    N := n!;
    for i from 0 to #numblist - 1 do(
	 N = N/((numblist#i)!); 
	 );
    return N;
    )
 
--This function compares two compositions, and returns true if all of the entries of  
--list1 are greater than or equal to the entries in list2
--Input:
--List of integers of the same length
--Output: 
--Boolean true or false

Compare = (list1, list2) ->(
    bool := true;
    if (#list1 == #list2)
    then for i from 0 to #list1-1 do (if list2#i>list1#i then bool = false)
    else bool = false;
    bool
    )

---Input:
--A polynomial ring S=k[x_1..x_n]=Sym(V) and integers d,r.
---Output:
--The map (as a matrix) of free modules D^dV\otimes S to D^(d-r)V \otimes S(r)
--given by comultiplication and the natural map from divided to symmetric powers

Comult = (S,d,r) -> (
    --This set indexes the basis for D^dV
    comps1 := compositions(# gens S, d);
    --This set indexes the basis for D^(d-r)V
    comps2 := compositions(# gens S, d-r);
    --This is the function which will be used to define the matrix
    fun := (i, j) -> (
	if (Compare(comps1#j, comps2#i))
	--The multinomial coefficient appears from the natural map from divided to symmetric powers
	--The other part of the code is just to build the correct monomial
	then multi(for k from 0 to ((# gens S)-1) list comps1#j#k-comps2#i#k)*product(for k from 0 to ((# gens S)-1) list ((gens S)#k)^((comps1#j#k)-(comps2#i#k)))
	else 0 
	);
    map(S^#comps2, S^#comps1, fun)
    )

--Input:
--A polynomial ring S=k[x_1..x_n]=Sym(V) and integers d,r
--Output:
--Fdr the kernel of the comultiplication map D^dV\otimes S to D^(d-r)V\otimes S(r)
--given by comultiplication and the natural map from divided to symmetric powers

Fdr = (S, d, r)->(
    return ker(Comult(S, d, r))
    )

--Input:
--Integers d,r
--Output:
--multidegrees for D^dU(r) = {{r,d,0}, {r,d-1,1},...,{r,0,d}}

DdU = (d,r)->(
    for i from 0 to d list ({r,d-i, i})
    )

--Input:
--Multigraded polynomial ring S = k[x,y, Degrees=> {{1,-1,0},{1,0,-1}}] = Sym(U) and integers d,r
--Output: multigraded module Fdr the kernel of the comultiplication map D^dV\otimes S to D^(d-r)V\otimes S(r)
--given by comultiplication and the natural map from divided to symmetric powers

gradedFdr = (S, d, r) ->(
    --get matrix from comult
    mat := Comult(S,d,r);
    deg1 := DdU(d, 0);  --compositions(d,2);
    deg2 := DdU(d-r, r);  --compositions(d-r,2);
    return ker (map(S^deg2, S^deg1, mat))
    )


---------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------
-- Multiplication in the graded Han-Monsky representation ring in positive characteristic
---------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------

--auxiliary Laurent polynomial ring
--keeps track of shifts and multiplicities for standard generators
--of the Han-Monsky reprezentation ring
q := local q;
HMRing = ZZ[q,Inverses => true, MonomialOrder=>Lex]
--if HMRing is a terrible name, we should replace it, ideas?
--also, we may want to export HMRing so people can access it directly and make their own hash tables
--with values in this ring?


--auxiliary function
--input: integers (a,b,p,j) with a<=b, 0<=j<a, and p prime 
--output: the c_j such that \delta_c[-j] is a summand of \delta_a*\delta_b in ch= p using a Conjecture 

cj = (a, b, p, j) ->(
	e := boundq(p, a-1); q := p^e;
	r := ((a+b-1-j)//q);
	if (b-j <= r*q) then return r*q;
	--Recursive case 
	q' := p^(e-1); m := (a//q');
	a' := (a - m*q'); i := (j//q');
	if (j <= ((i*q')+a'-1)) then return cj(a', b + (m - (2*i))*q', p, j -i*q')
	else return cj((q'-a'), (b+(m-1-(2*i))*q'), p, (j-(i*q')-a'))
	)


--auxiliary function:  calculates the multiplication \delta_a*\delta_b in the Han-Monsky representation ring.
--input: integers (p,a,b) with p prime 
--output: the list of c_j's such that \delta_c[-j] is a summand of \delta_a*\delta_b in characteristic p

hanMonskyPair  = method(Options => {UseConjecture => true})

hanMonskyPair(ZZ,ZZ,ZZ) := opts -> (p,a,b) -> (
    aux := min(a,b); b = max(a,b); a = aux; --make a be the smallest of a,b
    if p == 0 then
    (
	for j from 0 to a-1 list a+b-1-2*j
	)
    else if opts.UseConjecture then
    (
	for j from 0 to a-1 list cj(a, b, p, j)
	)
    else
    (
	x := symbol x;
	y := symbol y;
	S := ZZ/p[x, y];
	I := ideal(x^a, y^b);
	i := 1; len := a; l := x+y;
	sum while len > 0 list
	(
	    lis := splice{len:1} | splice{a-len:0};
	    len = length((I + l^i)/(I + l^(i+1)));
	    i = i+1;
	    lis
	    )
	)
    )

--auxiliary function:  calculates the multiplication coe*\delta_a*H in the Han-Monsky representation ring.
--input: integers (p,a,H,coe) with p prime, a positive integer,
--H a HashTable representing an element in the Han-Monsky ring
--coe an element in HMRing encoding the number of copies of \delta_a and their degree shift
--output: Hash Table representing the product coe * \delta_a * H

hanMonskyPair(ZZ,ZZ,HashTable,RingElement) := opts -> (p,a,H,coe) -> (
    prod := new MutableHashTable from {};
    for b in keys H do (
	cjs := hanMonskyPair(p,a,b,opts);
	for j from 0 to #cjs-1 do
	(
	    c := cjs#j;
	    if prod#?c then prod#c = prod#c + coe * H#b * HMRing_{j} else
		prod#c = coe * H#b * HMRing_{j};
		);
	);
    new HashTable from prod
    )

--auxiliary function:  calculates the multiplication \delta_a*H in the Han-Monsky representation ring.
--input: integers (p,a,H) with p prime, a positive integer,
--H a HashTable representing an element in the Han-Monsky ring
--output: Hash Table representing the product \delta_a * H

hanMonskyPair(ZZ,ZZ,HashTable) := opts -> (p,a,H) -> hanMonskyPair(p,a,H,1_HMRing,opts)

--auxiliary function:  calculates the multiplication H1*H2 in the Han-Monsky representation ring.
--input: integers (p,H1,H2) with p prime, a positive integer,
--H1,H2 HashTables representing two elements in the Han-Monsky ring
--output: Hash Table representing the product H1*H2

hanMonskyPair(ZZ,HashTable,HashTable) := opts -> (p,H1,H2) -> (
    prod := new MutableHashTable from {};
    for a in keys H1 do
    (
	aH2 := hanMonskyPair(p,a,H2,H1#a);
	for c in keys aH2 do
	    if prod#?c then prod#c = prod#c + aH2#c else prod#c = aH2#c;
	);
    new HashTable from prod
    )

--user function:  this function computes the multiplication \delta_{a_1}*\delta_{a_2}*....*\delta_{a_n} in the graded Han-Monsky representation ring, in ch=p
--equivalently this function gives the Jordan degree type of the monomial complete intersection (ZZ/p)[x_1,...,x_n}/(x_1^{a_1}, …, x_n^{a_n}) with respect to the linear element x_1+...+x_n
--input: (p, L) p prime and L={a_1,a_2 ... a_n} list of integers 
--output:Hash Table representing the product  \delta_{a_1}*\delta_{a_2}*....*\delta_{a_n} 
--option "UseConjecture => false" to not use Conjecture 4.1 [KMRR,25]

hanMonsky = method(Options => {UseConjecture => true})
hanMonsky(ZZ, List) := opts -> (p, L) -> (
    if #L == 0 then return new HashTable from {1 => 1_HMRing} else
	hanMonskyPair(p,L#0,hanMonsky(p,drop(L,1)),opts)
    )

   
-----------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
-- Lefschetz properties for monomial complete intersections in positive characteristic
----------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------


--auxiliary function
--Input: a list of integers L={a_1,...,a_n}
--Output: smallest integer k such that 2^{k+1} is bigger then the nim sum of a_1 ... a_n


boundNim = (L) ->(
    m := max L;
    counter := boundq(2,m)-1;
    a := sum for i from 0 to #L-1 list L#i//(2^counter);
    while a%2 == 0 and counter>0  do (
	counter = counter - 1; 
	a = sum for i from 0 to #L-1 list L#i//(2^counter)
	);
    counter
    )

--auxiliary function
--Input: integers q and x
--Output: the reminder r of x divided by 2q if this is less then q, -infinity if r=2q-1, and 2q-2-r otherwise

theta = (q,x) -> (
    r := x%(2*q);
    if r<q then r
       else if r<2*q-1 then 2*q-2-r
       else -infinity
)

--------------------------------------------------------
---- Weak Lefschetz Property (WLP)----------------------
--------------------------------------------------------

--auxiliary function
--Input: prime p, integer a representing element \delta_a, hash table H representing element in Han-Monsky ring
--Output: hash table representing for each summand \delta_c the lowest shift j such that
--        \delta_c(-j) appears in the product \delta_a * H

hmMinPair = (p,a,H) -> (
    prod := new MutableHashTable from {};
    for b in keys H do (
	cjs := hanMonskyPair(p,a,b);
	for j from 0 to #cjs-1 do
	(
	    c := cjs#j;
	    if prod#?c then prod#c = min(prod#c,j + H#b) else
		prod#c = j + H#b;
		);
	);
    new HashTable from prod
    )

--auxiliary function
--Input: prime p, list L of integers a indexing generators \delta_a of the Han-Monsky ring
--Output: hash table representing for each summand \delta_c the lowest shift j such that
--        \delta_c(-j) appears in the product \delta_a indexed by a in L


hmMinShifts = (p, L) -> (
    if #L == 0 then return new HashTable from {1 => 0} else
	hmMinPair(p,L#0,hmMinShifts(p,drop(L,1)))
    )


--auxiliary function
--Input: prime p, list L of positive integers a_1,...,a_n
--Output: checks WLP for k[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n}) using the product in the Han-Monsky ring

hmWLP = (p,L) -> (
    H := hmMinShifts(p,L);
    s:=sum(L)-(#L);
    for r in keys H do
	if r+H#r-1 < s/2 then return false;
    return true
    )

--auxiliary function
--Input: list L of positive integers a_1,...,a_n
--Output: checks WLP for Z/2[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n}) using Theorem 8.1 [KMRR,24]

char2WLP = (L) -> (
    L = for i from 0 to #L-1 list L#i-1;
    m := max L;
    if m==0 then return true 
       else (
	   for k from boundNim(L) to  boundq(2,2*m)-1 do (
    	       q := 2^k;
    	       a := sum for i from 0 to #L-1 list theta(q, L#i);
    	       if a>2*q-2 then return false);
	   return true
	   )
    )

--auxiliary function
--Input: prime p, list L of positive integers a_1,...,a_n
--Output: checks WLP for k[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n}) using the Sperner number

directWLP = (p,L) -> (
    n:= #L; s := sum(L)-n;
    x := symbol x;
    T := (ZZ/p)[x_0..x_(n-1)];
    L1 := for i to n-1 list x_i^(L#i);
    A := T/ideal(L1);--A is the artinian CI k[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n})
    Sperner :=  hilbertFunction(floor(s/2),A);-- Compute the Sperner number, i.e. the maximum value of the Hilbert function of A
    l := sum for i to n-1 list x_i;		     
    L2 := append(L1,l);
    B := T/ideal(L2); -- B=A/lA
    degree B == Sperner -- Check if the dimension of B as k-vector space is equal to the Sperner number of A
    )





--------------------------------------------------------------------------------------------------------------------------------------

--method to check if a graded Artinian algebra has the Weak Lefschetz property 

hasWLP = method( Options => {UseConjecture => true, GorensteinAlg => false, MonomialAlg => false})


--user function: this function checks if the monomial complete intersection (ZZ/p)[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n}) has the Weak Lefschetz property,
--useing the multiplication in the Han-Monsky representation ring
--input: (p, L) p prime and L={a_1,a_2 ... a_n} list of integers 
--output: true if  k[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n}) has the WLP in characteristic p, false otherwise
--use the option "UseConjecture => false" to not use Conjecture 4.1 [KMRR,25] and check the WLP without computing the Han-Monsky multiplication


hasWLP(ZZ, List) := opts -> (p, L) -> (
	 if #L <= 2 or p == 0 then return true --up to 2 variables and in char 0 the WLP always holds
	 else if p == 2 then char2WLP(L) --the case p=2 is treated separately, using  Theorem 8.1 [KMRR,24]
	     else (
	     	 if not(opts.UseConjecture) then directWLP(p,L) --check WLP using the Sperner number
	     	 else hmWLP(p,L) --check WLP using the Han-Monsky multiplication
	     	 )
	     )


--user function: this function checks if the graded Artinian Algebra R/I, where R=kk[x_1...x_n] with kk infinite field, has the Weak Lefschetz property 
--input: input a (standard graded) polynomial ring R and an homogeneous ideal I such that R/I is Artinian
--output: true if R/I has the Weak Lefschetz Property, false otherwise 


  hasWLP(PolynomialRing, Ideal) := opts ->  (R,I) ->( L:=flatten entries super basis(1, R);
       	 n:=#L;
	 A:=R/I;
	 if not dim (R/I)==0 then error "expected an Artinian Algebra";
	 e:=0; hf:=hilbertFunction(e,A); HF:={};
     	 while hf>0 do ( HF=HF|{hf}; e=e+1; hf=hilbertFunction(e,A));
	 e=e-1;--e is now the socle degree and for i from 0 to e HF#i==hilbertFunction(e,A)
	 Sperner :=  max HF; --max value of the Hilbert function of A
     if opts.MonomialAlg then( 
	 --for monomial CI it is enough to check if the sum of the variable is a Lefschetz element
	 B:= R/ideal(I,sum for i to n-1 list L#i);
	 return degree B == Sperner)
     else(
	 indx:=0;
	 while HF#indx<=HF#(indx+1) do indx=indx+1;--increasing part of the Hilbert function
	 while indx<e do if HF#indx<HF#(indx+1) then return false else indx=indx+1;--return false if the Hilbert function is not unimodal 
    	 a:= symbol a;
    	 T:=A[a_1..a_n];
    	 X:= for i from 0 to n-1 list sub(L#(i-1),T);
    	 line:= sum (for i from 1 to n list a_i*X#(i-1));
    	 m:=sub(ideal(for i from 1 to n list X#(i-1)),T);
    	 if opts.GorensteinAlg then (
	     --for a Gorenstein, it is enough to check that there exists a linear l form such that the multiplication has max rank in the middle degree xl:A_(floor(e/2)->A_(floor(e/2)+1)
	     lineScaled:= mingens m^(floor(e/2))*line;
	     VarsScaled:= mingens m^(floor(e/2)+1);
	     M:=lineScaled//VarsScaled; 
	     d:=min(HF#(floor(e/2)),HF#(floor(e/2)+1));
 	     return rank M>= d;)
     	 else (
	      degreeSperner:=0;
	      while HF#degreeSperner<Sperner do degreeSperner = degreeSperner+1;--degree for which the Hilbert function reaches maximum value
	      mA:=ideal(for i from 0 to n-1 list sub(L#i,A));--the maximal homogeneous ideal of A
	      socleA:= ann mA;
	      indexAnn:=0;
	      while  (HF#indexAnn-hilbertFunction(indexAnn, socleA))==0 do indexAnn=indexAnn+1;--annindex is the first degree where SocleA is non-zero, annindex<=e (for A Gorenstein they are equal)
	      l:=min((indexAnn-1), degreeSperner); -- multiplication index
    	      j:=0;-- flag
     	      while (j<1 and l<degreeSperner+1) do (
		  lineScaled1:= mingens m^(l)*line;
	 	  VarsScaled1:= mingens m^(l+1);
	 	  M1:=lineScaled1//VarsScaled1;
	 	  d1:=min(HF#l,HF#(l+1));
	 	  if  rank M1<d1 then j=1 else l=l+1); 
     	     return j==0;)))




-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- auxiliary function
-- Input: integers n,s 
-- output:list of n-tuples (a_1,....,a_n) such that a_1+.......+a_n-n=s and 2<=a_1<= ....<= a_n


nTuples = (n, s) -> (
    if n == 1 then {{s + 1}}
    else flatten apply(toList(2..(s+n)//n), i -> (
	    subTuples := select(nTuples(n - 1, s - i+1), x -> x#0>=i); -- recursively generate (n-1)-tuples that sum to (s-i +n) and that the fist component is not less then i
	    apply(subTuples, x -> {i}|x)) -- append i as the first component to each sub-tuple
	))


-- user function:  this function lists all the monomial CI of socle degree s that fail the WLP 
-- Input: integers n,s 
-- Output: list of  n-tuples (a_1,....,a_n) such that a_1+.......+a_n-n=s and 2<=a_1<= ....<= a_n
-- such that k[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n}) does not have the WLP

monomialCIsWithoutWLP = method(Options => {UseConjecture => true})

monomialCIsWithoutWLP(ZZ, ZZ, ZZ) := opts -> (p, n, s) -> (
    L:=nTuples(n,s);--list of n-tuples (a_1,....,a_n) such that a_1+.......+a_n-n=s and 2<=a_1<= ....<= a_n
    select(L, x -> not hasWLP(p, x,opts))
)




--------------------------------------------------------
---- Strong Lefschetz Property (SLP)----------------------
-------------------------------------------------------

--auxiliary function
--Input: prime p, integer a representing element \delta_a, hash table H representing a sum of \delta_b's
--Output: false if the product \delta_a * H is different from the one in characteristic 0
--        otherwise a hash table with keys indexed by positive integers c
--        such that \delta_c appears (with some shift) in the product \delta_a * H

SLPair = (p,a,H) -> (
    prod := new MutableHashTable from {};
    for b in keys H do (
	cjs := hanMonskyPair(p,a,b);
	for j from 0 to #cjs-1 do
	(
	    c := cjs#j;
	    if c != a+b-1-2*j then return false;
	    if not prod#?c then prod#c = 0;
	    );
	);
    new HashTable from prod
    )

--auxiliary function
--Input: prime p, list L of integers a indexing generators \delta_a of the Han-Monsky ring
--Output: false if the product \delta_a indexed by a in L is different from the one in characteristic 0
--        otherwise hash table with keys indexed by positive integers c
--        such that \delta_c appears (with some shift) in the product \delta_a indexed by a in L

recSLP = (p,L) -> (
    if #L == 0 then new HashTable from {1 => 0} else
    (
       H := recSLP(p,drop(L,1));
       if H === false then false else SLPair(p,L#0,H)
       )
)

--auxiliary function, to check if  k[x,y]/(x^a y^b) using Theorem 3.2 [Nicklasson17]
--input:  prime p and  integers a and b with a,b>=p and p>2
--output:  true if  k[x,y]/(x^a y^b) has the SLP in characteristic p, false otherwise
--requres p>2 and a,b>p-1

auxSLP2= (p,a,b)->(
    a0:=a%p;
    b0:=b%p;
    if a0==(p-1)/2 or a0==(p+1)/2 then(
	if b0==(p-1)/2 or b0==(p+1)/2 then(
	    k1:= boundq(p,a)-1;
	    k2:= boundq(p,b)-1;
	    k:= min {k1,k2};
		-- then a=ak1*p^k1+.... a1*p+a0, we check that aj=(p-1)/2 for j=1,...,k-1
		if (a%p^k)-a0==(p^k -p)/2 then( 
		    -- similarly  b=bk2*p^k2+.... b1*p+a0, we check that bj=(p-1)/2 for j=1,...,k-1
		    if (b%p^k)-b0== (p^k -p)/2 then(
			ak:= (a%p^(k+1))//p^k;
			bk:= (b%p^(k+1))//p^k;
			if ak+bk<p then (
			    if k1==k2 then true 
			    else  if( k1<k2 and ak<= bk) or (k2<k1 and bk<= ak) then true
			    else false)---if any of the condition above fails return false
		      	else false)                   
                    else false)               
                else false)             
            else false)         
        else false)


--User function: This function checks if the monomial complete intersection (ZZ/p)[x_1,...,x_n]/(x_1^{a_1},...,x_n^{a_n})  has the Strong Lefschetz Property (SLP) in characteristic p 
--This function uses the classification presented in Theorem 3.8 [Lundqvist, Nicklasson'18], or, when the option UseConjecture is set to be true, the Han-Monsky multiplication adopting Conjecture 4.1 [KMRR,25]
--input: (p, L) where p is prime and L={a_1,a_2 ... a_n} is a list of integers 
--output: true if  k[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n}) has the SLP in characteristic p, false otherwise


hasSLP = method(Options => {UseConjecture => false})

hasSLP(ZZ, List) := opts -> (p, L) -> (
    n:=#L;
    if n <= 1 or p == 0 then true else (
    	if sum L - n < p then true 
	else (
    	    if not(opts.UseConjecture) then ( 
		if p==2 then ( 
		    if n==2 and ((L#0==2 and  L#1%2==1)  or (L#1==2 and  L#0%2==1) or 
			(L#0==3 and  L#1%4==2)  or (L#0==3 and  L#1%4==2))   then true	 -- by Corollary 6.3. [Cook'11]  
		    else false)-- by Corollary 4.8  [Cook'11] if n>2 and p=2 we don't have the SLP
		else( L1:=select(L, x -> x>=p);-- list the a_j bigger then p
		    if #L1!=1 then  ( 
			if #L==2 and #L1==2 then (return auxSLP2(p, L#0, L#1);)--if this is the case when n=2, p>2 and both a_1,a_2>=p, so we use auxSLP
			else  false)-- if n>2 it is necessary that exactly one a_j is bigger then p
		    else (
			d:=L1#0;
			d0:=d%p;
			sum L -n-(d-1)<=min{p-d0,d0}
			    )
			)
		    )
	     else ( -- This case uses Conjecture 4.1 [KMRR,25] for the Han-Monsky multiplication
		H := recSLP(p,L);
	        not(H === false)
            	)
             )
    	 )
     )





-----------------------------------------------------------
-----------------------------------------------------------
--Computations of sheaf cohomology for divided powers of the cotangent sheaf on projective space
-----------------------------------------------------------
-----------------------------------------------------------

--Input: prime integer p, and list of non-negative integers less than p list1
--list1 corresponds to a positive integer written in base p
--Output: the corresponding integer now in base 10
basep10 = (p, list1) ->(sum(for i from 0 to #list1-1 list list1#i*p^(#list1-1-i)))
    


--Input: prime integer p and an integer num
--Output: a list of non-negative integers less than p
--This list corresponds to num written in base p
base10p = (p, num) ->(
    counter := 0;  while num >= p^counter do (counter = counter+1);
    list1 := {};
    num1 := num;
    for i from 0 to counter-1 do (list1 = append(list1, num1//p^(counter-1-i)); num1 = num1%p^(counter-1-i));
    list1
    )

--q-truncated Schur functor code

--The following code is used to compute certain characters or the sum of the coefficients of monomials for those characters

--Input: q = p^e a power of an integer prime or q = 0, positive integer d,
--and R=n or R = ZZ[x_1..x_n] either a positive integer or a polynomial ring
--Output: If R is a polynomial ring, then the function outputs the q-truncated symmetric polynomial
--where the q=0 is intrepreted to be not truncated
--If R=n an integer then, the output is the number of monomials in this polynomial
--(i.e. the sum of the coefficients of the monomial terms in this polynomial)
hqd = (q, d, R) ->(
    --degenerate case
    if d<0 then return 0;
    --Detecting if R is a polynomial ring or an integer
    bool1 := instance(R, ZZ);
    n :=0;
    if bool1 then (n = R) else (n = numgens R);    
    --initialize q2 locally this is used so normal symmetric powers can be included
    q2:= 0;
    if q == 0 then q2 = d+1 else q2 = q;
    --Case in which an integer is returned
    if bool1 then (
	if (d<q2) then (
	    --in this case it is normal symmetric powers
	    return binomial(d+n-1, d)
	    )
	--This formula for the dimension of truncated symmetric powers comes from the
	--koszul resolution of m^[q]
	else return sum(for i from 0 to ((d+n-1)//q) list ((-1)^i*binomial(n,i)*binomial(d-i*q+n-1, n-1)))
	);
    --this is the polynomial or integer that will eventually be returned
    poly1 := 0;
    --set of potential exponents
    comps := select(compositions(n,d),c -> all(c, i -> i<q2));
    poly1 = sum for c in comps list R_c;
    --need to check that all of the values in each exponent are less that q;
    return poly1
    )


--Input: q=p^e a power of a prime integer p or q = 0, positive integers a,b,
--and R=n or R = ZZ[x_1..x_n] either a positive integer or a polynomial ring
--Output: If R is a polynomial ring, then the function outputs the q-truncated Schur polynomial for (a,b)
--where the q=0 is intrepreted to be not truncated
--If R=n an integer then, the output is the sum of the coefficients of the monomial terms in this polynomial

sqab = (q,a, b, R) ->(hqd(q, a, R)*hqd(q, b, R)-hqd(q, a+1, R)*hqd(q, b-1, R))
    

--Input: prime integer p, positive integer d, integer e
--and R=n or R = ZZ[x_1..x_n] either a positive integer or a polynomial ring
--Output: If R is a polynomial ring, then the function outputs 
--the character Psi_{(a,b)} used in Theorem 1.1
--If R=n an integer then, the output is the sum of the coefficients of the monomial terms in this polynomial

psipab = (p, d, e, R) -> (sum for i from 0 to (d//p+1) list sqab(p,e+i*p, d-i*p,R))

--Inputs: q = p^e a power of a prime integer p
--and R=n or R = ZZ[x_1..x_n] either a positive integer or a polynomial ring
--Output: If R is a polynomial ring, then the output is the q-Frobenius map induced on the ring of characters
--i.e. x_i -> x_i^q
--If R is an integer, then the identity map on the ring of integers is returned
--(Frobenius doesn't affect the sum of coefficients of monomials and likewise does not affect
--dimension of a vector space)
qFrob = (q, R) -> (
    --needed in code when called in case where dimension is being calculated
    --in which case Frobenius does not affect dimension
    if instance(R, ZZ) then return map (ZZ,ZZ);
    n := numgens R;
    return map(R,R, (for i from 0 to n-1 list R_i^q))
    )


--User function
--Input: List of 5 integers l, first entry is 0 or 1 the cohomological degree i, second entry is a prime p
--third entry is a positive integer d, fourth entry is an integer e (These entries correspond to the
--sheaf D^d Omega(d+e)), and the fifth entry is a positive integer which is the number of variables in the
--character ring (or the dimension of the corresponding projective space minus 1)
--Optional Inputs: polynomial ring R = ZZ[x_1..x_n] (ring of characters)
--and FindCharacter boolean value defaulted to false
--Output: If FindCharacter is false and a polynomial ring R is not inputted, then the dimension of the cohomology
--group H^i(D^d Omega(d+e)) where Omega is the cotangent sheaf of projective space P^{n-1} over a field of characteristic p
--If FindCharacter is true then the character for this cohomology group is outputted instead
--If R is inputted, then the character is over this ring

recursiveDividedCohomology = method(Options => {FindCharacter => false})

recursiveDividedCohomology(List) := opts -> (l) ->(
    if ((not opts.FindCharacter)or(not(instance(l#4,ZZ)))) then return recursiveDividedCohomology2(l#0,l#1,l#2,l#3,l#4);
    --Otherwise it is the case that should return the character polynomial
    x := symbol x;
    R := ZZ[x_1..x_(l#4)];
    return recursiveDividedCohomology2(l#0,l#1,l#2,l#3,R)
    )

recursiveDividedCohomology(List, PolynomialRing) := opts ->(l, R) ->(
    return recursiveDividedCohomology2(l#0,l#1,l#2,l#3,R)
    )


--Input: the cohomological degree i (0 or 1), a prime integer p, positive integer d, integer e,
--and R which is either a positive integer n or a polynomial ring ZZ[x_1,...,x_n]
--Output:If R=n an integer, then the dimension of H^i(P^{n-1}, D^d Omega(d+e)) is returned
--If R is a polynomial ring, the character of this sheaf cohomology group is returned instead 
recursiveDividedCohomology2 = (i, p, d, e, R) ->(
    --this makes sure the input for i is in the correct range
    if((i>1)or(i<0)) then (print "this functions compute cohomology for i =0 or i=1"; return 0);
    --this is the base case
    if ((d<p)or(p==0))
    then(if i == 0 then(
	    if e>=d then return sqab(0,e,d,R)
	    else return 0;	    
	    );
	if i== 1 then(
	    if e<=d-2 then return sqab(0, d-1, e+1, R)
	    else return 0;
	    );
	);
   
    --this uses symmetry so that Theorem 1.1 can still be applied
    if e<=d-2 then return recursiveDividedCohomology2(1-i, p, e+1, d-1, R);
    --Having eliminated the previous cases, we can now apply Theorem 1.1
    --poly1 is the polynomial which will eventually be returned
    n:=0;
    if instance(R,ZZ) then n=R else n = numgens R;
    poly1:= 0;
    frob := qFrob(p, R);
    poly1 = sum flatten for a from 0 to (d//p)+1 list (
	for b from max({-1, (d-a*p+e-(2*n*(p-1)))//p}) to ((e+d-a*p)//p) list(
	   (psipab(p, d-a*p, e-b*p, R)*frob(recursiveDividedCohomology2(i, p, a, b,R))) 
	    )
	);
    return poly1
    )


--Input: positive integer m and nR which is a polynomial ring ZZ[x_1..x_n] or the number of variables n
--Output: If nR is a polynomial ring, then the output is the m-th Nim polynomial
--otherwise the output is the sum of the coefficients in the Nim polynomial
NimPoly = (m,nR) ->(
    --base case
    if m == 0 then return 1;
    --Recursive Case
    bool1 := instance(nR, ZZ);
    n := 0;
    if bool1 then n = nR else n = numgens nR;
    --indexing set all integers, i, with same parity of m such that 0 <= i <= min{m, n//2}
    --Nm is the integer (sum of mononomials in the m-th Nim polynomial)
    --or is the m-th Nim polynomial
    Nm := sum for i from 0 to min({m, (n//2)}) list(
	if (((m-i)%2) == 0) then(
	    hqd(2, 2*i, nR)*(qFrob(2, nR))(NimPoly(((m-i)//2), nR))
	    )
	else(continue)
        );
    
    return Nm 
    )


--Input: A non-negative integer d
--Output: A list of lists of three integers
--This list is the index set defined in Theorem 1.2
--This function will be used in nimDividedCohomology
Deltad = (d) -> (
    --the set of q-values appearing are all of the values q=2^r<=d with r>=1;
    --this loops over the exponent r
    r := 1;
    --this is the list to be returned
    delta := flatten while 2^r <= d list(
	q := 2^r;
	-- for a fixed q value, the (q, m, j) that are members of the set satisfy
	-- m+j <= floor((floor(d/q)-1)/2)
	--thus we should include all compositions (m,j) <= floor((floor(d/q)-1)/2)
	t := (((d//q)-1)//2);
	r = r+1;
	flatten for i from 0 to t list (
	    comps := compositions(2, i);
	    for j from 0 to #comps-1 list (
	        {q, comps#j#0, comps#j#1}
		)
	    )
	);
    delta
    )

--User function
--Input: non-negative integer i,  d, integer e, 
--Optional Inputs: polynomial ring R = ZZ[x_1..x_n] (ring of characters) or an integer n the number of variables
--and FindCharacter boolean value defaulted to false
--Output: If FindCharacter is false and a polynomial ring R is not inputted, then the dimension of the cohomology
--group H^i(D^d Omega(d+e)) where Omega is the cotangent sheaf of projective space P^{n-1} over a field of characteristic 2
--If FindCharacter is true then the character for this cohomology group is outputted instead
--If R is inputted, then the character is over this ring
nimDividedCohomology = method(Options => {FindCharacter => false})

nimDividedCohomology(ZZ, ZZ,ZZ,ZZ) := opts -> (i,d,e,n) ->(
    if (not opts.FindCharacter) then return nimDividedCohomology2(i,d,e,n);
    x := symbol x;
    R := ZZ[x_1..x_n];
    return nimDividedCohomology2(i,d,e,R)
    )

nimDividedCohomology(ZZ, ZZ, ZZ, PolynomialRing) := opts ->(i, d, e, R) ->(
    return nimDividedCohomology2(i,d,e,R)
    )


--Input: non-negative integer i,d, and integer
--and R which is either a polynomial ZZ[x_1..x_n] or the number of variables n
--Output: 
nimDividedCohomology2 = (i,d,e, R)-> (
    --Change to case e>=d-1
    if (e < d-1) then (return nimDividedCohomology2(1-i, e+1, d-1, R));
    --Change H^0 calculation to H^1 calculation
    if (i == 0) then return (sqab(0, e,d,R) + nimDividedCohomology2(1, d, e,R));
    --this is the index set, Delta_d, for the summation
    --each entry is of the form (q, m, j) as in the statement of Theorem 1.2
    delta := Deltad(d);
    --Loop to compute elements in sum
    poly1 := sum for i from 0 to #delta-1 list( sqab(delta#i#0, (e-((2*delta#i#1-2*delta#i#2-1)*delta#i#0)), (d-((2*delta#i#1+2*delta#i#2+1)*delta#i#0)), R) * ((qFrob(2*delta#i#0,R))(NimPoly(delta#i#1,R))) );
    --this polynomial is the character to eventually be returned
   -- poly1 := 0;
    --loop to compute the sum
   -- for i from 0 to #delta -1 do(
--	q := delta#i#0;
--	m := delta#i#1;
--	j := delta#i#2;
--	frob := qFrob(2*q, R);
--	Nm := NimPoly(m, R);
--	s := sqab(q, e-((2*m-2*j-1)*q),d-((2*m+2*j+1)*q), R);
--	poly1 = poly1 + ((frob(Nm))*s)
--	);
    return poly1;
    )


--------------------------------------------------------------------------
---Cohomology of line bundles on the incidence correspondence----------------
------------------------------------------------------------------------------



--This function is used to compute the dual function on the ring of characters
--Input: an integer n or a Laurent polynomial ring R = ZZ[x_1,...,x_n,x_1^(-1),...,x_n^(-1)]
--Output: Either the identity ZZ -> ZZ or the map R -> defined by x_i -> x_i^(-1)
dualL = (nR) -> (
    ---Taking duals does not change the dimension of a vector space
    if instance(nR, ZZ) then return map(ZZ,ZZ);
    ---Taking duals changes the character by replacing x_i with x_i^(-1)
    vars1 := gens nR;
    return map(nR, nR, for i from 0 to #vars1-1 list vars1#i^(-1))
    )

--This function is used to compute the determinant character on the ring of characters
--Input: an integer n or a Laurent polynomial ring R = ZZ[x_1,...,x_n,x_1^(-1),...,x_n^(-1)]
--Output: Either the identity ZZ -> ZZ or the map R -> defined by x_i -> x_i^(-1)
det1 = (nR) -> (
    --If nR is an integer then we want the dimension of the determinant representation which is 1
    if instance(nR, ZZ) then return 1;
    --If nR is a Laurent polynomial ring, then we want the character for the determinant representation
    --which is the product of variables
    return product gens nR;
    )
    

--User function
--Input: List of 5 integers l, first entry is the cohomological degree i, second entry is a prime p
--third entry is an integer a, fourth entry is an integer b (These entries correspond to the
--sheaf line bundle j^*(O(a) \boxtimes O(b)), and the fifth entry is a positive integer which is the number of variables in the
--character ring (or the dimension of vector space defining the partial flag variety)
--Optional Inputs: Laurent polynomial ring R = ZZ[x_1..x_n, x_1^{-1}..x_n^{-1}] (ring of characters)
--and FindCharacter boolean value defaulted to false
--Output: If FindCharacter is false and a laurent polynomial ring R is not inputted, then the dimension of the cohomology
--group H^i(j^*(O(a) \boxtimes O(b))) where j is the inclusion of the incidence correspondence
--j: X \to P(V)\times P(V^*) over a field of characteristic p
--If FindCharacter is true then the character for this cohomology group is outputted instead
--If R is inputted, then the character is over this ring
incidenceCohomology = method(Options => {FindCharacter => false})

incidenceCohomology(List) := opts -> (l) ->(
    if ((not opts.FindCharacter)or(not (instance(l#4,ZZ)))) then return incidenceCohomology2(l#0,l#1,l#2,l#3,l#4);
    --Otherwise it is the case that should return the character polynomial
    x := symbol x;
    R := ZZ[x_1..x_(l#4), Inverses=>true, MonomialOrder=>Lex];
    return incidenceCohomology2(l#0,l#1,l#2,l#3,R)
    )

incidenceCohomology(List, PolynomialRing) := opts ->(l, R) ->(
    return incidenceCohomology2(l#0,l#1,l#2,l#3,R)
    )



--This is an auxiliary function used to run the recursion to calculate the cohomology of line bundles on the incidence correspondence
--The inputs and outputs are as described for incidenceCohomology with the exception that nR can be either n or R as described there
incidenceCohomology2 = (i,p,a,b,nR) ->(
    --initialize n to the number of variables of nR or to nR.
    n:=0;
    if instance(nR, ZZ) then n=nR else n= #(gens nR);
    --Check if in range when always 0
    if ((((-n+2) <= a)and(a<=-1))or(((-n+2)<=b)and(b<=-1))) then(return 0);
    --f is the dual map for characters or the identity if only returning dimension
    f:=dualL(nR);
    --We can use symmetry to assume that a>=b
    if (b>a) then(
	return f(incidenceCohomology2(i,p,b,a,nR)));
    --Use Serre duality and symmetry to reduce to case a>=0 and a>=-b-n+1    
    if ((a+b)<(-n+1)) then(
	return (incidenceCohomology2((2*n-3-i), p, (-n+1-b), (-n+1-a), nR)));
    --Apply Kempf Vanishing in case the weights are dominant
    if (b>=0) then(
	if (i != 0)then return 0;
	--Global sections (i==0) is given by the Schur functor $_(a,0,...,0,-b)
	return (hqd(0,a,nR)*(f(hqd(0,b,nR)))-hqd(0,(a-1),nR)*(f(hqd(0,(b-1), nR))))
	);
    --We are now in the case b<=-n+1 and a>=-b-n+1, which are determined by the cohomology
    --of D^dOmega(e) computed by recursiveDividedCohomology
    if (b<=(-n+1)) then (
	if ((i!= (n-1))and (i!=n-2)) then return 0
	else return (det1(nR)*recursiveDividedCohomology2((i-n+2),p,(-b-n+1),(a-1),nR)))
    )



-----------------------------------------------------------
-----------------------------------------------------------
--Documentation
-----------------------------------------------------------
-----------------------------------------------------------



beginDocumentation()
doc ///
  Key
   IncidenceCorrespondenceCohomology
  Headline
     Cohomology on the incidence correspondence, bundle of principal parts, Han-Monsky multiplication, and Lefschetz properties
  Description
   Text
      This package computes the sheaf cohomology of line bundles on the incidence correspondence, 
      the torus equivariant splitting type of the bundle of principal parts for $\mathcal{O}(d)$ on $\mathbb{P}^1$,
      the multiplication in the graded Han-Monsky representation ring, and checks the Weak Lefschetz property.
   Text 
      This Macaulay2 package implements algorithms based on the theoretical results from
   Text   
      @UL{{"Annet Kyomuhangi, Emanuela Marangone, Claudiu Raicu, and Ethan Reed, ",
	      EM"Cohomology on the incidence correspondence and related questions, ",
	      HREF("https://arxiv.org/abs/2411.13450 "),
	      " arXiv, ", "2024"}}@ 
   Text 
      @UL{{"Annet Kyomuhangi, Emanuela Marangone, Claudiu Raicu, and Ethan Reed, ", EM "Computing the cohomology of line bundles on the incidence correspondence and related invariants,", "arXiv, ","2025"} }@
 
   Text 
      This package also checks Strong Lefschetz Property in positive characteristic based on the classification from
   Text 
      @UL{{"Lisa Nicklasson, ", EM "The strong Lefschetz property of monomial complete intersections in two variables, ", "Collectanea Mathematica, ","2018"} }@
   Text
      Notation: 
      
      We indicate $\Omega$ the tangent bundle on $\mathbb{P}^{n-1}$, and with $\mathcal{R}=\Omega(1)$ the universal rank (n-1) subsheaf. $D^d \mathcal{R}$ denotes the d-th divided power of $\mathcal{R}$.
      
      We indicate the incidence correspondence with $X$. Each line bundle on $X$ is the restriction of a line bundle on $\mathbb{P}\times\mathbb{P}^\vee$: we denote with $\mathcal{O}_X(a,b)$ the restriction to $X$ of the line bundle $\mathcal{O}(a)\times\mathcal{O}(b)$.

 Subnodes
    splittingFdr
    splittingPrincipalParts
    Multidegree
    hanMonsky
    hasWLP
    monomialCIsWithoutWLP
    hasSLP
    UseConjecture
    GorensteinAlg
    MonomialAlg
    recursiveDividedCohomology
    nimDividedCohomology
    incidenceCohomology
    FindCharacter
    
///


------------------------------------------------------------------------------------------
--- Splitting type---------------------------------------------------------------------
---------------------------------------------------------------------------------------------
    
doc///
  Key
   splittingFdr
   (splittingFdr,ZZ,ZZ,ZZ)
  Headline
   Computes the torus equivariant splitting type of the bundles $\mathcal{F}^d_r$ introduced in [KMRR,24]
  Usage
   L = splittingFdr(p,d,r)
  Inputs
   p: ZZ
     prime or zero, characteristic of the field
   d: ZZ
     twist of the line bundle
   r: ZZ
     which bundle of principal part
  Outputs
   L: List
     list which gives the splitting type as a direct sum of line bundles for $\mathcal{F}^d_r$ in characteristic p
     By default, only the twists appear in the list, but if Multidegree is set to true then it returns a list of list of three integers: the first is the twist while the second and third are the multidegrees
 
  Description
   Text
     This function computes the splitting type as a direct sum of line bundles for  $\mathcal{F}^d_r$ over a field of characteristic p using Theorem 3.2 [KMRR,24].
    
     The user inputs the prime p and the integers d and r.
     
     The output is a list whose entries are either integers if Multidegree is false, or lists of three integers if Multidegree is true. 
     When Multidegree is false, the integers in the list are the twists for the line bundles occurring in the decomposition. When Multidegree is true, for each element of the list, the first entry is the corresponding twist for the line bundle, and the second and third entries are the multidegrees arising from a torus action upon choosing coordinates for $\mathbb{P^1}$.
   Example
     p = 5;
     d = 8; r = 6;
     splittingFdr(p,d,r)
    
   Text
     We compute the same example but now we also see the multidegrees.   
   Example
     p = 5;
     d = 8; r= 6;
     splittingFdr(p,d,r, Multidegree=>true)
   Example
     p = 0;
     d = 8; r= 6;
     splittingFdr(p,d,r, Multidegree=>true)
    
///

doc///
  Key
   splittingPrincipalParts
   (splittingPrincipalParts,ZZ,ZZ,ZZ)
  Headline
   Computes the torus equivariant splitting type of the bundle of principal parts on $\mathbb{P}^1$ 
  Usage
   L = splittingPrincipalParts(p,m,k)
  Inputs
   p: ZZ
     prime or zero, characteristic of the field
   m: ZZ
     twist of line bundle
   k: ZZ
     which bundle of principal part
  Outputs
   L: List
     list which gives the splitting type as a direct sum of line bundles for the k-th bundle of principal parts $\mathcal{P}^k(\mathcal{O}(m))$ on $\mathbb{P}^1$ in characteristic p
     By default, only the twists appear in the list, but if Multidegree is set to true then it returns a list of list of three integers the first being the twist and the second and third being multidegrees
 
  Description
   Text
     This function computes the splitting type as a direct sum of line bundles of the k-th bundle of principal parts for $\mathcal{O}(m)$ on $\mathbb{P}^1$ over a field of characteristic p using the relationship between $\mathcal{P}^k(\mathcal{O}(m))$ and the bundle $\mathcal{F}^d_r$ as described in [KMRR,24].
     
     The user inputs the prime p and the integers m and k.
     
     The output is a list whose entries are either integers if Multidegree is false, or lists of three integers if Multidegree is true. When Multidegree is false, the integers in the list are the twists for the line bundles occurring in the decomposition.
     When Multidegree is true, for each element of the list, the first entry is the corresponding twist for the line bundle, and the second and third entries are the multidegrees arising from a torus action upon choosing coordinates for $\mathbb{P}^1$.  
   Example
     p = 5;
     m = 10; k = 7;
     splittingPrincipalParts(p,m,k)
   Example
     p = 0;
     m = 10; k = 7;
     splittingPrincipalParts(p,m,k)
    
   Text
     We compute the same example but now we also compute the multidegrees.  
   Example
     p = 5;
     m = 10; r= 7;
     splittingPrincipalParts(p,m,k, Multidegree=>true)
    
///

doc ///
 Key
  Multidegree
  [splittingFdr, Multidegree]
  [splittingPrincipalParts, Multidegree]

 Headline
  An option to compute the multidegrees arising from a torus action on $\mathbb{P}^1$
 Description
    Text
       The default value is false. When defined to be true, the function will output the multidegrees in addition to the twist of line bundles occurring in the decomposition.

 SeeAlso
  splittingFdr
  splittingPrincipalParts

///


----------------------------------------------------------------------------------------------------------------------
------ Han-Monsky multiplication and Lefschetz Property------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------



doc ///
 Key
   hanMonsky
   (hanMonsky,ZZ,List)
 Headline
     Computes the multiplication in the graded Han-Monsky representation ring
 Usage
   HM = hanMonsky(p, L)
 Inputs
   p: ZZ
     prime or zero, characteristic of the field
   L: List
     list of integers $a_1,...,a_n$

 Outputs
   MH: HashTable
       Hash Table representing the product  $\delta_{a_1}*\delta_{a_2}*....*\delta_{a_n}$: 
       each pair on the Hash Table is of the form {c => $f(q)$} where $f(q)$ is a polynomial in ZZ[q] with non-negative coefficients such that $b$ $q^j$ is a term in $f(q)$ if and only if $\delta_c(-j)$ is a summand with multiplicity b in  $\delta_{a_1}*\delta_{a_2}*....*\delta_{a_n}$ 
     
 Description
   Text
     Computes the Han-Monsky multiplication $\delta_{a_1}*\delta_{a_2}*....*\delta_{a_n}$.
     In positive characteristic, the default instance employs Conjecture 4.1 [KMRR,25] to allow faster computations. 
   Example
     p = 5;
     L = {3,7,8};
     hanMonsky(p, L)
   Example
     p = 0;
     L = {3,7,8};
     hanMonsky(p, L)
      
   Text
     It is possible to compute it without employing Conjecture 4.1 [KMRR,25] setting the option “UseConjecture” to be false.
   Example
     p = 5;
     L = {3,7,8}
     hanMonsky(p, L, UseConjecture =>false)

///


doc ///
 Key
   hasWLP
   (hasWLP,ZZ,List)
   (hasWLP,PolynomialRing,Ideal)
 Headline
    Checks whether a graded Artinian algebra has the Weak Lefschetz Property (WLP)
 Usage
   hasWLP(p,L)
   hasWLP(R,I)
 Inputs
   p: ZZ
     prime characteristic of the field
   L: List
     list of integers $a_1,...,a_n$
   R: PolynomialRing
   I: Ideal 
     ideal of R, such that R/I is Artinian
 Outputs
   :Boolean
     whether the monomial complete intersections $A=k[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n})$, where k is a field of characteristic p, has the Weak Lefschetz property (hasWLP(p,L)).
     whether the monomial complete intersections A=R/I  has the Weak Lefschetz Property (hasWLP(R,I)).
 Description
   Text
     hasWLP(p,L) check if the monomial complete intersections $A=k[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n})$, where k is a field of characteristic p has the Weak Lefschetz property.
     For p=2 this method uses  Theorem 8.1 in [KMRR,24]. For p>2, it uses the Han-Monsky multiplication obtained with Conjecture 4.1 [KMRR,25].
   Example
     p = 3;
     L = {3,3,3};
     hasWLP(p, L) 
   Example
     p = 3;
     L = {4,5,7};
     hasWLP(p, L)
     
   Text
     It is possible to check the WLP without employing the Han-Monsky multiplication, setting the option UseConjecture to be false.
   Example
     p = 5;
     L = {3,7,8}
     hasWLP(p, L, UseConjecture =>false)  
     
   Text
     hasWLP(R,I) check if the graded Artinian algebra R/I, where R is a standard graded polynomial ring, has the Weak Lefschetz property.
   Example
     R=QQ[x,y,z];
     I=ideal(x^3, y^3, z^3, x*y*z);
     hasWLP(R,I)
   Example
     R=QQ[x,y,z];
     I=ideal(x*z, x*y, z^3,y^3,x^4+6*y^2*z^2);
     hasWLP(R,I)
   
   Text
     The options  GorensteinAlg and MonomialAlg allow simplifying the computation when the input I is respectively a Gorenstein or a monomial ideal. 
   Example 
     R=QQ[x,y,z];
     I=ideal(x^9,y^9,z^9,x^3*y^3*z^3);
     hasWLP(R,I, MonomialAlg => true)
   Example
     R=ZZ/7[x,y,z];
      I=ideal(x*z, x*y, z^3,y^3,x^4+6*y^2*z^2);--Gorenstein
     hasWLP(R,I,GorensteinAlg => true)
   Example
     R=ZZ/17[x,y,z];
     I=ideal(3*x^4+y^4,z*y^3, x^5-z^5);--Gorenstein 
     hasWLP(R,I,GorensteinAlg => true)

 Caveat
   The instances hasWLP(R,I) and hasWLP(R,I, GorensteinAlg=>true) require R to be a standard graded polynomial ring over a sufficiently large field k, e.g., R=QQ[x_1,…,x_n].
   If R is a polynomial ring over a finite field, then when hasWLP(R, I) outputs false, it confirms that R/I fails the WLP. 
   However, when hasWLP(R, I) outputs true, we can only conclude that R/I has the WLP over a field extension. The same holds for hasWLP(R,I, GorensteinAlg=>true).
   
   This does not apply to hasWLP(p,L) and hasWLP(R,I, MonomialAlg=>true), which work over any field k.
     
///


doc /// 
 Key
  monomialCIsWithoutWLP
  (monomialCIsWithoutWLP, ZZ, ZZ, ZZ)
 Headline
  Lists all the monomial complete intersections of socle s and height n that fail the Weak Lefschetz Property 
 Usage
   L= monomialCIsWithoutWLP(p,n,s)
 Inputs
   p: ZZ
     prime characteristic of the field
   n: ZZ
     number of variables
   s: ZZ
    socle degree
 Outputs
   L: List
     list of  n-tuples ($a_1,....,a_n$) such that $a_1+.......+a_n-n=s$ and $2\leq a_1\leq ....\leq a_n$ for which $k[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n})$ does not have the WLP in characteristic p
 Description
    Text
       This method lists all the n-tuples $(a_1,....,a_n)$, up to order, such that $a_1+.......+a_n-n=s$  and  $k[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n})$ fails the WLP with characteristic p, using the method hasWLP.
       The default value of the option UseConjecture is true, so the computations are based on Conjecture 4.1 [KMRR,25]. When UseConjecture is set to false, the method does not use Conjecture 4.1 [KMRR,25]. 
  Example
     p = 5;
     s = 10;
     n = 4;
     L = monomialCIsWithoutWLP(p,n,s)
       
 SeeAlso
    hasWLP
///


doc ///
 Key
   hasSLP
   (hasSLP,ZZ,List)
 Headline
    Checks whether a monomial complete intersection over a field of characteristic p has the Strong Lefschetz Property (SLP) 
 Usage
   hasSLP(p,L)
 Inputs
   p: ZZ
     prime characteristic of the field
   L: List
     list of integers $a_1,...,a_n$
 Outputs
   :Boolean
     whether the monomial complete intersections $A=k[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n})$, where k is a field of characteristic p, has the Strong Lefschetz property.     
 Description
   Text
     Check if the monomial complete intersections $A=k[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n})$, where k is a field of characteristic p has the Strong Lefschetz property using Theorem 3.4 of [Nicklasson,18].
   Example
     p = 3;
     L = {4,5,7}
     hasSLP(p, L)
   Example
     p = 13;
     L = {3,3,19}
     hasSLP(p, L)
    
   Text
     By default, this method does not use the Han-Monsky multiplication.  
     When UseConjecture is set to true, the method uses the Han-Monsky obtained from  Conjecture 4.1 [KMRR,25].
   Example
     p = 5;
     L = {3,7,8}
     hasSLP(p, L, UseConjecture =>true)

///




doc /// 
 Key
  UseConjecture
  [hanMonsky,UseConjecture]
  [hasWLP,UseConjecture]
  [monomialCIsWithoutWLP, UseConjecture]
  [hasSLP,UseConjecture]
 Headline
   An option that, whenever the Han-Monsky is computed, allows the user to choose if employ Conjecture 4.1 [KMRR,25] or not
 Description
    Text
       The default value in hanMonsky is true, so the Han-Monsky multiplication is computed based on Conjecture 4.1 [KMRR,25]. 
       When defined to be false, the function will not use Conjecture 4.1 [KMRR,25]. This will significantly increase the time of computation for the method hanMonsky.
       
       The default value in hasWLP(p,L) and monomialCIsWithoutWLP is true, when is set to be false the computations are done with different methods, without computing the hanMonsky multiplication. This option does not affect hasWLP(R,L) that does not use the hanMonsky multiplication.
       In hasSLP, instead, the default value is false.
 SeeAlso
    hanMonsky
    hasWLP
    hasSLP
    monomialCIsWithoutWLP
///



doc /// 
 Key
  GorensteinAlg
  [hasWLP,GorensteinAlg]
 Headline
   An option to check the WLP for Gorenstein algebras
 Description
    Text
       The default value is false. When defined to be true, the function hasWLP(R,I) assumes R/I Gorenstein. 
 SeeAlso 
   hasWLP
  

///

doc /// 
 Key
  MonomialAlg
  [hasWLP,MonomialAlg]
 Headline
   An option to check the WLP for R/I with I Monomial ideal
 Description
    Text
       The default value is false.  When defined to be false, the function hasWLP(R,I) assumes I is monomial.
 SeeAlso 
   hasWLP
  

///

---------------------------------------------------------------------------------------
-- recursiveDividedCohomology nimDividedCohomology
-------------------------------------------------------------------------------------


  
doc ///
 Key
  recursiveDividedCohomology 
  (recursiveDividedCohomology,List)
  (recursiveDividedCohomology, List, PolynomialRing)
 Headline
  Computes dimension or character of sheaf cohomology of twists of divided powers of the cotangent sheaf on projective space
 Usage
  C = recursiveDividedCohomology(L)
  C = recursiveDividedCohomology(L, R)
 Inputs
  L: List
   List of integers {i,p,d,e,n} where i = 0,1 is the cohomological degree, p is the characteristic of the field, d is the Divided power, e allows for additional twists, and n is the dimension of projective space + 1
  R: PolynomialRing
   Ambient ring for the character of the cohomology group, i.e. ZZ[x_1..x_n] where n-1 is the dimension of projective space
 Outputs
  C: ZZ
   Dimension of the cohomology group (with the option to instead have the character)
 Description
  Text
    recursiveDividedCohomology({i,p,d,e,n}) computes the dimension of $H^i(\mathbb{P}^{n-1}, D^d R(e))$, where $D^d \mathcal{R}$ is the d-th divided power of the universal rank (n-1) subsheaf $\mathcal{R}$.
    
    The cohomology index i is 0 or 1 and the underlying characteristic of the field is p (prime or 0). 
  Example
    i = 1; p = 3; d = 7; e = 6; n = 3;
    recursiveDividedCohomology({i,p,d,e,n})
  Example
    i = 1; p = 0; d = 7; e = 6; n = 3;
    recursiveDividedCohomology({i,p,d,e,n})
  Text
    We could instead ask for the character instead of just the dimension, setting the option FindCharacter to be true.
  Example
   i = 1; p = 3; d = 7; e = 6; n = 3;
   recursiveDividedCohomology({i,p,d,e,n}, FindCharacter => true)
  Example
   i = 1; p = 0; d = 5; e = 3; n = 3;
   recursiveDividedCohomology({i,p,d,e,n}, FindCharacter => true)
  Text
   Additional input of a polynomial ring allows the user to control the ambient ring of the character.
  Example
   i = 1; p = 3; d = 7; e = 6; R = ZZ[x_1..x_3];
   recursiveDividedCohomology({i,p,d,e},R)
         
///


  
doc///
 Key
  nimDividedCohomology
  (nimDividedCohomology, ZZ, ZZ, ZZ, ZZ)
  (nimDividedCohomology, ZZ, ZZ, ZZ, PolynomialRing)
 Headline
  Computes the dimension or character of sheaf cohomology of twists of divided powers of the cotangent sheaf on projective space, in characteristic 2
 Usage
  C = nimDividedCohomology(i,d,e,n)
  C = nimDividedCohomology(i,d,e,R)
 Inputs
  i:ZZ
   cohomological degree
  d: ZZ
   divided power of the cotangent sheaf
  e: ZZ
   twist of the sheaf
  n: ZZ
   dimension of projective space + 1
  R: PolynomialRing
   ambient polynomial ring of the character, i.e. ZZ[x_1..x_n] where n-1 is the dimension of projective space
 Outputs
  C: ZZ
   dimension of the cohomology group H^i(P^{n-1}, D^d Omega (d+e)), with the option to instead have the character
 Description
  Text
   This function computes in characteristic 2 the sheaf cohomology of twists of divided powers of the cotangent sheaf on projective space $H^i(\mathbb{P}^{n-1}, D^d \mathcal{R}(d))$, where $D^d \mathcal{R}$ is the d-th divided power of the universal rank (n-1) subsheaf $\mathcal{R}$.
  Example
   i =1; d = 5; e = 4; n = 3;
   nimDividedCohomology(i,d,e,n)
  Text
   This function can also output the character instead of just the dimension, setting the option FindCharacter to be true.
  Example
   i = 1; d = 5; e = 4; n = 3;
   nimDividedCohomology(i,d,e,n, FindCharacter => true)
  Text
   This function allows the user to control the ambient ring of the character, using the ambient ring R as the fourth input instead then n.
  Example
   i = 1; d = 5; e = 4; R = ZZ[x_1..x_3];
   nimDividedCohomology(i,d,e,R)
///


doc ///
 Key
  incidenceCohomology 
  (incidenceCohomology,List)
  (incidenceCohomology, List, PolynomialRing)
 Headline
  Computes dimension or character of sheaf cohomology of line bundles on the incidence correspondence
 Usage
  C = incidenceCohomology(L)
  C = incidenceCohomology(L, R)
 Inputs
  L: List
   List of integers {i,p,a,b,n} List of integers {i,p,a,b,n} where i is the cohomological degree, p is the characteristic of the field, the couple (a,b) indicates the line bundle $\mathcal{O}_X(a,b)$ on the incidence correspondence $X\subseteq \mathbb{P}\times \mathbb{P}^\vee$, and $n$ is the dimension of $\mathbb{P} +1
  R: PolynomialRing
   Laurent polynomial ring, ambient ring for the character of the cohomology group, i.e. ZZ[x_1..x_n,x_1^{-1}..x_n^{-1}] where n-1 is the dimension of projective space
 Outputs
  C: ZZ
   dimension of the cohomology group (with the option to instead have the character)
 Description
  Text
    This function computes the sheaf cohomology  $H^i(X, \mathcal{O}_X(a,b))$, of the line bundle $\mathcal{O}_X(a,b)$ on the incidence correspondence $X$.
    The underlying characteristic of the field is p, prime or zero.
  Example
    i = 0; p = 0; a = 7; b = 6; n = 3;
    incidenceCohomology({i,p,a,b,n})
  Example
    i = 1; p = 3; a = 6; b = -8; n = 3;
    incidenceCohomology({i,p,a,b,n})
  Example
    i = 1; p = 0; a = 6; b = -8; n = 3;
    incidenceCohomology({i,p,a,b,n}) 
  Text
    We could instead ask for the character instead of just the dimension, setting the option FindCharacter to be true.
  Example
   i = 3; p = 0; a = -2; b = -6; n = 3;
   incidenceCohomology({i,p,a,b,n}, FindCharacter => true)
  Example
   i = 1; p = 3; a = 6; b = -8; n = 3;
   incidenceCohomology({i,p,a,b,n}, FindCharacter => true)
  Text
   Additional input of a polynomial ring allows the user to control the ambient ring of the character.
  Example
   i = 2; p = 3; a = 7; b = -10; R = ZZ[x_1..x_3, Inverses=>true, MonomialOrder=>Lex];
   incidenceCohomology({i,p,a,b},R)
         
///




doc /// 
 Key
  FindCharacter
  [recursiveDividedCohomology, FindCharacter]
  [nimDividedCohomology, FindCharacter]
  [incidenceCohomology, FindCharacter]
  
 Headline
   An option to compute the character instead of the dimension
 Description
    Text
      The default value is false.  When defined to be true, the function will output the character instead of the dimension for the cohomology.
 SeeAlso 
  recursiveDividedCohomology
  nimDividedCohomology
  incidenceCohomology
  

///


-------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Tests---------------------------------------------------------------------
-------------------------------------------------------------------------------
------------------------------------------------------------------------------------


---------------------------------------------------------------------------------
--Test for splittingFdr------------------------------------------------------
--------------------------------------------------------------------------------------


TEST /// -- Test 0
    debug(IncidenceCorrespondenceCohomology)
    R=ZZ/5[x,y, Degrees=>{{1,-1,0},{1,0,-1}}]
    assert((tally degrees Hom(gradedFdr(R,4,3),R))-(tally splittingFdr(5,4,3, Multidegree=>true))==0)
///

TEST /// -- Test 1
    debug(IncidenceCorrespondenceCohomology)
    R=ZZ/3[x,y, Degrees=>{{1,-1,0},{1,0,-1}}]
    assert((tally degrees Hom(gradedFdr(R,5,6),R))-(tally splittingFdr(3,5,6, Multidegree=>true))==0)
///

TEST /// -- Test 2
    debug(IncidenceCorrespondenceCohomology)
    R=ZZ/3[x,y, Degrees=>{{1,-1,0},{1,0,-1}}]
    assert((tally degrees Hom(gradedFdr(R,15,6),R))-(tally splittingFdr(3,15,6, Multidegree=>true))==0)
///

TEST /// -- Test 3
    debug(IncidenceCorrespondenceCohomology)
    R=ZZ/3[x,y, Degrees=>{{1,-1,0},{1,0,-1}}]
    assert((tally degrees Hom(gradedFdr(R,13,6),R))-(tally splittingFdr(3,13,6, Multidegree=>true))==0)
///

TEST /// -- Test 4
    debug(IncidenceCorrespondenceCohomology)    
    R=ZZ/3[x,y, Degrees=>{{1,-1,0},{1,0,-1}}]
    assert((tally degrees Hom(gradedFdr(R,7,6),R))-(tally splittingFdr(3,7,6,Multidegree=>true))==0)
///


TEST /// --Test 5
    debug(IncidenceCorrespondenceCohomology)
    R=ZZ/5[x,y]
    assert((tally flatten degrees Hom(Fdr(R,4,3),R))-(tally splittingFdr(5,4,3))==0)
///

TEST /// --Test 6
    debug(IncidenceCorrespondenceCohomology)
    R=ZZ/3[x,y]
    assert((tally flatten degrees Hom(Fdr(R,5,6),R))-(tally splittingFdr(3,5,6))==0)
///

TEST ///  --Test 7
    debug(IncidenceCorrespondenceCohomology)
    R=ZZ/3[x,y]
    assert((tally flatten degrees Hom(Fdr(R,15,6),R))-(tally splittingFdr(3,15,6))==0)
///

TEST /// --Test 8
    debug(IncidenceCorrespondenceCohomology)
    R=ZZ/3[x,y]
    assert((tally flatten degrees Hom(Fdr(R,13,6),R))-(tally splittingFdr(3,13,6))==0)
///

TEST /// --Test 9
    debug(IncidenceCorrespondenceCohomology)
    R=ZZ/3[x,y]
    assert((tally flatten degrees Hom(Fdr(R,7,6),R))-(tally splittingFdr(3,7,6))==0)
///

TEST /// --Test 10
    assert(((tally splittingPrincipalParts(5, 10, 7)) - (tally {5, 2, 2, 5, 2, 2, 3, 3}))==0)
///

TEST /// --Test 11
    assert(((tally splittingPrincipalParts(5, 10, 7,Multidegree=>true)) - (tally {{5, 0, 5}, {2, 1, 7}, {2, 2, 6}, {5, 5, 0}, {2, 6, 2}, {2, 7, 1}, {3, 3, 4}, {3, 4, 3}}))==0)
///

----------------------------------------------------------------------------------
--Test for the Hanmosnky multiplication--------------------------------------------
--------------------------------------------------------------------------------

TEST/// -- Test 12
  p = 5;
  L = {3,3,7,8}
  assert(hanMonsky(p, L)=== hanMonsky(p, L, UseConjecture =>false))
 
///


TEST/// -- Test 13
  debug (IncidenceCorrespondenceCohomology)
  p = 3;
  L = {5,6};
  H = hanMonsky(p, L);
  assert((H#3 == HMRing_{3} + HMRing_{4}) and (H#6 == HMRing_{2}) and (H#9 == HMRing_{1} + HMRing_{0}) and (#keys H == 3))
///

--------------------------------------------------------------------------------
--Tests for the Lefschetz Properties --------------------------------------------
--------------------------------------------------------------------------------


TEST /// -- Test 14
 p = 3;
 L = {3,3,3}
assert(hasWLP(p, L)==false)
///

TEST /// -- Test 15
 p = 3;
 L = {3,4,6}
assert( hasWLP(p, L)==true)
///


TEST /// -- Test 16
 p = 5;
 L = {3,7,10,35}
assert(hasWLP(p, L, UseConjecture =>false)== hasWLP(p, L))
///


TEST /// -- Test 17
     p = 3;
     L = {3,4,6}
 assert( hasSLP(p, L)==false)
///


TEST /// -- Test 18
     p = 11;
     L = {3,3,15}
 assert( hasSLP(p, L)==true)
///



TEST /// -- Test 19
     p = 7;
     L = {3,3,10}
 assert( hasSLP(p, L, UseConjecture =>true)==hasSLP(p, L))
///


TEST /// -- Test 20
    p = 5;
    n=3;
    s=15;
    d=(s+1)//2;
    e=s//2;
    ex=exponents recursiveDividedCohomology({1,p,d,e,n}, FindCharacter => true); --list the exponents of the character of H^i(P^{n-1}, D^d Omega(d+e))
    S= for i from 0 to #ex-1 list apply(ex#i, j->j+1); -- this must correspond to n-tuples (a_1,....,a_n) such  k[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n}) does not have the WLP
    result= select(S, x -> x#0>1 and x_1>=x_0 and x#2>=x#1);--we restric to 2<=a_1<= ....<= a_n 
assert(isSubset(monomialCIsWithoutWLP(p,n,s), result) and isSubset(result, monomialCIsWithoutWLP(p,n,s)))
///


TEST /// -- Test 21
    p = 7;
    n=4;
    s=20;
    d=(s+1)//2;
    e=s//2;
    ex=exponents recursiveDividedCohomology({1,p,d,e,n}, FindCharacter => true); --list the exponents of the character of H^i(P^{n-1}, D^d Omega(d+e))
    S= for i from 0 to #ex-1 list apply(ex#i, j->j+1); -- this must correspond to n-tuples (a_1,....,a_n) such  k[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n}) does not have the WLP
    result= select(S, x -> x#0>1 and x_1>=x_0 and x#2>=x#1 and x#3>=x#2); --we restric to 2<=a_1<= ....<= a_n 
assert(isSubset(monomialCIsWithoutWLP(p,n,s), result) and isSubset(result, monomialCIsWithoutWLP(p,n,s)))
///

TEST /// -- Test 22
    p = 3;
    n=4;
    s=23;
    d=(s+1)//2;
    e=s//2;
    ex=exponents recursiveDividedCohomology({1,p,d,e,n}, FindCharacter => true);--list the exponents of the character of H^i(P^{n-1}, D^d Omega(d+e))
    S= for i from 0 to #ex-1 list apply(ex#i, j->j+1); -- this must correspond to n-tuples (a_1,....,a_n) such  k[x_1,...,x_n]/(x_1^{a_1}, …, x_n^{a_n}) does not have the WLP
    result= select(S, x -> x#0>1 and x_1>=x_0 and x#2>=x#1 and x#3>=x#2);--we restric to 2<=a_1<= ....<= a_n 
assert(isSubset(monomialCIsWithoutWLP(p,n,s), result) and isSubset(result, monomialCIsWithoutWLP(p,n,s, UseConjecture =>false)))
///


  TEST /// --Test 23
    p=11;
    L={7,11,14,22};
    R=ZZ/p[x,y,z,w];
    I=ideal(x^(L#0),y^(L#1),z^(L#2),w^(L#3));
 assert( hasWLP(p, L)==hasWLP(R,I))
 ///
 

TEST /// -- Test 24
     p = 13;
     L = {13,13,13};
     R=ZZ/p[x,y,z];
     I=ideal(x^(L#0),y^(L#1),z^(L#2));
 assert( hasWLP(p, L)==hasWLP(R,I,MonomialAlg => false))
///


TEST /// -- Test 25
     R=QQ[x,y,z];
     I=ideal(3*x^4+y^4,z*y^3, x^5-z^5);
 assert( hasWLP(R,I,GorensteinAlg => true)==true)
///


TEST /// -- Test 26
     R=QQ[x,y,z];
     I=ideal(x^9,y^9,z^9,x^3*y^3*z^3);
 assert( hasWLP(R,I)==false)
///

-------------------------------------------------------------------------
-----Test for the cohomology calculation--------------------------------
-----------------------------------------------------------------------

TEST ///-- Test 27
    assert(recursiveDividedCohomology({1,2,6,5,3})==nimDividedCohomology(1,6,5,3))
///

TEST /// -- Test 28
    R=ZZ[x_1..x_3];
    assert(recursiveDividedCohomology({1,2,5,5},R)==nimDividedCohomology(1,5,5,R))
    ///
    
TEST /// --Test 29
    
///

TEST /// -- Test 30
    assert(incidenceCohomology({0,3,3,-2,4})==0)
///


TEST /// -- Test 31
    assert(incidenceCohomology({2,3,-11,7,3})==80)
///

end



restart

debug needsPackage"IncidenceCorrespondenceCohomology"


uninstallPackage"IncidenceCorrespondenceCohomology"
installPackage"IncidenceCorrespondenceCohomology"

check("IncidenceCorrespondenceCohomology")

hasWLP(2,{0,0,2})

debug(IncidenceCorrespondenceCohomology)


viewHelp IncidenceCorrespondenceCohomology

incidenceCohomology({3,3,-4,-3,3},FindCharacter=>true)

R = ZZ[x_1,x_2,x_3, Inverses=>true, MonomialOrder=>Lex]


------------------------
------------------------
------------------------
------------------------
restart
debug needsPackage"IncidenceCorrespondenceCohomology"

pr = 5;
lis = {3,4,5,9,10};

time directWLP(pr,lis)

time hanMonsky(pr,lis,UseConjecture => false);
time hanMonsky2(pr,lis,UseConjecture => false);


pr = 5;
lis = {3,4,5,9,10,11,24};
time hanMonsky(pr,lis);
time hanMonsky2(pr,lis);


pr = 2; lis = {3,5,7,2,4,5};
hanMonsky2(pr,lis)
hmMinShifts(pr,lis)


pr = 5; lis = {123,45,47,2444,4,125,23,55,16,193,242};

pr = 5; lis = {123,45,47,55,16,193,242};
time hmWLP(pr,lis)
time hasWLP(pr,lis,UseConjecture => false)


restart
loadPackage("IncidenceCorrespondenceCohomology")
debug(IncidenceCorrespondenceCohomology)
splittingFdr2(0, 5, 3)


splittingPrincipalParts(0, 10, 7) 
splittingPrincipalParts(0, 4, 7)
splittingPrincipalParts(0, -5, 7)


splittingPrincipalParts(0, 10, 7, Multidegree=>true)
splittingPrincipalParts(0, 4, 7, Multidegree =>true)
splittingPrincipalParts(0, -5, 7, Multidegree=>true)


splittingFdr(0, 11,8)
splittingFdr(0, 11,8, Multidegree=>true)


splittingPrincipalParts(0, 0, 1)
splittingPrincipalParts(0, 15, 1, Multidegree=>true)
