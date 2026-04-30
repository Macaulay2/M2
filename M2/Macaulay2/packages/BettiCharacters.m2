--------------------------------------------------------------------------------
-- Copyright 2021-2026  Federico Galetto
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------


newPackage(
     "BettiCharacters",
     Version => "2.6",
     Date => "Dec 22, 2025",
     AuxiliaryFiles => false,
     Authors => {{Name => "Federico Galetto",
     	       Email => "galetto.federico@gmail.com",
	       HomePage => "http://math.galetto.org"}},
     Headline => "finite group characters on free resolutions and graded modules",
     DebuggingMode => false,
     PackageExports => {"Complexes"},
     Keywords => {"Commutative Algebra"},
     Certification => {
	 "journal name" => "Journal of Software for Algebra and Geometry",
	 "journal URI" => "https://msp.org/jsag/",
	 "article title" => "Setting the scene for Betti characters",
	 "acceptance date" => "2023-05-30",
	 "published article URI" => "https://msp.org/jsag/2023/13-1/p04.xhtml",
	 "published article DOI" => "10.2140/jsag.2023.13.45",
	 "published code URI" => "https://msp.org/jsag/2023/13-1/jsag-v13-n1-x04-BettiCharacters.m2",
	 "repository code URI" => "https://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/BettiCharacters.m2",
	 "release at publication" => "a446af4424af33c06ab97694761a4d5bbc4d535f",
	 "version at publication" => "2.1",
	 "volume number" => "13",
	 "volume URI" => "https://msp.org/jsag/2023/13-1/"
	 }
     )

export {
    "action",
    "Action",
    "ActionOnComplex",
    "ActionOnGradedModule",
    "actors",
    "character",
    "characterTable",
    "Character",
    "CharacterDecomposition",
    "CharacterTable",
    "decomposeCharacter",
    "degreeOrbit",
    "degreeRepresentative",
    "Labels",
    "ringActors",
    "Semidirect",
    "Sub",
    "symmetricGroupActors",
    "symmetricGroupTable",
    "hyperoctahedralGroupTable",
    "hyperoctahedralGroupActors"
    }

----------------------------------------------------------------------
-- Types
----------------------------------------------------------------------

Character = new Type of HashTable
CharacterTable = new Type of HashTable
CharacterDecomposition = new Type of HashTable
Action = new Type of HashTable
ActionOnComplex = new Type of Action
ActionOnGradedModule = new Type of Action

----------------------------------------------------------------------
-- Characters and character tables -----------------------------------
----------------------------------------------------------------------

-- function to take a single degree and make it into a list
-- this is the default degreeOrbit function to be used by
-- all actions GxT where T is the torus that gives the grading
-- actions by semidirect products G⋉T need a different function
bracketize := d -> {d}

-- method for returning characters of various action types
character = method(TypicalValue=>Character,Options=>{Semidirect=>(bracketize,identity)})

-- construct a finite dimensional character by hand
-- new constructor with v2.5
-- INPUT:
-- 1) polynomial ring (over a field)
-- 2) hash table for raw character: (homdeg,deg) => character matrix
character(PolynomialRing,HashTable) := Character => op -> (R,H) -> (
    -- check polynomial ring is over a field
    F := coefficientRing R;
    if not isField F then (
	error "character: expected degree ring over a field";
	);
    -- check keys are in the right format
    k := keys H;
    if any(k, i -> class i =!= Sequence or #i != 2 or
	class i#0 =!= ZZ or class i#1 =!= List) then (
	error "character: expected keys of the form (ZZ,List)";
	);
    -- check if character ring is already present
    -- if not, construct it and cache it
    if not R#?cache then R#cache = new CacheTable;
    if not R#cache#?degreesRing then (
	R#cache#degreesRing = F degreesMonoid R;
	);
    DR := R#cache#degreesRing;
    -- get degree length
    dl := numgens DR;
    -- check degree vectors are allowed
    degs := apply(k,last);
    if any(degs, i -> #i != dl or any(i, j -> class j =!= ZZ)) then (
	error ("character: expected integer degree vectors of length " | toString(dl));
	);
    -- check character vectors are allowed
    v := values H;
    if any(v, i -> class i =!= Matrix) then (
	error "character: expected characters to be matrices";
	);
    if any(v, i -> numRows i != 1) then (
	error ("character: expected characters to be one-row matrices");
	);
    cl := numColumns first v;
    if any(v, i -> numColumns i != cl) then (
	error ("character: expected matrices to have the same number of columns");
	);
    -- move character values into given ring
    H = try applyValues(H, v -> promote(v,F)) else (
	error "character: could not promote characters to field of coefficients";
	);
    -- partition keys by hom degree, then extract internal degree
    -- so {(0,{0}),(0,{1})} goes to 0=>{(0,{0}),(0,{1})}
    pk := partition(i -> i#0,k);
    -- for each hom degree, multiply each char matrix with degree monomial
    -- then add them all
    H' := applyValues(pk, l -> sum apply(l, d -> (H#d) * DR_(d#1)) );
    new Character from {
	cache => new CacheTable,
	(symbol degreesRing) => DR,
	(symbol degreeOrbit) => first op.Semidirect,
	(symbol degreeRepresentative) => last op.Semidirect,
	(symbol numActors) => cl,
	(symbol characters) => H',
	}
    )


-- equality for characters as raw hash tables
Character == Character := (A,B) -> A === B

-- direct sum of characters
-- modeled after code in Macaulay2/Core/matrix.m2
-- plus and + added after v2.1 to match difference
Character + Character := Character => directSum
plus(Character,Character) := Character => directSum
Character ++ Character := Character => directSum
directSum Character := c -> Character.directSum (1 : c)
Character.directSum = args -> (
    -- check degreesRing is the same for all summands
    DR := (args#0).degreesRing;
    if any(args, c -> c.degreesRing =!= DR)
    then error "directSum: expected characters over the same ring";
    -- check character length is the same for all summands
    cl := (args#0).numActors;
    if any(args, c -> c.numActors != cl)
    then error "directSum: expected characters all of the same length";
    -- check degreeOrbit is the same for all summands
    degOrb := (args#0).degreeOrbit;
    if any(args, c -> c.degreeOrbit =!= degOrb)
    then error "directSum: characters have different degree orbit functions";
    -- check degreeRepresentative is the same for all summands
    degRep := (args#0).degreeRepresentative;
    if any(args, c -> c.degreeRepresentative =!= degRep)
    then error "directSum: characters have different degree representative functions";
    -- raw character of direct sum (could have zero entries)
    H := fold( (c1,c2) -> merge(c1,c2,plus), apply(args, c -> c.characters) );
    new Character from {
	cache => new CacheTable,
	(symbol degreesRing) => DR,
	(symbol degreeOrbit) => degOrb,
	(symbol degreeRepresentative) => degRep,
	(symbol numActors) => cl,
	-- add raw characters
	(symbol characters) => applyPairs(H,(k,v)->if not zero v then (k,v)),
	}
    )

-- function to multiply character matrices (Hadamard product)
multiplyCharacters = (c1,c2) -> (
    e1 := flatten entries c1;
    e2 := flatten entries c2;
    m := apply(e1,e2,times);
    -- ensure char matrix has right degrees
    map(target c1,source c1,matrix{m})
    )

-- tensor product of characters
-- modeled after directSum, but only works for two characters
Character ** Character := Character => tensor
tensor(Character,Character) := Character => {} >> opts -> (c1,c2) -> (
    -- check degreesRing is the same for all factors
    DR := c1.degreesRing;
    if (c2.degreesRing =!= DR)
    then error "tensor: expected characters all over the same ring";
    -- check character length is the same for all summands
    cl := c1.numActors;
    if (c2.numActors != cl)
    then error "tensor: expected characters all of the same length";
    -- check degreeOrbit is the same for all summands
    degOrb := c1.degreeOrbit;
    if (c2.degreeOrbit =!= degOrb)
    then error "tensor: characters have different degree orbit functions";
    -- check degreeRepresentative is the same for all summands
    degRep := c1.degreeRepresentative;
    if (c2.degreeRepresentative =!= degRep)
    then error "tensor: characters have different degree representative functions";
    -- raw character of tensor product (may contain zeros)
    -- homological degrees should be added, hence the first plus
    -- raw characters should be Hadamard multiplied
    -- if different homological degrees add up to the same value,
    -- the corresponding characters should be added, hence the last plus
    H := combine(c1.characters,c2.characters,plus,multiplyCharacters,plus);
    new Character from {
	cache => new CacheTable,
	(symbol degreesRing) => DR,
	(symbol degreeOrbit) => degOrb,
	(symbol degreeRepresentative) => degRep,
	(symbol numActors) => cl,
	-- multiply raw characters
	(symbol characters) => applyPairs(H,(k,v)->if not zero v then (k,v))
	}
    )

-- tensor power (new in v2.5)
-- M2 uses BinaryPowerMethod in M2/Macaulay2/d/actors.d
-- to construct tensor powers of rings, modules, etc.
-- however, this requires a constructor for the inverse/dual
-- which in the case of characters requires additional user input
-- we define a recursive tensor power only for positive exponents
Character ^** ZZ := Character => (c,n) -> (
    -- return error for negative exponents
    if n < 0 then (
	error "Character ^** ZZ: not implemented for negative exponents; use dual";
	)
    -- for n=0, return trivial character in hom degree zero
    else if n == 0 then (
	H := hashTable {
	    (0,matrix{toList(c.numActors:1_(c.degreesRing))})
	    };
	new Character from {
	    cache => new CacheTable,
	    (symbol degreesRing) => c.degreesRing,
	    (symbol degreeOrbit) => c.degreeOrbit,
	    (symbol degreeRepresentative) => c.degreeRepresentative,
	    (symbol numActors) => c.numActors,
	    (symbol characters) => H
	    }
	)
    -- for n=1, return original character
    else if n == 1 then (
	c
	)
    -- for n>=2, then reduce to lower power
    else (
	tensor(c, c ^** (n-1) )
	)
    )

-- shift homological degree of characters
Character Array := Character => (c,A) -> (
    if # A =!= 1 then error "Character Array: expected array of length 1";
    n := A#0;
    if not instance(n,ZZ) then error "Character Array: expected an integer";
    new Character from {
	cache => new CacheTable,
	(symbol degreesRing) => c.degreesRing,
	(symbol degreeOrbit) => c.degreeOrbit,
	(symbol degreeRepresentative) => c.degreeRepresentative,
	(symbol numActors) => c.numActors,
	-- homological shift raw characters
	(symbol characters) => applyKeys(c.characters,
	    k -> k - n)
	}
    )

-- character dual
-- borrowing default options from alexander dual method
alexopts = {Strategy=>0};

-- character of dual/contragredient representation with conjugation
dual(Character,RingMap) := Character => alexopts >> o -> (c,phi) -> (
    -- check characteristic
    DR := c.degreesRing;
    F := coefficientRing DR;
    if char(F) != 0 then (
	error "dual: use permutation constructor in positive characteristic";
	);
    -- check conjugation map
    if (source phi =!= F or target phi =!= F or phi^2 =!= id_F) then (
	error "dual: expected an order 2 automorphism of the base field";
	);
    -- ring map that inverts variables in degree ring
    -- this sends a degree T^d to its opposite T^(-d)
    inv := map(DR,DR,apply(gens DR, T -> T^(-1)));
    -- create hash table for raw dual
    H := applyPairs(c.characters,
	(k,v) -> (
	    (M,C) := coefficients v;
	    M = inv M;
	    C = promote(phi(lift(C,F)),DR);
	    (-k, M*C)
	    )
	);
    new Character from {
	cache => new CacheTable,
	(symbol degreesRing) => c.degreesRing,
	(symbol degreeOrbit) => c.degreeOrbit,
	(symbol degreeRepresentative) => c.degreeRepresentative,
	(symbol numActors) => c.numActors,
	(symbol characters) => H
	}
    )

-- character of dual/contragredient representation without conjugation
dual(Character,List) := Character => alexopts >> o -> (c,perm) -> (
    n := c.numActors;
    if #perm != n then (
	error "dual: expected permutation size to match character length";
	);
    -- check permutation has the right entries
    if set perm =!= set(1..n) then (
	error ("dual: expected a permutation of {1,..," | toString(n) | "}");
	);
    -- ring map that inverts variables in degree ring
    -- this sends a degree T^d to its opposite T^(-d)
    DR := c.degreesRing;
    inv := map(DR,DR,apply(gens DR, T -> T^(-1)));
    new Character from {
	cache => new CacheTable,
	(symbol degreesRing) => c.degreesRing,
	(symbol degreeOrbit) => c.degreeOrbit,
	(symbol degreeRepresentative) => c.degreeRepresentative,
	(symbol numActors) => n,
	-- dual lives in opposite homological dimension
	-- for internal degrees apply the inversion map
	-- permute columns for values of dual character
	(symbol characters) => applyPairs(c.characters,
	    (k,v) -> (-k, (inv v)_(apply(perm, i -> i-1)))
	    )
	}
    )

-- extract character by homological dimension (added after v2.1)
Character _ ZZ := Character => (c,i) -> (
    H := select(pairs c.characters, p -> first p == i);
    new Character from {
	cache => new CacheTable,
	(symbol degreesRing) => c.degreesRing,
	(symbol degreeOrbit) => c.degreeOrbit,
	(symbol degreeRepresentative) => c.degreeRepresentative,
	(symbol numActors) => c.numActors,
	(symbol characters) => hashTable H
	}    
    )

-- extract several characters by hom dim (added after v2.1)
Character _ List := Character => (c,l) -> (
    if any(l, i -> not instance(i,ZZ)) then (
	error "Character_List: expected a list of integers";
	);
    -- unique avoids adding pieces of the character multiple times
    directSum(apply(unique l, i -> c_i))
    )

-- extract characters by degree (added after v2.1)
Character ^ List := Character => (c,degs) -> (
    -- if single degree, repackage as list (defer checks)
    if any(degs,i->not instance(i,List)) then (
	degs = {degs};
	);
    -- check all degrees are compatible
    dl := numgens c.degreesRing;
    if all(degs,d->all(d,i->instance(i,ZZ)) and #d==dl) then (
	-- find all degrees in the orbit of degs and remove duplicates
	exps := unique flatten apply(degs, d -> c.degreeOrbit d);
	-- get corresponding monomials in character ring
	DR := c.degreesRing;
	mons := apply(exps, e -> DR_e);
	-- extract those monomials from character
	H := applyValues(c.characters, v -> (
		(M,C) := coefficients(v,Monomials=>mons);
		M*C
		)
	    );
    	return new Character from {
	    cache => new CacheTable,
	    (symbol degreesRing) => c.degreesRing,
	    (symbol degreeOrbit) => c.degreeOrbit,
	    (symbol degreeRepresentative) => c.degreeRepresentative,
	    (symbol numActors) => c.numActors,
	    (symbol characters) => H
	    }    
	) else (
	error ("Character^List: expected a (list of) (multi)degree(s) of length " | toString(dl));
	);
    )

-- multiplication of character with a scalar (added after v2.1)
ZZ * Character :=
QQ * Character :=
RingElement * Character := Character => (r,c) -> (
    try a := promote(r,c.degreesRing) else (
	error "RingElement*Character: could not promote scalar to field of character";
	);
    H := applyPairs(c.characters,(k,v)->(
	    w := a*v;
	    if not zero w then (k,w)
	    )
	);
    new Character from {
	cache => new CacheTable,
	(symbol degreesRing) => c.degreesRing,
	(symbol degreeOrbit) => c.degreeOrbit,
	(symbol degreeRepresentative) => c.degreeRepresentative,
	(symbol numActors) => c.numActors,
	(symbol characters) => H
	}    
    )

-- for commutativity
Character * ZZ :=
Character * QQ :=
Character * RingElement := Character => (c,r) -> r*c

-- additive inverse of a character (added after v2.1)
- Character :=
minus Character := Character => c -> (
    new Character from {
	cache => new CacheTable,
	(symbol degreesRing) => c.degreesRing,
	(symbol degreeOrbit) => c.degreeOrbit,
	(symbol degreeRepresentative) => c.degreeRepresentative,
	(symbol numActors) => c.numActors,
	(symbol characters) => applyValues(c.characters,v->-v)
	}    
    )

-- difference of characters (added after v2.1)
Character - Character :=
difference(Character,Character) := Character =>
(c1,c2) -> directSum(c1,-c2)



----------------------------------------------------------------------
-- Character tables and decompositions -------------------------------
----------------------------------------------------------------------
    
-- method to construct character tables
characterTable = method(TypicalValue=>CharacterTable,Options=>{Labels => {}});

-- character table constructor using conjugation
-- modified after v2.1 to be defined over a field
-- modified after v2.1 to allow TeX labels
-- INPUT:
-- 1) list of conjugacy class sizes
-- 2) matrix of irreducible character values
-- 3) field over which to construct the table
-- 4) ring map, conjugation of coefficients
-- OPTIONAL: lists of labels for irreducible characters
characterTable(List,Matrix,Ring,RingMap) := CharacterTable =>
o -> (conjSize,charTable,F,phi) -> (
    -- check third argument is a field
    if not isField F then (
	error "characterTable: expected third argument to be a field";
	);
    -- check characteristic
    if char(F) != 0 then (
	error "characterTable: use permutation constructor in positive characteristic";
	);
    n := #conjSize;
    -- check all arguments have the right size
    if numRows charTable != n or numColumns charTable != n then (
	error "characterTable: expected matrix size to match number of conjugacy classes";
	);
    -- promote character matrix to F
    X := try promote(charTable,F) else (
	error "characterTable: could not promote character table to given field";
	);
    if (source phi =!= F or target phi =!= F or phi^2 =!= id_F) then (
	error "characterTable: expected an order 2 automorphism of the coefficient ring";
	);
    -- check orthogonality relations
    ordG := sum conjSize;
    C := diagonalMatrix(F,conjSize);
    m := C*transpose(phi charTable);
    -- if x is a character in a one-row matrix, then x*m is the one-row matrix
    -- containing the inner products of x with the irreducible characters
    if X*m != ordG*map(F^n) then (
	error "characterTable: orthogonality relations not satisfied";
	);
    -- get user labels or create default ones
    if o.Labels == {} then (
    	netLabels := for i to n-1 list net(expression("ꭓ")_(expression i));
	texLabels := for i to n-1 list ("\\chi_{" | toString(i) | "}");
	)
    else if (#o.Labels == 2 and all(o.Labels,x -> class x === List)) then (
	netLabels = first o.Labels;
	texLabels = last o.Labels;
	)
    else (
	netLabels = o.Labels;
	texLabels = o.Labels;
	);
    -- check labels have the right format
    if (#netLabels != n or #texLabels != n) then (
	error ("characterTable: expected " | toString(n) | " labels");
	);
    if not all(netLabels, i -> instance(i, Net)) then (
	error "characterTable: expected labels to be strings (or nets)";	    
	);
    if not all(texLabels, i -> instance(i, Net)) then (
	error "characterTable: expected labels to be strings (or nets)";	    
	);
    new CharacterTable from {
	(symbol numActors) => #conjSize,
	(symbol size) => conjSize,
	(symbol table) => X,
	(symbol ring) => F,
	(symbol matrix) => m,
	(symbol Labels) => {netLabels,texLabels},
	}
    )

-- character table constructor without conjugation
-- modified after v2.1 to be defined over a field
-- modified after v2.1 to allow TeX labels
-- INPUT:
-- 1) list of conjugacy class sizes
-- 2) matrix of irreducible character values
-- 3) field over which to construct the table
-- 4) list, permutation of conjugacy class inverses
-- OPTIONAL: lists of labels for irreducible characters
characterTable(List,Matrix,Ring,List) := CharacterTable =>
o -> (conjSize,charTable,F,perm) -> (
    -- check third argument is a field
    if not isField F then (
	error "characterTable: expected third argument to be a field";
	);
    n := #conjSize;
    -- check all arguments have the right size
    if numRows charTable != n or numColumns charTable != n then (
	error "characterTable: expected matrix size to match number of conjugacy classes";
	);
    if #perm != n then (
	error "characterTable: expected permutation size to match number of conjugacy classes";
	);
    -- promote character matrix to F
    X := try promote(charTable,F) else (
	error "characterTable: could not promote character table to given field";
	);
    -- check permutation has the right entries
    if set perm =!= set(1..n) then (
	error ("characterTable: expected a permutation of {1,..," | toString(n) | "}");
	);
    -- check characteristic
    ordG := sum conjSize;
    if ordG % char(F) == 0 then (
	error "characterTable: characteristic divides order of the group";
	);
    -- check orthogonality relations
    C := diagonalMatrix(F,conjSize);
    P := map(F^n)_(apply(perm, i -> i-1));
    m := C*transpose(X*P);
    -- if x is a character in a one-row matrix, then x*m is the one-row matrix
    -- containing the inner products of x with the irreducible characters
    if X*m != ordG*map(F^n) then (
	error "characterTable: orthogonality relations not satisfied";
	);
    -- get user labels or create default ones
    if o.Labels == {} then (
    	netLabels := for i to n-1 list net(expression("ꭓ")_(expression i));
	texLabels := for i to n-1 list ("\\chi_{" | toString(i) | "}");
	)
    else if (#o.Labels == 2 and all(o.Labels,x -> class x === List)) then (
	netLabels = first o.Labels;
	texLabels = last o.Labels;
	)
    else (
	netLabels = o.Labels;
	texLabels = o.Labels;
	);
    -- check labels have the right format
    if (#netLabels != n or #texLabels != n) then (
	error ("characterTable: expected " | toString(n) | " labels");
	);
    if not all(netLabels, i -> instance(i, Net)) then (
	error "characterTable: expected labels to be strings (or nets)";	    
	);
    if not all(texLabels, i -> instance(i, Net)) then (
	error "characterTable: expected labels to be strings (or nets)";	    
	);
    new CharacterTable from {
	(symbol numActors) => #conjSize,
	(symbol size) => conjSize,
	(symbol table) => X,
	(symbol ring) => F,
	(symbol matrix) => m,
	(symbol Labels) => {netLabels,texLabels},
	}
    )

-- equality for character tables as raw hash tables
CharacterTable == CharacterTable := (A,B) -> A === B

-- new method for character decomposition
decomposeCharacter = method(TypicalValue=>CharacterDecomposition);

-- decompose a character against a character table
decomposeCharacter(Character,CharacterTable) :=
CharacterDecomposition => (C,T) -> (
    -- check character and table are over same ring
    F := coefficientRing C.degreesRing;
    if T.ring =!= F then (
	error "decomposeCharacter: expected character and table over the same field";
	);
    -- check number of actors is the same
    if C.numActors != T.numActors then (
	error "decomposeCharacter: character length does not match table";
	);
    -- order of the group = sum of conjugacy class sizes
    ord := sum T.size;
    -- create decomposition hash table
    D := applyValues(C.characters, char -> 1/ord*char*T.matrix);
    new CharacterDecomposition from {
	cache => new CacheTable,
	(symbol numActors) => C.numActors,
	(symbol degreesRing) => C.degreesRing,
	(symbol degreeOrbit) => C.degreeOrbit,
	(symbol degreeRepresentative) => C.degreeRepresentative,
	(symbol decompose) => D,
	(symbol Labels) => T.Labels
	}
    )

-- shortcut for character decomposition
Character / CharacterTable := CharacterDecomposition => decomposeCharacter

-- recreate a character from decomposition
character(CharacterDecomposition,CharacterTable) :=
Character => op -> (D,T) -> (
    -- check decomposition and table are over same ring
    F := coefficientRing D.degreesRing;
    if T.ring =!= F then (
	error "character: expected decomposition and table over the same field";
	);
    -- check number of actors is the same
    if D.numActors != T.numActors then (
	error "character: decomposition and table have different number of actors";
	);
    new Character from {
	cache => new CacheTable,
	(symbol degreesRing) => D.degreesRing,
	(symbol degreeOrbit) => D.degreeOrbit,
	(symbol degreeRepresentative) => D.degreeRepresentative,
	(symbol numActors) => D.numActors,
	(symbol characters) => applyValues(D.decompose, i -> i*T.table),
	}
    )

-- shortcut to recreate character from decomposition
CharacterDecomposition * CharacterTable := Character => character

-- equality for character decompositions as raw hash tables
CharacterDecomposition == CharacterDecomposition := (A,B) -> A === B

----------------------------------------------------------------------
-- Actions on complexes and characters of complexes ------------------
----------------------------------------------------------------------

-- constructor for action on resolutions and modules
-- optional argument Sub=>true means ring actors are passed
-- as one-row matrices of substitutions, Sub=>false means
-- ring actors are passed as matrices
-- Semidirect option (added after v2.2)
action = method(TypicalValue=>Action,
    Options=>{Sub=>true,Semidirect=>(bracketize,identity)})

-- constructor for action on resolutions
-- INPUT:
-- 1) a resolution
-- 2) a list of actors on the ring variables
-- 3) a list of actors on the i-th module of the resolution
-- 4) homological index i
action(Complex,List,List,ZZ) := ActionOnComplex => op -> (C,l,l0,i) -> (
    --check C is a homogeneous complex over a poly ring over a field
    --NOTE: minimality is necessary, but assumed
    R := ring C;
    if not isPolynomialRing R then (
	error "action: expected a complex over a polynomial ring";
	);
    F := coefficientRing R;
    if not isField F then (
	error "action: expected coefficients in a field";
	);
    if not all(length C,i -> isFreeModule C_(i+min(C))) then (
	error "action: expected a complex of free modules";
	);
    if not isHomogeneous C then (
	error "action: complex is not homogeneous";
	);
    --check the matrix of the action on the variables has right size
    n := numgens R;
    if not all(l,g->numColumns(g)==n) then (
	error "action: ring actor matrix has wrong number of columns";
	);
    --move ring actors to ring for uniformity
    l = try apply(l, g -> promote(g,R)) else (
	error "action: could not promote actors to ring of complex";
	);
    if op.Sub then (
	--if ring actors are substitutions, they must be one-row matrices
	if not all(l,g->numRows(g)==1) then (
	    error "action: expected ring actors to be a one-row matrices";
	    );
	) else (
	--if ring actors are matrices, they must be square
	if not all(l,g->numRows(g)==n) then (
	    error "action: ring actor matrix has wrong number of rows";
	    );
	--convert them to substitutions
	l = apply(l, g -> (vars R) * g);
	);
    --check list of group elements has same length
    if #l != #l0 then (
	error "action: lists of actors must have equal length";
	);
    -- check if character ring is already present
    -- if not, construct it and cache it
    if not R#?cache then R#cache = new CacheTable;
    if not R#cache#?degreesRing then (
	R#cache#degreesRing = F degreesMonoid R;
	);
    DR := R#cache#degreesRing;
    --check size of module actors matches rank of starting module
    r := rank C_i;
    if not all(l0,g->numColumns(g)==r and numRows(g)==r) then (
	error "action: module actor matrix has wrong number of rows or columns";
	);
    --store everything into a hash table
    new ActionOnComplex from {
	cache => new CacheTable from {
	    (symbol actors,i) => apply(l0,g->map(C_i,C_i,g))
	    },
	(symbol ring) => R,
	(symbol target) => C,
	(symbol numActors) => #l,
	(symbol ringActors) => l,
	(symbol degreesRing) => DR,
	(symbol degreeOrbit) => first op.Semidirect,
	(symbol degreeRepresentative) => last op.Semidirect,
	}
    )

-- shortcut constructor for resolutions of quotient rings
-- actors on generator are assumed to be trivial
action(Complex,List) := ActionOnComplex => op -> (C,l) -> (
    R := ring C;
    l0 := toList(#l:(id_(R^1)));
    action(C,l,l0,min C,Sub=>op.Sub,Semidirect=>op.Semidirect)
    )

-- equality check for actions on complexes
-- user provided action is stored in cache because user
-- may provide initial action in different homological degrees
-- then it is not enough to compare as raw hash tables
-- so we compare actors in all homological degrees
ActionOnComplex == ActionOnComplex := (A,B) -> (
    -- first compare raw hash tables
    if A =!= B then return false;
    -- if same, compare action which is stored in cache
    C := A.target;
    all(min C .. max C, i -> actors(A, i) == actors(B, i))
    )

-- returns number of actors
numActors = method(TypicalValue=>ZZ)
numActors(Action) := ZZ => A -> A.numActors

-- returns action on ring variables
-- Sub=>true returns one-row substitution matrices
-- Sub=>false returns square matrices
ringActors = method(TypicalValue=>List,Options=>{Sub=>true})
ringActors(Action) := List => op -> A -> (
    if not op.Sub then (
	GB := gb(vars A.ring,StopWithMinimalGenerators=>true,ChangeMatrix=>true);
	apply(A.ringActors,g->g//GB)
	)
    else A.ringActors
    )

-- returns various group actors
actors = method(TypicalValue=>List)

-- returns actors on resolution in a given homological degree
-- if homological degree is not the one passed by user upon construction,
-- the actors are computed and stored
actors(ActionOnComplex,ZZ) := List => (A,i) -> (
    -- if not cached, compute
    if not A.cache#?(symbol actors,i) then (
	-- homological degrees where action is already cached
	places := apply(select(keys A.cache, k -> instance(k,Sequence) and k#0 == symbol actors), k -> k#1);
	-- get the complex
	C := A.target;
	-- if zero in that hom degree, return zeros
	if zero(C_i) then return toList(A.numActors:map(C_i));
	-- if hom degree is to the right of previously computed
	if i > max places then (
	    -- compute GB of differential but only up to min gens
	    -- NOTE: does not work if ChangeMatrix=>false (which is default)
	    GB := gb(C.dd_i,StopWithMinimalGenerators=>true,ChangeMatrix=>true);
	    A.cache#(symbol actors,i) =
	    apply(A.ringActors, actors(A,i-1),
		-- given a map of free modules C.dd_i : F <-- F',
		-- the group action on the ring (as substitution)
		-- and the group action on F, computes the group action on F'
		(g,g0) -> g0*sub(C.dd_i,g)//GB
		);
	    )
	-- if hom degree is to the left of previously computed
	else (
	    -- may need to compute inverse of ring actors
	    if not A.cache.?inverse then (
		--convert variable substitutions to matrices
		--then invert and convert back to substitutions
		R := A.ring;
		b := gb(vars R,StopWithMinimalGenerators=>true,ChangeMatrix=>true);
		A.cache.inverse = apply(A.ringActors, g ->
		    (vars R) * (inverse lift(g//b,coefficientRing R))
		    );
		);
	    GB = gb(transpose(C.dd_(i+1)),StopWithMinimalGenerators=>true,ChangeMatrix=>true);
	    A.cache#(symbol actors,i) =
	    apply(A.cache.inverse,actors(A,i+1),
		-- given a map of free modules C.dd_i : F <-- F',
		-- the inverse group action on the ring (as substitution)
		-- and the group action on F', computes the group action on F
		(gInv,g0) -> (
		    transpose(transpose(sub(C.dd_(i+1),gInv)*g0)//GB)
		    )
		);
	    );
	);
    -- return cached value
    A.cache#(symbol actors,i)
    )

-- return the character of one free module of a resolution
-- in a given homological degree
character(ActionOnComplex,ZZ) := Character => op -> (A,i) -> (
    -- if not cached, compute
    if not A.cache#?(symbol character,i) then (
	F := coefficientRing A.ring;
	DR := A.degreesRing;
	n := A.numActors;
	-- if complex is zero in hom degree i, return empty character, don't cache
	if zero (A.target)_i then (
	    return new Character from {
		cache => new CacheTable,
		(symbol degreesRing) => DR,
		(symbol degreeOrbit) => A.degreeOrbit,
		(symbol degreeRepresentative) => A.degreeRepresentative,
		(symbol numActors) => n,
		(symbol characters) => hashTable {},
		};
	    );
	-- create raw character from actors
	a := actors(A,i);
	r := rank((A.target)_i) - 1;
	-- for each basis element extract corresponding diagonal entry
	-- put it in a row matrix and multiply by degree, then add
	-- this will give the graded raw character as a matrix
	raw := sum parallelApply(toList(0..r), j -> (
		d := degree( ((A.target)_i)_j );
		lift(matrix{apply(a, g -> g_(j,j) )},F) * (DR_d)
		)
	    );
	-- cache character
	A.cache#(symbol character,i) = 	new Character from {
	    cache => new CacheTable,
	    (symbol degreesRing) => DR,
	    (symbol degreeOrbit) => A.degreeOrbit,
	    (symbol degreeRepresentative) => A.degreeRepresentative,
	    (symbol numActors) => A.numActors,
	    (symbol characters) => hashTable {i=>raw},
	    };
	);
    -- return cached value
    A.cache#(symbol character,i)
    )

-- return characters of all free modules in a resolution
-- by repeatedly using previous function
character ActionOnComplex := Character => op -> A -> (
    C := A.target;
    directSum for i from min(C) to min(C)+length(C) list character(A,i)
    )

----------------------------------------------------------------------
-- Actions on modules and characters of modules ----------------------
----------------------------------------------------------------------

-- constructor for action on various kinds of graded modules
-- INPUT:
-- 1) a graded module (polynomial ring or quotient, module, ideal)
-- 2) a list of actors on the ring variables
-- 3) a list of actors on the generators of the ambient free module
action(PolynomialRing,List,List) :=
action(QuotientRing,List,List) :=
action(Ideal,List,List) :=
action(Module,List,List) := ActionOnGradedModule => op -> (M,l,l0) -> (
    -- check M is graded over a poly ring over a field
    -- the way to get the ring depends on the class of M
    if instance(M,Ring) then (
	R := ambient M;
	) else (
	R = ring M;
	);
    if not isPolynomialRing R then (
	error "action: expected a module/ideal/quotient over a polynomial ring";
	);
    F := coefficientRing R;
    if not isField F then (
	error "action: expected coefficients in a field";
	);
    if not isHomogeneous M then (
	error "action: module/ideal/quotient is not graded";
	);
    --check matrix of action on variables has right size
    n := numgens R;
    if not all(l,g->numColumns(g)==n) then (
	error "action: ring actor matrix has wrong number of columns";
	);
    --move ring actors to ring for uniformity
    l = try apply(l, g -> promote(g,R)) else (
	error "action: could not promote actors to ring of module";
	);
    if op.Sub then (
	--if ring actors are substitutions, they must be one-row matrices
    	if not all(l,g->numRows(g)==1) then (
	    error "action: expected ring actors to be a one-row matrices";
	    );
	) else (
	--if ring actors are matrices, they must be square
	if not all(l,g->numRows(g)==n) then (
	    error "action: ring actor matrix has wrong number of rows";
	    );
	--convert them to substitutions
	l = apply(l, g -> (vars R) * g);
	);
    --check list of group elements has same length
    if #l != #l0 then (
	error "action: lists of actors must have equal length";
	);
    --check size of module actors matches rank of ambient module
    if instance(M,Module) then (
    	A := ambient M;
	) else ( A = R^1; );
    r := rank A;
    if not all(l0,g->numColumns(g)==r and numRows(g)==r) then (
	error "action: module actor matrix has wrong number of rows or columns";
	);
    --turn input object into a module M'
    if instance(M,QuotientRing) then (
	M' := coker presentation M;
	) else if instance(M,Module) then (
	M' = M;
	) else (
	M' = module M;
	);
    -- check if character ring is already present
    -- if not, construct it and cache it
    if not R#?cache then R#cache = new CacheTable;
    if not R#cache#?degreesRing then (
	R#cache#degreesRing = F degreesMonoid R;
	);
    DR := R#cache#degreesRing;
    --store everything into a hash table
    new ActionOnGradedModule from {
	cache => new CacheTable,
	(symbol ring) => R,
	(symbol target) => M,
	(symbol module) => M',
	(symbol numActors) => #l,
	(symbol ringActors) => l,
	(symbol actors) => apply(l0,g->map(A,A,g)),
	(symbol relations) => gb image relations M',
	(symbol degreesRing) => DR,
	(symbol degreeOrbit) => first op.Semidirect,
	(symbol degreeRepresentative) => last op.Semidirect,
	}
    )

-- shortcut constructor when actors on generator are trivial
action(PolynomialRing,List) :=
action(QuotientRing,List) :=
action(Ideal,List) :=
action(Module,List) := ActionOnGradedModule => op -> (M,l) -> (
    if instance(M,Module) then (
    	l0 := toList(#l:(id_(ambient M)));
	) else if instance(M,Ideal) then (
    	l0 = toList(#l:(id_(ambient module M)));	
	) else (
    	l0 = toList(#l:(id_(module ambient M)));	
	);
    action(M,l,l0,Sub=>op.Sub,Semidirect=>op.Semidirect)
    )

-- equality check for actions on graded modules
-- since the user provided action on generators is stored
-- it is enough to compare as raw hash tables
ActionOnGradedModule == ActionOnGradedModule := (A,B) -> A === B

-- returns actors on component of given multidegree
-- the actors are computed and stored
actors(ActionOnGradedModule,List) := List => (A,d) -> (
    -- ensure function is computed with rep of degree orbit
    degRep := A.degreeRepresentative d;
    -- if not cached, compute
    if not A.cache#?(symbol actors,degRep) then (
	M := A.module;
	-- get basis in degree d as map of free modules
	-- (after semidirect: single degree d replaced by degree orbit)
	degList := A.degreeOrbit d;
	-- collect bases for degrees in orbit and join horizontally
	b := rsort fold( (x,y) -> x|y, apply(degList, d -> ambient basis(d,M)));
	-- the basis command returns a matrix with columns in decreasing order
	-- joining basis from different degrees may break this order
	-- the rsort at the beginning recovers M2's default order
	-- this sorting was made necessary after introducing semidirect options
	-- actors matrix would be useless without out as it may not match basis
	if zero b then (
	    A.cache#(symbol actors,degRep) = toList(A.numActors:map(source b));
	    )
	else (
	    GB := gb(b,StopWithMinimalGenerators=>true,ChangeMatrix=>true);
	    A.cache#(symbol actors,degRep) =
		apply(A.ringActors, A.actors,
		--g0*b acts on the basis of the ambient module
		--sub(-,g) acts on the polynomial coefficients
		--result must be reduced against module relations
		--then factored by original basis to get action matrix
		(g,g0) -> (sub(g0*b,g) % A.relations) // GB
	    );
	);
    );
    -- return cached value
    A.cache#(symbol actors,degRep)
    )

-- returns actors on component of given degree
actors(ActionOnGradedModule,ZZ) := List => (A,d) -> actors(A,{d})

-- return character of component of given multidegree
character(ActionOnGradedModule,List) := Character => op -> (A,d) -> (
    -- ensure function is computed with rep of degree orbit
    degRep := A.degreeRepresentative d;
    -- if not cached, compute
    if not A.cache#?(symbol character,degRep) then (
	F := coefficientRing A.ring;
	DR := A.degreesRing;
	-- zero action, return empty character and don't cache
	acts := actors(A,degRep);
	if all(acts,zero) then (
	    return new Character from {
		cache => new CacheTable,
		(symbol degreesRing) => DR,
		(symbol degreeOrbit) => A.degreeOrbit,
		(symbol degreeRepresentative) => A.degreeRepresentative,
		(symbol numActors) => A.numActors,
		(symbol characters) => hashTable {},
		};
	    );
	-- otherwise make character of A in degree d
	-- create raw character from actors
	r := (numRows first acts) - 1;
	degList := degrees source first acts;
	-- for each basis element extract corresponding diagonal entry
	-- put it in a row matrix and multiply by degree, then add
	-- this will give the graded raw character as a matrix
	raw := sum parallelApply(toList(0..r), j -> (
		d := degList_j;
		lift(matrix{apply(acts, g -> g_(j,j) )},F) * (DR_d)
		)
	    );
	A.cache#(symbol character,degRep) = new Character from {
		cache => new CacheTable,
		(symbol degreesRing) => DR,
		(symbol degreeOrbit) => A.degreeOrbit,
		(symbol degreeRepresentative) => A.degreeRepresentative,
		(symbol numActors) => A.numActors,
		(symbol characters) => hashTable {0 => raw},
		};
	);
    -- return cached value
    A.cache#(symbol character,degRep)
    )

-- return character of component of given degree
character(ActionOnGradedModule,ZZ) := Character => op -> (A,d) -> (
    character(A,{d})
    )

-- return character of components in a range of degrees
character(ActionOnGradedModule,ZZ,ZZ) := Character => op -> (A,lo,hi) -> (
    if degreeLength A.ring != 1 then (
	error "character: expected module over a ZZ-graded polynomial ring";
    	);
    directSum for d from lo to hi list character(A,d)
    )

---------------------------------------------------------------------
-- Specialized functions for symmetric groups -----------------------
---------------------------------------------------------------------

-- take r boxes from partition mu along border
-- unexported auxiliary function for Murnaghan-Nakayama
strip := (mu,r) -> (
    -- if one row, strip r boxes
    if #mu == 1 then return {mu_0 - r};
    -- if possible, strip r boxes in 1st row
    d := mu_0 - mu_1;
    if d >= r then (
	return {mu_0 - r} | drop(mu,1);
	);
    -- else, remove d+1 boxes and iterate
    {mu_0-d-1} | strip(drop(mu,1),r-d-1)
    )

-- irreducible Sn character chi^lambda
-- evaluated at conjugacy class of cycle type rho
-- unexported
murnaghanNakayama := (lambda,rho) -> (
    -- if both empty, character is 1
    if lambda == {} and rho == {} then return 1;
    r := rho#0;
    -- check if border strip fits ending at each row
    borderStrips := select(
	-- for all c remove first c parts, check if strip fits in the rest
	for c to #lambda-1 list (take(lambda,c) | strip(drop(lambda,c),r)),
	-- function that checks if list is a partition (0 allowed)
    	mu -> (
    	    -- check no negative parts
    	    if any(mu, i -> i<0) then return false;
    	    -- check non increasing
    	    for i to #mu-2 do (
	    	if mu_i < mu_(i+1) then return false;
	    	);
    	    true
    	    )
    	);
    -- find border strip height
    heights := apply(borderStrips,
	bs -> number(lambda - bs, i -> i>0) - 1);
    -- recursive computation
    rho' := drop(rho,1);
    sum(borderStrips,heights, (bs,h) ->
	(-1)^h * murnaghanNakayama(delete(0,bs),rho')
	)
    )

-- speed up computation by caching values
murnaghanNakayama = memoize murnaghanNakayama

-- symmetric group character table
symmetricGroupTable = method(TypicalValue=>CharacterTable);
symmetricGroupTable(ZZ,Ring) := (n,F) -> (
    -- check n is at least one
    if n < 1 then (
	error "symmetricGroupTable: expected first argument to be a positive integer";
	);
    -- check second argument is a field
    if not isField F then (
	error "symmetricGroupTable: expected second argument to be a field";
	);
    -- check characteristic
    if n! % (char F) == 0 then (
	error ("symmetricGroupTable: expected field characteristic not dividing " | toString(n) | "!");
	);
    -- list partitions
    P := apply(partitions n, toList);
    -- compute table using Murnaghan-Nakayama
    -- uses murnaghanNakayama unexported function with
    -- code in BettiCharacters.m2 immediately before this method
    X := matrix(F, table(P,P,murnaghanNakayama));
    -- compute size of conjugacy classes
    conjSize := apply(P/tally,
	t -> n! / product apply(pairs t, (k,v) -> k^v*v! )
	);
    -- matrix for inner product
    m := diagonalMatrix(F,conjSize)*transpose(X);
    -- prepare labels
    pows := apply(P/tally,t -> apply(rsort keys t, k -> Power(k,t#k)));
    netLabels := apply(pows,p -> "(" | horizontalJoin between(",",p/net) | ")");
    texLabels := apply(pows,p -> texMath toSequence p);
    new CharacterTable from {
	(symbol numActors) => #P,
	(symbol size) => conjSize,
	(symbol table) => X,
	(symbol ring) => F,
	(symbol matrix) => m,
	-- compact partition notation used for symmetric group labels
	(symbol Labels) => {netLabels,texLabels}
	}
    )

-- symmetric group table for backwards compatibility
symmetricGroupTable PolynomialRing := R -> (
    symmetricGroupTable(numgens R,coefficientRing R)
    )

-- symmetric group variable permutation action
symmetricGroupActors = method();
symmetricGroupActors PolynomialRing := R -> (
    -- check argument is a polynomial ring over a field
    if not isField coefficientRing R then (
	error "symmetricGroupActors: expected polynomial ring over a field";
	);
    -- check number of variables
    n := numgens R;
    if n < 1 then (
	error "symmetricGroupActors: expected a positive number of variables";
	);
    for p in partitions(n) list (
	L := gens R;
	g := for u in p list (
	    l := take(L,u);
	    L = drop(L,u);
	    rotate(1,l)
	    );
	matrix { flatten g }
    	)
    )

---------------------------------------------------------------------
-- Specialized functions for hyperoctahedral groups (v2.6) ----------
---------------------------------------------------------------------

-- auxiliary unexported function for the size of conjugacy classes of Hn
-- a conjugacy class of Hn is indexed by a bipartition (alpha,beta)
-- where alpha is the cycle type of balanced cycles and
-- beta is the cycle type of unbalanced cycles
hConjSize = (alpha, beta) -> (
    -- convert to lists
    p := toList alpha;
    q := toList beta;
    -- get total size
    n := (sum p)+(sum q);
    -- size of conjugacy class of alpha in symmetric group
    a := (sum p)! / product apply(pairs tally p, (k,v) -> k^v*v! );
    -- size of conjugacy class of beta in symmetric group
    b := (sum q)! / product apply(pairs tally q, (k,v) -> k^v*v! );
    -- get weight from first partition
    binomial(n,sum p) * a * b * 2^(n-#p-#q)
    )

-- auxiliary unexported function for the cycle type of a permutation
-- pass a permutation as a list which represents the 2nd row
-- of the 2-row notation, start from 0
cycleType = sigma -> (
    used := {};
    rsort while #used < #sigma list (
	u := min toList( set(sigma) - set(used) );
	cycle := {};
	while not isMember(u,cycle) do (
	    cycle = append(cycle,u);
	    u = sigma_u;
	    );
	used = used | cycle;
	#cycle
	)
    )

-- auxiliary unexported function for the value of an irreducible character of Hn
-- the irreducible character of Hn indexed by a bipartition (lambda,mu)
-- is evaluated at an element in the conjugacy class indexed by (alpha,beta)
hCharValue = (lambda,mu,alpha,beta) -> (
    -- get weight of first partition
    k := sum toList lambda;
    -- get weight of bipartition
    n := k + sum toList mu;
    -- list 0 to n-1, used a few times
    N := toList(0..n-1);
    -- form element of cycle type alpha,beta
    -- as a 01-vector s and a permutation sigma
    s := toList((sum toList alpha):0);
    s = s | flatten for u in beta list ( {1} | toList(u-1:0) );
    L := N;
    sigma := flatten for u in (toList alpha)|(toList beta) list (
	l := take(L,u);
	L = drop(L,u);
	rotate(1,l)
	);
    -- get cosets of the Young subgroup Sk x S(n-k)
    -- that contribute to the induced character
    G := select(subsets(n,k), gamma -> isSubset(sigma_gamma,gamma));
    -- for each conjugacy class, compute contribution to character
    sum for gamma in G list (
	-- restrict sigma to gamma
	p := sigma_gamma;
	-- and find its cycle type
	-- convert p to a permutation on 0 to #p-1
	H := new HashTable from pack(2,mingle {sort p,toList(0..#p-1)});
	-- get the cycle type of p
	cp := cycleType apply(p, i -> H#i);
	-- restrict sigma to the complement of gamma
	q := sigma_(N-set(gamma));
	-- and find its cycle type
	-- convert q to a permutation on 0 to #q-1
	H = new HashTable from pack(2,mingle {sort q,toList(0..#q-1)});
	-- get the cycle type of q
	cq := cycleType apply(q, i -> H#i);
	-- pad gamma to a permutation of 0..n-1
	gammaN := gamma | (N-set(gamma));
	-- weight vector for the Z_2^n-action
	vk := toList(k:0) | toList(n-k:1);
	-- dot product of s and vk
	e := sum apply(s_gammaN,vk, (i,j) -> i*j);
	(-1)^e * murnaghanNakayama(toList lambda,cp) * murnaghanNakayama(toList mu,cq)
	)
    )

-- symmetric group character table
hyperoctahedralGroupTable = method(TypicalValue=>CharacterTable);
hyperoctahedralGroupTable(ZZ,Ring) := (n,F) -> (
    -- check n is at least one
    if n < 1 then (
	error "hyperoctahedralGroupTable: expected first argument to be a positive integer";
	);
    -- check second argument is a field
    if not isField F then (
	error "hyperoctahedralGroupTable: expected second argument to be a field";
	);
    -- check characteristic
    if n! % (char F) == 0 then (
	error ("hyperoctahedralGroupTable: expected field characteristic not dividing " | toString(n) | "!*2^" | toString(n));
	);
    -- list bipartitions of n
    B := flatten for i to n list (
	flatten table(partitions (n-i),partitions i, identity)
	);
    -- make matrix of character table
    X := matrix(F, table(B,B, (a,b) -> hCharValue(a_0,a_1,b_0,b_1)));
    -- compute size of conjugacy classes
    conjSize := apply(B, b -> hConjSize(b_0,b_1));
    -- matrix for inner product
    m := diagonalMatrix(F,conjSize)*transpose(X);
    -- prepare labels
    bitallies := apply(B, b -> (tally toList b_0,tally toList b_1));
    -- turn tallies into powers, make empty tally into a single zero
    pows := apply(bitallies, (a,b) ->
	(if a === tally{} then ({Power(0,1)}) else (apply(rsort keys a, k -> Power(k,a#k))),
	    if b === tally{} then ({Power(0,1)}) else (apply(rsort keys b, k -> Power(k,b#k))))
	);
    netLabels := apply(pows, (a,b) ->
	"(" | horizontalJoin between(",",a/net) | ";" | horizontalJoin between(",",b/net) | ")"
	);
    texLabels := apply(pows,p -> (
	    a := texMath toSequence p_0;
	    b := texMath toSequence p_1;
	    -- remove additional closing and opening parentheses
	    substring(0,#a-7,a) | ";" | substring(6,b)
	    )
	);
    new CharacterTable from {
	(symbol numActors) => #B,
	(symbol size) => conjSize,
	(symbol table) => X,
	(symbol ring) => F,
	(symbol matrix) => m,
	-- compact partition notation used for hyperoctahedral group labels
	(symbol Labels) => {netLabels,texLabels}
	}
    )

-- hyperoctahedral group variable permutation action
hyperoctahedralGroupActors = method();
hyperoctahedralGroupActors PolynomialRing := R -> (
    -- check argument is a polynomial ring over a field
    if not isField coefficientRing R then (
	error "hyperoctahedralGroupActors: expected polynomial ring over a field";
	);
    -- check number of variables
    n := numgens R;
    if n < 1 then (
	error "hyperoctahedralGroupActors: expected a positive number of variables";
	);
    flatten for i to n list (
	flatten for p in partitions(n-i) list (
	    for q in partitions(i) list (
		L := gens R;
		alpha := flatten for u in (toList p) list (
		    l := take(L,u);
		    L = drop(L,u);
		    rotate(1,l)
		    );
		beta := flatten for u in (toList q) list (
		    l := take(L,u);
		    L = drop(L,u);
		    take(l,-u+1) | {minus first l}
		    );
		matrix { alpha | beta }
		)
	    )
	)
    )

---------------------------------------------------------------------
-- Pretty printing of new types -------------------------------------
---------------------------------------------------------------------

-- printing for characters
-- the next function preps a character for printing by caching
-- a bigraded hash table of its data as before v2.5
prepCharacter := c -> (
    DR := c.degreesRing;
    F := coefficientRing DR;
    -- go through homological degrees
    -- collect multidegrees in the same orbit of the group action
    -- and save a single character with the degree representative
    h := new MutableHashTable;
    for k in keys c.characters do (
	raw := c.characters#k;
	mons := flatten entries monomials raw;
	while mons =!= {} do (
	    m := first mons;
	    d := c.degreeRepresentative first exponents m;
	    orbit := select(mons, f -> c.degreeRepresentative first exponents f == d);
	    C := lift(last coefficients(raw, Monomials=>orbit),F);
	    h#(k,d) = matrix{toList (numRows C:1_F)} * C;
	    mons = mons - set(orbit);
	    );
	);
    c.cache.print = new HashTable from h;
    )

-- create net for pretty printing of character
net Character := c -> (
    if not c.cache.?print then prepCharacter c;
    bottom := apply(sort pairs c.cache.print,
	(k,v) -> {net k} | apply(flatten entries v,net));
    F := coefficientRing c.degreesRing;
    stack("Character over "|(net F)," ",
	netList(bottom,BaseRow=>0,Alignment=>Right,Boxes=>{false,{1}},HorizontalSpace=>2))
    )

-- create tex string for characters
texMath Character := c -> (
    if not c.cache.?print then prepCharacter c;
    -- make table headers, one column per actor
    s := concatenate("\\begin{array}{c|",c.numActors:"r","}\n");
    -- character entries
    rows := apply(sort pairs c.cache.print,
	(k,v) -> concatenate(texMath k,"&",
	    between("&",apply(flatten entries v,texMath))
	    )
	);
    -- assemble and close array
    s | concatenate(between("\\\\ \n",rows),"\n\\end{array}")
    )

-- printing for character tables
net CharacterTable := T -> (
    -- top row of character table
    a := {{""} | T.size};
    -- body of character table
    b := apply(pack(1,first T.Labels),entries T.table,(i,j)->i|j);
    stack("Character table over "|(net T.ring)," ",
	netList(a|b,BaseRow=>1,Alignment=>Right,Boxes=>{{1},{1}},HorizontalSpace=>2)
	)
    )

-- tex string for character tables
texMath CharacterTable := T -> (
    -- make table headers, one column per actor
    s := concatenate("\\begin{array}{c|",T.numActors:"r","}\n");
    -- print size of "conjugacy" classes
    s = s | concatenate("&",between("&",apply(T.size,texMath)),"\\\\ \\hline\n");
    -- get matrix of table entries and convert to strings
    M := for row in entries(T.table) list (
	concatenate(between("&",apply(row,texMath)))
	);
    -- put character label in front of its row
    M = apply(last T.Labels,M,(l,r)->l|"&"|r);
    -- close the array
    s | concatenate(between("\\\\ \n",M),"\n\\end{array}")
    )

-- printing character decompositions
-- the next function preps a character for printing by caching
-- a bigraded hash table of its data as before v2.5
prepDecomposition := D -> (
    DR := D.degreesRing;
    F := coefficientRing DR;
    -- go through homological degrees
    -- collect multidegrees in the same orbit of the group action
    -- and save a single character with the degree representative
    h := new MutableHashTable;
    for k in keys D.decompose do (
	raw := D.decompose#k;
	mons := flatten entries monomials raw;
	while mons =!= {} do (
	    m := first mons;
	    d := D.degreeRepresentative first exponents m;
	    orbit := select(mons, f -> D.degreeRepresentative first exponents f == d);
	    C := lift(last coefficients(raw, Monomials=>orbit),F);
	    h#(k,d) = matrix{toList (numRows C:1_F)} * C;
	    mons = mons - set(orbit);
	    );
	);
    D.cache.print = new HashTable from h;
    )

-- create net for pretty printing of character decomposition
net CharacterDecomposition := D -> (
    if not D.cache.?print then prepDecomposition D;
    -- find non zero columns of table for printing
    M := matrix apply(values D.decompose, m -> flatten entries m);
    p := positions(toList(0..numColumns M - 1), i -> M_i != 0*M_0);
    -- top row of decomposition table
    a := {{""} | (first D.Labels)_p };
    -- body of decomposition table
    b := apply(sort pairs D.cache.print,
	(k,v) -> {k} | (flatten entries v)_p );
    stack("Decomposition table"," ",
	netList(a|b,BaseRow=>1,Alignment=>Right,Boxes=>{{1},{1}},HorizontalSpace=>2)
	)
    )

-- tex string for character decompositions
texMath CharacterDecomposition := D -> (
    if not D.cache.?print then prepDecomposition D;
    -- find non zero columns of table for printing
    M := matrix apply(values D.decompose, m -> flatten entries m);
    p := positions(toList(0..numColumns M - 1), i -> M_i != 0*M_0);
    -- make table headers, one column per nonzero irrrep
    s := concatenate("\\begin{array}{c|",#p:"r","}\n");
    -- top row with labels of characters appearing in decomposition
    s = s | concatenate("&",between("&",(last D.Labels)_p),"\\\\ \\hline\n");
    -- decomposition table entries
    rows := apply(sort pairs D.cache.print,
	(k,v) -> concatenate(texMath k,"&",
	    between("&",apply((flatten entries v)_p,texMath))
	    )
	);
    -- assemble and close array
    s | concatenate(between("\\\\ \n",rows),"\n\\end{array}")
    )

-- printing for Action type
net Action := A -> (
    (net class A.target)|" with "|(net (A.numActors))|" actors"
    )


----------------------------------------------------------------------
-- Documentation
----------------------------------------------------------------------

beginDocumentation()

doc ///

Node
    Key
    	BettiCharacters
    Headline
    	finite group characters on free resolutions and graded modules
    Description
    	Text
	    This package contains functions for computing characters
	    of free resolutions and graded modules equipped with
	    the action of a finite group.
	    
	    Let $R$ be a positively graded polynomial ring over a
	    field $\Bbbk$, and $M$ a finitely generated graded
	    $R$-module. Suppose $G$ is a finite group whose order
	    is not divisible by the characteristic of $\Bbbk$.
	    Assume $G$ acts $\Bbbk$-linearly on $R$ and $M$
	    by preserving degrees, and distributing over
	    $R$-multiplication.
	    If $F_\bullet$ is a minimal free resolution of $M$, and
	    $\mathfrak{m}$ denotes the maximal ideal generated by the variables
	    of $R$, then each $F_i / \mathfrak{m}F_i$ is a graded
	    $G$-representation. We call the
	    characters of the representations $F_i / \mathfrak{m}F_i$
	    the {\bf Betti characters} of $M$, since
	    evaluating them at the identity element of $G$ returns
	    the usual Betti numbers of $M$.
	    Moreover, the graded
	    components of $M$ are also $G$-representations.
	    
	    This package provides functions to
	    compute the Betti characters and the characters of
	    graded components of $M$
	    based on the algorithms in @HREF("https://doi.org/10.1016/j.jsc.2022.02.001","F. Galetto - Finite group characters on free resolutions")@.
	    The package is designed to
	    be independent of the group; the user provides matrices for
	    the group actions and character tables (to decompose
		characters into irreducibles).
	    See the menu below for using this package
	    to compute some examples from the literature.
	    
	    @HEADER4 "Version history:"@
	    
	    @UL {(BOLD "1.0: ", "Initial version. Includes computation of
		actions and Betti characters.") ,
		(BOLD "2.0: ", "Introduces character tables, decompositions,
		and other methods for characters."),
		(BOLD "2.1: ", "Adds equality checks for actions and
		    characters. Contains several small improvements to the
		    code and documentation, including a new multigraded
		    example."),
		(BOLD "2.2: ", "Characters and character tables are now
		    defined over fields (instead of polynomial rings).
		    This version also introduces new character operations
		    and $\\TeX$ printing for characters and character tables."),
		(BOLD "2.3: ", "New option for the action of a semidirect
		    product of a finite group acting on a torus.
		    Improved caching and removed calls to deprecated functions."),
		(BOLD "2.4: ", "Introduces significant optizimations to the core
		    algorithm for computing Betti characters. Removed the ",
		    TT "inverseRingActors", " method since it is not used anymore."),
		(BOLD "2.5: ", "Overhauls the internal representation of characters
		    to better handle actions of semidirect products; v2.5 characters
		    are incompatible with those from previous versions. Adds tensor
		    powers of characters. Removes some less used methods to access
		    keys of actions and characters. Requires Macaulay2 1.24.05."),
		(BOLD "2.6: ", "Adds methods for hyperoctahedral group.")
		}@
    Subnodes
    	:Defining and computing actions
      	action
	actors
      	:Characters and related operations
	"Character class"
        character
	"Character operations"
	:Character tables and decompositions
	characterTable
	decomposeCharacter
	:Additional methods
	"Equality checks"
	symmetricGroupActors
	symmetricGroupTable
	hyperoctahedralGroupActors
	hyperoctahedralGroupTable
    	:Examples
      	"BettiCharacters Example 1"
      	"BettiCharacters Example 2"
      	"BettiCharacters Example 3"
      	"BettiCharacters Example 4"
	"BettiCharacters Example 5"

Node
    Key
	"Character class"
    Headline
	internal representation of graded characters
    Description
	Text
	   Version 2.5 of the @TO "BettiCharacters"@ package introduces
	   a new internal representation of graded characters.
	   Although character printouts look the same as in
	   previous versions, this new
	   representation is incompatible with the one from earlier
	   versions of the package. The earlier representation was
	   not sufficient to compute tensor products of characters of
	   the semidirect product of a finite group with a torus
	   (see @TO "Semidirect"@). Not only the new representation
	   makes this possible, it also generally makes character
	   operations more natural and sets the groundwork for
	   functionality to be added in future versions.

	   To understand how characters are stored, consider the
	   following example.
	Example
	    R = QQ[x,y,z]
	    I = ideal(x+y+z,x*y+x*z+y*z,x*y*z)
	    Q = R/I
	    S2 = symmetricGroupActors R
	    A = action(Q,S2)
	    a = character(A,0,10)
	Text
	    The quotient ring @TT "Q"@ is a module concentrated in
	    homological degree zero. It is an Artinian ring with
	    nonzero components in degrees zero, one, two, and three.
	    Hence, the printout of the character of @TT "Q"@
	    shows four rows indexed by pairs @TT "(h,d)"@, with
	    @TT "h"@ the homological degree and @TT "d"@ the
	    internal degree.

	    Internally, the character stores a single matrix for
	    each homological degree. To separate the internal
	    degrees, the character uses elements from the
	    @TO "degreesMonoid"@ of the ambient ring of @TT "Q"@.
	    Factoring out the monomials of the monoid of degrees,
	    gives the characters of the graded components.
	Example
	    a.characters
	    coefficients a.characters#0
	Text
	    With this setup, the character in each homological
	    degree is a matrix with values in a "character ring".
	    This character ring is constructed as @TT "F[M]"@,
	    where @TT "F"@ is the field of coefficients of @TT "R"@
	    and @TT "M"@ is the @TO "degreesMonoid"@ of @TT "R"@.
	    The character ring is stored with each action and
	    character, and in the cache table of the polynomial
	    ring under the key @TT "degreesRing"@.
	Example
	    A.degreesRing
	    a.degreesRing
	    R.cache#degreesRing
	Text
	    When different characters are combined with methods
	    such as @TO "BettiCharacters::directSum(Character)"@ or
	    @TO "BettiCharacters::tensor(Character,Character)"@, the package
	    checks that all characters have values in the same
	    character ring. All actions and characters of
	    objects (rings, ideals, modules, resolutions) over
	    the same ambient ring automatically share the
	    same character ring.

	    The character ring can be defined just as well
	    when the ambient ring is multigraded and in the
	    presence of actions by the semidirect product of
	    a finite group and the torus responsible for the
	    multigrading.
	Example
	    S = QQ[u,v,w,Degrees=>{{1,0,0},{0,1,0},{0,0,1}}]
	    J = ideal(u^2,v^2,w^2,u*v*w)
	    RJ = res J
	    S3 = symmetricGroupActors(S)
	    B = action(RJ,S3,Semidirect=>{uniquePermutations,rsort})
	    b = character B
	    b.characters
	    b.degreesRing
    Caveat
	To avoid issues such as incompatible character rings
	or character rings being recreated over and over,
	the handling of character rings is currently not
	exposed to the user.
	
Node
    Key
    	"Character operations"
    Headline
    	including shift, direct sum, dual, and tensor product
    Description
    	Text
	    The @TO BettiCharacters@ package contains
	    several functions for working with characters.
	    See links below for more details.
    SeeAlso
	(symbol SPACE,Character,Array)
	(symbol _,Character,ZZ)
	(symbol ^,Character,List)
    	(symbol *,RingElement,Character)
	(minus,Character)
	(directSum,Character)
	(difference,Character,Character)
	(dual,Character,RingMap)
	(tensor,Character,Character)
    	

Node
   Key
       "BettiCharacters Example 1"
   Headline
       Specht ideals / subspace arrangements
   Description
    Text
    	In this example, we identify the Betti characters of the
	Specht ideal associated	with the partition (5,2).
	The action of the symmetric group on the resolution of
	this ideal is described in	
	@HREF("https://doi.org/10.1142/S0219498823501992",
	    "K. Shibata, K. Yanagawa - Elementary construction of minimal free resolutions of the Specht ideals of shapes (n−2,2) and (d,d,1)")@.
	The same ideal is also the ideal of the 6-equals
	subspace arrangement in a 7-dimensional affine space.
	This point of view is explored in
	@HREF("https://doi.org/10.1007/s00220-014-2010-4",
	    "C. Berkesch, S. Griffeth, S. Sam - Jack polynomials as fractional quantum Hall states and the Betti numbers of the (k+1)-equals ideal")@
	where the action of the symmetric group on the resolution
	is also described.
	
	We begin by constructing the ideal explicitly.
	As an alternative, the ideal can be obtained using the
	function @TT "spechtPolynomials"@
	provided by the package @TT "SpechtModule"@.
	We compute a minimal free resolution and its Betti table.
    Example
    	R=QQ[x_1..x_7]
	I1=ideal apply({4,5,6,7}, i -> (x_1-x_2)*(x_3-x_i));
	I2=ideal apply(subsets({3,4,5,6,7},2), s -> (x_1-x_(s#0))*(x_2-x_(s#1)));
	I=I1+I2
	RI=res I
	betti RI
    Text
    	Next we set up the group action on the resolution.
	The group is the symmetric group on 7 elements.
	Its conjugacy classes are determined by cycle types,
	which are in bijection with partitions of 7.
	Representatives for the conjugacy classes of the symmetric
	group acting on a polynomial ring by permuting the
	variables can be obtained via @TO symmetricGroupActors@.
	Once the action is set up, we compute the Betti characters.
    Example
    	S7 = symmetricGroupActors R
	A = action(RI,S7)
	elapsedTime c = character A
    Text
    	To make sense of these characters we decompose them
	against	the character table of the symmetric group,
	which can be computed using the function
	@TO "symmetricGroupTable"@. The irreducible characters
	are indexed by the partitions of 7, which are written
	using a compact notation (the exponents indicate how
	    many times a part is repeated).
    Example
    	T = symmetricGroupTable(7,QQ)
	decomposeCharacter(c,T)
    Text
    	As expected from the general theory, we find a single
	irreducible representation in each homological degree.
	
	Finally, we can observe the Gorenstein duality of the
	resolution and its character. We construct the character
	of the sign representation concentrated in homological
	degree 0, internal degree 7. Then we dualize the character
	of the resolution previously computed, shift its homological
	degree by the length of the resolution, and twist it by
	the sign character just constructed: the result is the
	same as the character of the resolution.
    Example
    	signrep = character(R,hashTable {(0,{7}) =>
		matrix{{1,-1,-1,1,-1,1,-1,1,1,-1,1,-1,1,-1,1}}})
	dual(c,id_QQ)[-5] ** signrep == c
    Text
    	The second argument in the @TT "dual"@ command is the
	restriction of complex conjugation to the field of
	definition of the characters.
	For more information, see @TO (dual,Character,RingMap)@.

Node
   Key
       "BettiCharacters Example 2"
   Headline
       Symbolic powers of star configurations
   Description
    Text
    	In this example, we identify the Betti characters of the
	third symbolic power of a monomial star configuration.
	The action of the symmetric group on the resolution of
	this ideal is described in Example 6.5 of
	@HREF("https://doi.org/10.1016/j.jalgebra.2020.04.037",
	    "J. Biermann, H. De Alba, F. Galetto, S. Murai, U. Nagel, A. O'Keefe, T. Römer, A. Seceleanu - Betti numbers of symmetric shifted ideals")@,
	and belongs to the larger class of symmetric shifted
	ideals.
	
	First, we construct the ideal
	and compute its minimal free resolution and Betti table.
    Example
	R=QQ[x_1..x_6]
	I=intersect(apply(subsets(gens R,4),x->(ideal x)^3))
	RI=res I
	betti RI
    Text
    	Next, we set up the group action on the resolution.
	The group is the symmetric group on 6 elements.
	Its conjugacy classes are determined by cycle types,
	which are in bijection with partitions of 6.
	Representatives for the conjugacy classes of the symmetric
	group acting on a polynomial ring by permuting the
	variables can be obtained via @TO symmetricGroupActors@.
	After setting up the action, we compute the Betti characters.
    Example
    	S6 = symmetricGroupActors R
	A=action(RI,S6)
	elapsedTime c=character A
    Text
    	Next, we decompose the characters
	against	the character table of the symmetric group,
	which can be computed using the function
	@TO "symmetricGroupTable"@. The irreducible characters
	are indexed by the partitions of 6, which are written
	using a compact notation (the exponents indicate how
	    many times a part is repeated).
    Example
    	T = symmetricGroupTable(6,QQ)
	decomposeCharacter(c,T)
    Text
    	The description provided in
	@HREF("https://doi.org/10.1016/j.jalgebra.2020.04.037",
	    "J. Biermann, H. De Alba, F. Galetto, S. Murai, U. Nagel, A. O'Keefe, T. Römer, A. Seceleanu - Betti numbers of symmetric shifted ideals")@
	uses representations induced from products of smaller
	symmetric groups. To compare that description with the results
	obtained here, one may use the Littlewood-Richardson rule
	to decompose induced representations into a direct sum
	of irreducibles.


Node
   Key
       "BettiCharacters Example 3"
   Headline
       Klein configuration of points
   Description
    Text
    	In this example, we identify the Betti characters of the
	defining ideal of the Klein configuration of points in the
	projective plane and its square.
	The defining ideal of the Klein configuration is
	explicitly constructed in Proposition 7.3 of
	@HREF("https://doi.org/10.1093/imrn/rnx329",
	    "T. Bauer, S. Di Rocco, B. Harbourne, J. Huizenga, A. Seceleanu, T. Szemberg - Negative Curves on Symmetric Blowups of the Projective Plane, Resurgences, and Waldschmidt Constants")@.
	We start by constructing the ideal, its square, and both
	their resolutions and Betti tables. In order to later use
	characters, we work over the cyclotomic field obtained by
	adjoining a primitive 7th root of unity to $\mathbb{Q}$.
    Example
    	needsPackage "Cyclotomic"
	kk = toField(QQ[a] / cyclotomicPoly(7, a))
	R = kk[x,y,z]
	f4 = x^3*y+y^3*z+z^3*x
	H = jacobian transpose jacobian f4
	f6 = -1/54*det(H)
	I = minors(2,jacobian matrix{{f4,f6}})
	RI = res I
	betti RI
	I2 = I^2;
	RI2 = res I2
	betti RI2
    Text
	The unique simple group of order 168 acts as described
	in §2.2 of @HREF("https://doi.org/10.1093/imrn/rnx329",
	"BDHHSS")@. In particular, the group is generated by the
	elements @TT "g"@ of order 7, @TT "h"@ of order 3, and
	@TT "i"@ of order 2, and is minimally defined over the
	7th cyclotomic field. In addition, we consider the identity,
	the inverse of @TT "g"@,
	and another element @TT "j"@ of order 4 as representatives
	of the conjugacy classes of the group.
	The action of the group on the resolution of
	both ideals is described in the second proof of
	Proposition 8.1.
    Example
	g = matrix{{a^4,0,0},{0,a^2,0},{0,0,a}}
	h = matrix{{0,1,0},{0,0,1},{1,0,0}}
	i = (2*a^4+2*a^2+2*a+1)/7 * matrix{
    	    {a-a^6,a^2-a^5,a^4-a^3},
    	    {a^2-a^5,a^4-a^3,a-a^6},
    	    {a^4-a^3,a-a^6,a^2-a^5}
    	    }
	j = -1/(2*a^4+2*a^2+2*a+1) * matrix{
    	    {a^5-a^4,1-a^5,1-a^3},
    	    {1-a^5,a^6-a^2,1-a^6},
    	    {1-a^3,1-a^6,a^3-a}
    	    }
	G = {id_(R^3),i,h,j,g,inverse g};
    Text
    	We compute the action of this group
	on the two resolutions above.
	Notice how the group action is passed as a list of square
	matrices (instead of one-row substitution matrices as in
	    @TO "BettiCharacters Example 1"@ and
	    @TO "BettiCharacters Example 2"@); to enable this,
	we set the option @TO Sub@ to @TT "false"@.
    Example
	A1 = action(RI,G,Sub=>false)
	A2 = action(RI2,G,Sub=>false)
	elapsedTime a1 = character A1
	elapsedTime a2 = character A2
    Text
    	Next we set up the character table of the group
	and decompose the Betti characters of the resolutions.
	The arguments are: a list with the cardinality of the
	conjugacy classes, a matrix with the values of the irreducible
	characters, the base field, and the complex
	conjugation map restricted to the field of coefficients.
	See @TO characterTable@ for more details.
    Example
        s = {1,21,56,42,24,24}
	m = matrix{{1,1,1,1,1,1},
    	    {3,-1,0,1,a^4+a^2+a,-a^4-a^2-a-1},
    	    {3,-1,0,1,-a^4-a^2-a-1,a^4+a^2+a},
    	    {6,2,0,0,-1,-1},
    	    {7,-1,1,-1,0,0},
    	    {8,0,-1,0,1,1}};
	conj = map(kk,kk,{a^6})
        T = characterTable(s,m,kk,conj)
	a1/T
	a2/T
    Text
    	Since $\chi_0$ is the trivial character,
	this computation shows that the
	free module in homological degree two in the resolution of the
	defining ideal of the Klein configuration is a direct sum
	of two trivial representations, one in degree 11 and one in
	degree 13. It follows that its second
	exterior power is a trivial representation concentrated in
	degree 24. As observed in the second
	proof of Proposition 8.1 in @HREF("https://doi.org/10.1093/imrn/rnx329",
	"BDHHSS")@, the free module in homological degree 3 in the
    	resolution of the square of the ideal is exactly this
	second exterior power (and a trivial representation).
	
	Alternatively, we can compute the symbolic square of the
	ideal modulo the ordinary square. The component of degree
	21 of this quotient matches the generators of the last
	module in the resolution of the ordinary square in degree
	24 (by local duality); in
	particular, it is a trivial representation. We can verify
	this directly.
    Example
	needsPackage "SymbolicPowers"
	Is2 = symbolicPower(I,2);
	M = Is2 / I2;
	B = action(M,G,Sub=>false)
	elapsedTime b = character(B,21)
	b/T

Node
    Key
    	"BettiCharacters Example 4"
    Headline
    	a multigraded example
    Description
    	Text
	    Consider the polynomial ring $\mathbb{Q} [x_1,x_2,y_1,y_2,y_3]$
       	    with the variables $x_i$ of bidegree @TT "{1,0}"@ and the
       	    variables $y_j$ of bidegree @TT "{0,1}"@.
	    We consider the action of a product of two symmetric
	    groups, the first permuting the $x_i$ variables and the
	    second permuting the $y_j$ variables. We compute the
	    Betti characters of this group on the resolution of
	    the bigraded irrelevant ideal
	    $\langle x_1,x_2\rangle \cap \langle y_1,y_2,y_3\rangle$.
	    This is also the edge ideal of the complete bipartite graph
	    $K_{2,3}$.
    	Example
    	    R = QQ[x_1,x_2,y_1,y_2,y_3,Degrees=>{2:{1,0},3:{0,1}}]
	    I = intersect(ideal(x_1,x_2),ideal(y_1,y_2,y_3))
	    RI = res I
    	    G = {
	    	matrix{{x_1,x_2,y_2,y_3,y_1}},
	    	matrix{{x_1,x_2,y_2,y_1,y_3}},
	    	matrix{{x_1,x_2,y_1,y_2,y_3}},
	    	matrix{{x_2,x_1,y_2,y_3,y_1}},
	    	matrix{{x_2,x_1,y_2,y_1,y_3}},
	    	matrix{{x_2,x_1,y_1,y_2,y_3}}
	    	}
    	    A = action(RI,G)
    	    character A
    	Text
    	    We can also compute the characters of some graded
	    components of the quotient by this ideal.
    	Example
    	    Q = R/I
       	    B = action(Q,G)
       	    character(B,{1,0})
       	    character(B,{0,1})
       	    character(B,{4,0})
       	    character(B,{0,5})
    	Text
	    Note that all mixed degree components are zero.
    	Example
       	    character(B,{1,1})
       	    character(B,{2,1})
       	    character(B,{1,2})


Node
    Key
    	"BettiCharacters Example 5"
    Headline
    	semidirect product of torus and symmetric group
    Description
    	Text
	    We present the example in the introduction of
	    @HREF("https://doi.org/10.1112/jlms.12551",
	    "S. Murai, C. Raicu - An equivariant Hochster’s formula for $\\mathfrak{S}_n$-invariant monomial ideals")@.

	    Consider the ideal $I$ in three variables generated by
	    monomials whose exponent vectors are permutations of
	    $(4,1,1)$ or $(5,2,0)$. This ideal is clearly stable
	    under the permutation action of $\mathfrak{S}_3$.
	    Moreover, $I$ is compatible with the fine grading
	    on $R = \Bbbk [x_1,x_2,x_3]$ given by $\deg (x_i) = e_i
	    \in \mathbb{Z}^3$. We compute a minimal free resolution of
	    $R/I$ and show its Betti diagram.
    	Example
    	    R = QQ[x_1..x_3,Degrees=>{{1,0,0},{0,1,0},{0,0,1}}]
	    I = ideal(x_1^4*x_2*x_3,x_1*x_2^4*x_3,x_1*x_2*x_3^4,
		x_1^5*x_2^2,x_1^5*x_3^2,x_1^2*x_2^5,x_1^2*x_3^5,x_2^5*x_3^2,x_2^2*x_3^5)
	    RI = res I
	    betti RI
	Text
	    Next, we set up the action of the semidirect product
	    $(\Bbbk^\times)^3 \rtimes \mathfrak{S}_3$ where
	    $\mathfrak{S}_3$ acts on $(\Bbbk^\times)^3$ by
	    permuting entries. This results in $\mathfrak{S}_3$
	    acting on the grading group $\mathbb{Z}^3$ (the character
		group of $(\Bbbk^\times)^3$) by permuting the
	    entries of the degree vectors. Thus, the orbit of a
	    degree $d\in \mathbb{Z}^3$ consists of all permutations
	    of $d$; we fix the nonincreasing permutation of $d$ as
	    the distinguished representative of this orbit.
	    See @TO "Semidirect"@ for details.
	Example
	    S3 = symmetricGroupActors(R)
    	    A = action(RI,S3,Semidirect=>{uniquePermutations,rsort})
    	    c = character A
    	Text
    	    To match the description of the paper, which resolves the
	    ideal $I$ instead of the quotient $R/I$, we remove
	    the component in homological degree 0, then shift the
	    complex to the left. Finally, the resulting character is
	    decomposed against the character table of $\mathfrak{S}_3$.
    	Example
	    c = (c - c_0)[1]
    	    T = symmetricGroupTable(3,QQ)
	    decomposeCharacter(c,T)
	Text
	    The irreducible representations found above match
	    our expectations as can be verified by
	    applying Pieri's rule to the description
	    in Example 1.4 of Murai and Raicu's paper.

Node
    Key
    	Action
    Headline
    	the class of all finite group actions
    Description
    	Text
	    This class is provided by the package
	    @TO BettiCharacters@.
    Subnodes
    	ActionOnComplex
	ActionOnGradedModule
	ringActors
	(net,Action)
	    
Node
    Key
    	ActionOnComplex
    Headline
    	the class of all finite group actions on complexes
    Description
    	Text
	    This class is provided by the package
	    @TO BettiCharacters@.
	    
Node
    Key
    	ActionOnGradedModule
    Headline
    	the class of all finite group actions on graded modules
    Description
    	Text
	    This class is provided by the package
	    @TO BettiCharacters@.

	    
Node
    Key
    	Character
    Headline
    	the class of all characters of finite group representations
    Description
    	Text
	    This class is provided by the package
	    @TO BettiCharacters@.
    Subnodes
    	(symbol SPACE,Character,Array)
    	(symbol *,RingElement,Character)
    	(symbol ^,Character,List)
    	(symbol _,Character,ZZ)
    	(minus,Character)
	(directSum,Character)
    	(difference,Character,Character)
	(dual,Character,RingMap)
	(net,Character)
	(tensor,Character,Character)
	(texMath,Character)

Node
    Key
    	(symbol SPACE,Character,Array)
    Headline
    	homological shift
    Description
    	Text
	    Shift the homological degrees of a character.
    	Example
	    R = QQ[x,y,z]
	    I = ideal(x*y,x*z,y*z)
	    RI = res I
	    S3 = symmetricGroupActors R
	    A = action(RI,S3)
	    a = character A
	    a[-10]
        	    
Node
    Key
    	(symbol _,Character,ZZ)
    	(symbol _,Character,List)
    Headline
    	extract component
    Description
    	Text
	    Extract the component(s) of a character in
	    the given homological dimension(s).
    	Example
	    R = QQ[x,y,z]
	    I = ideal vars R
	    RI = res I
	    S3 = symmetricGroupActors R
	    A = action(RI,S3)
	    c = character A
	    c_3
	    c_{1,3}
        	    
Node
    Key
    	(symbol ^,Character,List)
    Headline
    	extract graded component
    Description
    	Text
	    Extract the component(s) of a character in
	    the given (multi)degree(s).
    	Example
	    R = QQ[x,y,z]
	    I = (ideal vars R)^3
	    Q = R/I
	    S3 = symmetricGroupActors R
	    A = action(Q,S3)
	    c = character(A,0,10)
	    c^{1}
	    c^{{1},{2}}
	    c^{3}
        	    
Node
    Key
    	(symbol *,RingElement,Character)
    	(symbol *,ZZ,Character)
    	(symbol *,QQ,Character)
    	(symbol *,Character,RingElement)
    	(symbol *,Character,ZZ)
    	(symbol *,Character,QQ)
    Headline
    	scalar multiple of a character
    Description
    	Text
	    Multiply a character with an element in its
	    field of definition.
    	Example
	    R = QQ[x,y,z]
	    I = (ideal vars R)^3
	    Q = R/I
	    S3 = symmetricGroupActors R
	    A = action(Q,S3)
	    c = character(A,0,10)
	    2*c
	    c*(1/3)
	Text
	    As of version 2.5, it is possible to multiply
	    characters by elements of their degrees ring,
	    which will result in an internal degree shift.
	Example
	    DR = c.degreesRing
	    T = DR_0
	    c * T^10

Node
    Key
    	(minus,Character)
    	(symbol -,Character)
    Headline
    	additive inverse of a character
    Description
    	Text
	    Additive inverse of a character.
    	Example
	    R = QQ[x,y,z]
	    I = (ideal vars R)^3
	    S3 = symmetricGroupActors R
	    A = action(I,S3)
	    c = character(A,0,10)
	    -c
        	    
Node
    Key
    	(difference,Character,Character)
    	(symbol -,Character,Character)
    Headline
    	difference of characters
    Description
    	Text
	    Difference of two characters.
    	Example
	    R = QQ[x,y,z]
	    I = (ideal vars R)^3
	    J = ideal(x^3,y^3,z^3)
	    S3 = symmetricGroupActors R
	    A1 = action(I,S3)
	    A2 = action(J,S3)
	    c1 = character(A1,0,10)
	    c2 = character(A2,0,10)
	    c1 - c2
        	    
Node
    Key
    	CharacterTable
    Headline
    	the class of all character tables of finite groups
    Description
    	Text
	    This class is provided by the package
	    @TO BettiCharacters@.
    Subnodes
	(net,CharacterTable)
    	(texMath,CharacterTable)
    	    
Node
    Key
    	CharacterDecomposition
    Headline
    	the class of all finite group character decompositions
    Description
    	Text
	    This class is provided by the package
	    @TO BettiCharacters@.
    Subnodes
	(net,CharacterDecomposition)
    	(texMath,CharacterDecomposition)

Node
    Key
    	action
    Headline
    	define finite group action
    Description
    	Text
	    Use this function to set up a finite group action on
	    a minimal free resolution or graded module.
	    See the specific use cases for more details.
    Subnodes
    	Action
	(action,Complex,List,List,ZZ)
	(action,Module,List,List)
	Semidirect
	Sub
	    
Node
    Key
    	(action,Complex,List,List,ZZ)
    	(action,Complex,List)
    Headline
    	define finite group action on a resolution
    Usage
    	A=action(C,G)
	A=action(C,G,G',i)
    Inputs
    	C:Complex
	    a minimal free resolution over a polynomial ring @TT "R"@
	G:List
	    of group elements acting on the variables of @TT "R"@
	G':List
	    of group elements acting on a basis of @TT "C_i"@
	i:ZZ
	    a homological degree
    Outputs
    	A:ActionOnComplex
    Description
    	Text
	    Use this function to define the action of a finite group
	    on the minimal free resolution of a module over a
	    polynomial ring with coefficients in a field.
	    After setting up the action, use the function
	    @TO character@ to compute the Betti characters.
	    
	    The input @TT "G"@ is a @TO List@ of group elements
	    acting on the vector space spanned by the variables
	    of the ring @TT "R"@. By default, these elements are
	    passed as one-row substitution matrices as those
	    accepted by @TO substitute@. One may pass these elements
	    as square matrices by setting the optional input @TO Sub@
	    to @TT "false"@. The list @TT "G"@ can contain
	    arbitrary group elements however, to
	    obtain a complete representation theoretic description
	    of the characters, @TT "G"@ should be a list of
	    representatives of the conjugacy classes of the group.
	    
	    The example below sets up the action of a symmetric
	    group on the resolution of a monomial ideal.
	    The symmetric group acts by permuting the four
	    variables of the ring. The conjugacy classes of
	    permutations are determined by their cycle types,
	    which are in bijection with partitions. In this case,
	    we consider five permutations with cycle types,
	    in order: 4, 31, 22, 211, 1111.
    	Example
	    R = QQ[x_1..x_4]
	    I = ideal apply(subsets(gens R,2),product)
	    RI = res I
	    G = {matrix{{x_2,x_3,x_4,x_1}},
    		 matrix{{x_2,x_3,x_1,x_4}},
    		 matrix{{x_2,x_1,x_4,x_3}},
    		 matrix{{x_2,x_1,x_3,x_4}},
    		 matrix{{x_1,x_2,x_3,x_4}} }
	    A = action(RI,G)
	Text
	    The group elements acting on the ring can be recovered
	    using @TO ringActors@.
	Example
	    ringActors A
	Text
	    The simplified version of this function suffices when
	    dealing with resolutions of quotients of the ring
	    @TT "R"@ by an ideal as in the previous example.
	    In this case, the first module in the resolution is
	    @TT "R"@ and it is assumed that the group acts
	    trivially on the generator of this first module.
	    
	    When resolving modules or when more flexibility is 
	    needed, one may use the general version of the function.
	    In this case, it is necessary to specify a homological
	    degree @TT "i"@ and a list of group elements acting on
	    the module @TT "C_i"@. The group elements are passed
	    as a @TO List@ @TT "G'"@ of matrices written with
	    respect to the basis of @TT "C_i"@ used by Macaulay2.
	    Moreover, the group elements in @TT "G'"@ must match
	    (in number and order) the elements in @TT "G"@.
	    
	    To illustrate, we set up the action on the resolution
	    of the ideal in the previous example considered as a
	    module (as opposed to the resolution of the quotient
	    by the ideal). In this case, the elements of @TT "G'"@
	    are the permutation matrices obtained by acting with
	    elements of @TT "G"@ on the span of the minimal
	    generators of the ideal. For simplicity, we construct
	    these matrices by permuting columns of the identity.
    	Example
	    M = module I
	    RM = res M
	    G' = { (id_(R^6))_{2,4,5,0,1,3},
    		   (id_(R^6))_{2,0,1,4,5,3},
    		   (id_(R^6))_{0,4,3,2,1,5},
    		   (id_(R^6))_{0,2,1,4,3,5},
    		    id_(R^6) }
	    action(RM,G,G',0)
	Text
	    By changing the last argument, it is possible to
	    specify the action of the group on any module of the
	    resolution. For example, suppose we wish to construct
	    the action of the symmetric group on the resolution
	    of the canonical module of the quotient in the first
	    example. In this case, it will be more convenient to
	    declare a trivial action on the last module of the
	    resolution rather than figuring out the action on the
	    first module (i.e., the generators of the canonical
	    module). This can be achieved as follows.
    	Example
	    E = Ext^3(R^1/I,R^{-4})
	    RE = res E
	    G'' = toList(5:id_(R^1))
	    action(RE,G,G'',3)
    Caveat
    	This function does not check if the complex @TT "C"@ is a minimal
	free resolution. If the user passes a complex that is not a minimal
	free resolution, then later computations (i.e., Betti characters)
	may fail or return meaningless results.


Node
    Key
    	(action,Module,List,List)
    	(action,Module,List)
    	(action,Ideal,List,List)
    	(action,Ideal,List)
    	(action,PolynomialRing,List,List)
    	(action,PolynomialRing,List)
    	(action,QuotientRing,List,List)
    	(action,QuotientRing,List)
    Headline
    	define finite group action on a graded module
    Usage
    	A=action(M,G)
	A=action(M,G,G')
    Inputs
    	M:Module
	    a graded module/ideal/quotient over a polynomial ring @TT "R"@
	G:List
	    of group elements acting on the variables of @TT "R"@
	G':List
	    of group elements acting on the ambient module of @TT "M"@
    Outputs
    	A:ActionOnGradedModule
    Description
    	Text
	    Use this function to define the action of a finite group
	    on a graded module over a polynomial ring
	    with coefficients in a field. This includes also an
	    ideal in the polynomial ring, a quotient of the
	    polynomial ring, and the polynomial ring itself.
	    After setting up the action, use the function
	    @TO character@ to compute the characters of graded
	    components.
	    
	    The input @TT "G"@ is a @TO List@ of group elements
	    acting on the vector space spanned by the variables
	    of the ring @TT "R"@. By default, these elements are
	    passed as one-row substitution matrices as those
	    accepted by @TO substitute@. One may pass these elements
	    as square matrices by setting the optional input @TO Sub@
	    to @TT "false"@. The list @TT "G"@ can contain
	    arbitrary group elements however, to
	    obtain a complete representation theoretic description
	    of the characters, @TT "G"@ should be a list of
	    representatives of the conjugacy classes of the group.
	    
	    The example below sets up the action of a symmetric
	    group on a polynomial ring, a monomial ideal,
	    and the corresponding quotient.
	    The symmetric group acts by permuting the four
	    variables of the ring. The conjugacy classes of
	    permutations are determined by their cycle types,
	    which are in bijection with partitions. In this case,
	    we consider five permutations with cycle types,
	    in order: 4, 31, 22, 211, 1111.
    	Example
	    R = QQ[x_1..x_4]
	    G = {matrix{{x_2,x_3,x_4,x_1}},
    		 matrix{{x_2,x_3,x_1,x_4}},
    		 matrix{{x_2,x_1,x_4,x_3}},
    		 matrix{{x_2,x_1,x_3,x_4}},
    		 matrix{{x_1,x_2,x_3,x_4}} }
	    action(R,G)
	    I = ideal apply(subsets(gens R,2),product)
    	    action(I,G)
	    Q = R/I
	    A = action(Q,G)
	Text
	    The group elements acting on the ring can be recovered
	    using @TO ringActors@.
	Example
	    ringActors A
	Text
	    The simplified version of this function assumes that
	    the group acts trivially on the generator of the
	    polynomial ring.
	    
	    When working with a module @TT "M"@, one needs to
	    declare the action of the group on a basis of the free
	    ambient module of @TT "M"@.
	    Unless this action is trivial, it can be specified
	    using the third argument, a list @TT "G'"@ of matrices
	    written with respect to the basis of the free ambient
	    module of  @TT "M"@ used by Macaulay2.
	    Moreover, the group elements in @TT "G'"@ must match
	    (in number and order) the elements in @TT "G"@.
	    
	    To illustrate, we set up the action on the canonical
	    module of the quotient in the previous example.
	    We obtain the list of group elements @TT "G'"@ for the
	    canonical module by computing the action on its
	    resolution.
    	Example
	    E = Ext^3(R^1/I,R^{-4})
	    RE = res E
	    G'' = toList(5:id_(R^1))
	    B = action(RE,G,G'',3)
	    G' = actors(B,0)
	    action(E,G,G')


Node
    Key
    	"Equality checks"
    	(symbol ==,ActionOnComplex,ActionOnComplex)
    	(symbol ==,ActionOnGradedModule,ActionOnGradedModule)
    	(symbol ==,Character,Character)
    	(symbol ==,CharacterDecomposition,CharacterDecomposition)
    	(symbol ==,CharacterTable,CharacterTable)
    Headline
    	compare actions and characters
    Description
    	Text
	    Use @TT "=="@ to check if two actions, characters,
	    decompositions or tables are equal.
	    
	    For actions, the underlying ring and object (complex or
	    module) must be the same.   
	    The group elements used to set up the actions being
	    compared must be the same and in the same order.
	    In the case of actions on complexes, the @TT "=="@ operator
	    compares the group action in all homological degrees.
	    In the case of actions on modules, the @TT "=="@ operator
	    compares the group action on the module generators.
	    
	    For characters, the underlying ring must be the same
	    (see @TO "Character class"@),
	    as well as the degree orbit and representative functions
	    (see @TO "Semidirect"@).
	    Characters are compared across all homological and
	    internal degrees.
    	Example
	    R = QQ[x_1..x_4]
	    I = ideal apply(subsets(gens R, 2), product)
	    RI = res I
	    S4 = symmetricGroupActors(R)
	    A = action(RI,S4)
	    G = {map(RI_3, RI_3, {{0, -1, 1}, {1, 1, 0}, {0, 1, 0}}),
		 map(RI_3, RI_3, {{0, 1, 0}, {-1, -1, 0}, {0, -1, 1}}),
		 map(RI_3, RI_3, {{0, -1, 1}, {-1, 0, -1}, {0, 0, -1}}),
		 map(RI_3, RI_3, {{0, 1, 0}, {1, 0, 0}, {0, 0, -1}}),
		 map(RI_3, RI_3, {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}) }
	    B = action(RI,S4,G,3)
	    A == B
	    character A == character B

Node
    Key
    	actors
    Headline
    	group elements of an action
    Description
    	Text
	    This method is used to return lists of matrices
	    representing the action of group elements on the
	    graded components of a module or on the terms of
	    a minimal free resolution.
	    See the specific use cases for more details.
    SeeAlso
    	action	    
    Subnodes
 	(actors,ActionOnComplex,ZZ)  
 	(actors,ActionOnGradedModule,List)
	    
Node
    Key
	(actors,ActionOnComplex,ZZ)
    Headline
    	group elements of action on resolution
    Usage
    	actors(A,i)
    Inputs
    	A:ActionOnComplex
	    a finite group action on a minimal free resolution
    	i:ZZ
	    a homological degree	    
    Outputs
    	:List
	    of group elements acting in homological degree @TT "i"@
    Description
    	Text
    	    This function returns matrices describing elements of a
	    finite group acting on a minimal free resolution in a
	    given homological degree. If the homological degree is
	    the one where the user originally defined the action,
	    then the user provided elements are returned.
	    Otherwise, suitable elements are computed as indicated
	    in @HREF("https://doi.org/10.1016/j.jsc.2022.02.001","F. Galetto - Finite group characters on free resolutions")@.

	    To illustrate, we compute the action of a
	    symmetric group on the resolution of a monomial ideal.
	    The ideal is generated by
	    all squarefree monomials of degree two in four variables.
	    The symmetric group acts by permuting the four
	    variables of the ring. We only consider five
	    permutations with cycle types,
	    in order: 4, 31, 22, 211, 1111 (since these are enough
		to determine the characters of the action).
    	Example	    
	    R = QQ[x_1..x_4]
	    I = ideal apply(subsets(gens R,2),product)
	    RI = res I
	    G = {matrix{{x_2,x_3,x_4,x_1}},
    		 matrix{{x_2,x_3,x_1,x_4}},
    		 matrix{{x_2,x_1,x_4,x_3}},
    		 matrix{{x_2,x_1,x_3,x_4}},
    		 matrix{{x_1,x_2,x_3,x_4}} }
	    A = action(RI,G)
	    actors(A,0)
	    actors(A,1)
	    actors(A,2)
	    actors(A,3)
    Caveat
    	When applied to a minimal free resolution $F_\bullet$,
    	this function returns matrices that induce the action of
	group elements on the representations $F_i/\mathfrak{m}F_i$, where
	$\mathfrak{m}$ is the maximal ideal generated by the variables of the
	polynomial ring.
	While these matrices often represent the action of the
	same group elements on the modules $F_i$ of the resolution,
	this is in general not a guarantee.
    SeeAlso
    	action	    


Node
    Key
	(actors,ActionOnGradedModule,List)
	(actors,ActionOnGradedModule,ZZ)
    Headline
    	group elements acting on components of a module
    Usage
    	actors(A,d)
    Inputs
    	A:ActionOnGradedModule
	    a finite group action on a graded module
	d:List
	    a (multi)degree
    Outputs
    	:List
	    of group elements acting in the given (multi)degree
    Description
    	Text
    	    This function returns matrices describing elements of a
	    finite group acting on the graded component of
	    (multi)degree @TT "d"@ of a module.

	    To illustrate, we compute the action of a
	    symmetric group on the components of a monomial ideal.
	    The symmetric group acts by permuting the four
	    variables of the ring. We only consider five
	    permutations with cycle types,
	    in order: 4, 31, 22, 211, 1111 (since these are enough
		to determine the characters of the action).
    	Example	    
	    R = QQ[x_1..x_4]
	    I = ideal apply(subsets(gens R,2),product)
	    G = {matrix{{x_2,x_3,x_4,x_1}},
          	 matrix{{x_2,x_3,x_1,x_4}},
          	 matrix{{x_2,x_1,x_4,x_3}},
          	 matrix{{x_2,x_1,x_3,x_4}},
          	 matrix{{x_1,x_2,x_3,x_4}} }
	    A = action(I,G)
	    actors(A,1)
	    actors(A,2)
	    actors(A,3)
    	Text
	    The degree argument can be an integer (in the case of
		single graded modules) or a list of integers (in
	    	the case of a multigraded module).
    SeeAlso
    	action	    


Node
    Key
    	character
    Headline
    	compute characters of finite group action
    Description
    	Text
	    Use this method to compute the Betti characters
	    of a finite group action on a minimal free resolution
	    or the characters of a finite group action on the
	    components of a graded module.
	    See the specific use cases for more details.
	    
	    All characters are bigraded by homological degree and
	    internal degree (inherited from the complex or module
		they are computed from). Modules are considered to
	    be concentrated in homological degree zero.
	    
	    Characters may also be constructed by hand using
	    @TO (character,PolynomialRing,HashTable)@.
    Subnodes
    	Character
    	(character,ActionOnComplex)
    	(character,ActionOnComplex,ZZ)
     	(character,ActionOnGradedModule,List)
	(character,PolynomialRing,HashTable)
	(character,CharacterDecomposition,CharacterTable)
	    
Node
    Key
    	(character,ActionOnComplex)
    Headline
    	compute all Betti characters of minimal free resolution
    Usage
    	character(A)
    Inputs
    	A:ActionOnComplex
	    a finite group action on a minimal free resolution
    Outputs
    	:Character
	    Betti characters of the resolution
    Description
    	Text
	    Use this function to compute all nonzero Betti
	    characters of a finite group action on a minimal free
	    resolution.
	    This function calls @TO (character,ActionOnComplex,ZZ)@
	    on all nonzero homological degrees and then assembles
	    the outputs in a hash table indexed by homological
	    degree.

	    To illustrate, we compute the Betti characters of a
	    symmetric group on the resolution of a monomial ideal.
	    The ideal is the symbolic square of the ideal generated by
	    all squarefree monomials of degree three in four variables.
	    The symmetric group acts by permuting the four
	    variables of the ring. The characters are determined
	    by five permutations with cycle types,
	    in order: 4, 31, 22, 211, 1111.
	Example
	    R = QQ[x_1..x_4]
	    J = intersect(apply(subsets(gens R,3),x->(ideal x)^2))
	    RJ = res J
	    G = { matrix{{x_2,x_3,x_4,x_1}},
    		  matrix{{x_2,x_3,x_1,x_4}},
    		  matrix{{x_2,x_1,x_4,x_3}},
    		  matrix{{x_2,x_1,x_3,x_4}},
    		  matrix{{x_1,x_2,x_3,x_4}} }
	    A = action(RJ,G)
	    character(A)
	Text
	    See @TO (character,ActionOnComplex,ZZ)@
	    for more details on this example.
    SeeAlso
    	action
	(character,ActionOnComplex,ZZ)


Node
    Key
    	(character,ActionOnComplex,ZZ)
    Headline
    	compute Betti characters of minimal free resolution
    Usage
    	character(A,i)
    Inputs
    	A:ActionOnComplex
	    a finite group action on a minimal free resolution
	i:ZZ
	    a homological degree
    Outputs
    	:Character
	    the @TT "i"@-th Betti character of the resolution
    Description
    	Text
	    Use this function to compute the Betti characters of a
	    finite group action on a minimal free resolution
	    in a given homological degree.
	    More explicitly, let $F_\bullet$ be a minimal free
	    resolution of a module $M$ over a polynomial ring $R$,
	    with a compatible action of a finite group $G$.
	    If $\mathfrak{m}$ denotes the maximal ideal generated by the
	    variables of $R$, then $F_i/\mathfrak{m}F_i$ is a graded
	    representation of $G$. We refer to its character as
	    the $i$-th {\bf Betti character} of $M$ (or a minimal free
	    resolution of $M$).
	    Betti characters are computed using Algorithm 1 in
	    @HREF("https://doi.org/10.1016/j.jsc.2022.02.001","F. Galetto - Finite group characters on free resolutions")@.

	    To illustrate, we compute the Betti characters of a
	    symmetric group on the resolution of a monomial ideal.
	    The ideal is the symbolic square of the ideal generated by
	    all squarefree monomials of degree three in four variables.
	    The symmetric group acts by permuting the four
	    variables of the ring. The characters are determined
	    by five permutations with cycle types,
	    in order: 4, 31, 22, 211, 1111.
	Example
	    R = QQ[x_1..x_4]
	    J = intersect(apply(subsets(gens R,3),x->(ideal x)^2))
	    RJ = res J
	    G = { matrix{{x_2,x_3,x_4,x_1}},
    		  matrix{{x_2,x_3,x_1,x_4}},
    		  matrix{{x_2,x_1,x_4,x_3}},
    		  matrix{{x_2,x_1,x_3,x_4}},
    		  matrix{{x_1,x_2,x_3,x_4}} }
	    A = action(RJ,G)
	    character(A,0)
	Text
	    By construction, the character in homological degree
	    0 is concentrated in degree 0 and trivial.
	Example
	    character(A,1)
	Text
	    The character in homological degree 1 has two
	    components. The component of degree 3 is the permutation
	    representation spanned by the squarefree monomials of
	    degree 3 (which can be identified with the natural
		representation of the symmetric group).
	    The component of degree 4 is the permutation representation
	    spanned by the squares of the squarefree monomials of degree
	    2.
	Example
	    character(A,2)
	Text
	    In homological degree 2, there is a component of degree
	    4 which is isomorphic to the irreducible standard
	    representation of the symmetric group.
	    In degree 5, we find the permutation representation of
	    the symmetric group on the set of ordered pairs of
	    distinct elements from 1 to 4.
	Example
	    character(A,3)
	Text
	    Finally, the character in homological degree 3 is
	    concentrated in degree 6 and corresponds to the direct
	    sum of the standard representation and the tensor
	    product of the standard representation and the sign
	    representation (i.e., the direct sum of the two
		irreducible representations of dimension 3).
    SeeAlso
    	action


Node
    Key
    	(character,ActionOnGradedModule,List)
    	(character,ActionOnGradedModule,ZZ)
    	(character,ActionOnGradedModule,ZZ,ZZ)
    Headline
    	compute characters of graded components of a module
    Usage
    	character(A,d)
    Inputs
    	A:ActionOnGradedModule
	    a finite group action on a graded module
	d:List
	    a (multi)degree
    Outputs
    	:Character
	    the character of the components of a module in given degrees
    Description
    	Text
	    Use this function to compute the characters of the
	    finite group action on the graded components of a
	    module. The second argument is the multidegree (as a list)
	    or the degree (as an integer) of the desired component.

	    To illustrate, we compute the Betti characters of a
	    symmetric group on the graded components of a quotient ring.
	    The characters are determined
	    by five permutations with cycle types,
	    in order: 4, 31, 22, 211, 1111.
	Example
	    R = QQ[x_1..x_4]
	    I = ideal apply(subsets(gens R,2),product)
	    G = {matrix{{x_2,x_3,x_4,x_1}},
          	 matrix{{x_2,x_3,x_1,x_4}},
          	 matrix{{x_2,x_1,x_4,x_3}},
          	 matrix{{x_2,x_1,x_3,x_4}},
          	 matrix{{x_1,x_2,x_3,x_4}} }
	    Q = R/I
	    A = action(Q,G)
	    character(A,0)
	    character(A,1)
    Synopsis
    	Usage
	    character(A,lo,hi)
    	Inputs
	    A:ActionOnGradedModule
	    	a finite group action on a graded module
    	    lo:ZZ
	    	the low degree
    	    hi:ZZ
	    	the high degree
    	Outputs
	    :Character
	    	the character of the components of a module in the given range of degrees
    Description
    	Text
	    For $\mathbb{Z}$-graded modules,
	    one may compute characters in a range of degrees by
	    providing the lowest and highest degrees in the range
	    as the second and third argument.
	Example
	    character(A,0,4)
    SeeAlso
    	action

	    
Node
    Key
    	(character,PolynomialRing,HashTable)
    Headline
    	construct a character
    Usage
    	character(R,H)
    Inputs
    	R:PolynomialRing
	    a polynomial ring over a field
    	H:HashTable
	    raw character data
    Outputs
    	:Character
    Description
    	Text
	    The @TO character@ method is mainly designed to compute
	    characters of finite group actions defined via @TO action@.
	    The user who wishes to define characters by hand
	    may do so with this particular application of the method.
	    
	    The first argument is a polynomial ring over a field. The
	    character inherits the grading of this polynomial ring
	    and takes values in the field of coefficients.
	    The second argument is a hash table
	    containing the "raw" character data. The hash table
	    entries are in the format @TT "(i,d) => c"@, where @TT "i"@
	    is an integer representing homological degree, @TT "d"@
	    is a list representing the internal (multi)degree, and
	    @TT "c"@ is a one-row matrix containing the values of the
	    character in the given degrees.
	Example
	    R = QQ[x_1..x_3]
	    regularRepresentation = character(R, hashTable {
		    (0,{0}) => matrix{{1,1,1}},
		    (0,{1}) => matrix{{-1,0,2}},
		    (0,{2}) => matrix{{-1,0,2}},
		    (0,{3}) => matrix{{1,-1,1}},
		    })
	    I = ideal(x_1+x_2+x_3,x_1*x_2+x_1*x_3+x_2*x_3,x_1*x_2*x_3)
	    S3 = {matrix{{x_2,x_3,x_1}},
		  matrix{{x_2,x_1,x_3}},
		  matrix{{x_1,x_2,x_3}} }
	    Q = R/I
	    A = action(Q,S3)
	    character(A,0,3) == regularRepresentation
    Caveat
    	This constructor implements basic consistency checks, but
	it is still possible to construct objects that are not
	actually characters (not even virtual).
    SeeAlso
    	character

Node
    Key
    	(character,CharacterDecomposition,CharacterTable)
	(symbol *,CharacterDecomposition,CharacterTable)
    Headline
    	recover character from decomposition
    Usage
    	character(d,T)
	d*T
    Inputs
    	d:CharacterDecomposition
    	T:CharacterTable
    Outputs
    	:Character
    Description
    	Text
	    Use this function to recover a character from its decomposition
	    into a linear combination of the irreducible characters
	    in a character table. The shortcut @TT "d*T"@
	    is equivalent to the command @TT "character(d,T)"@.
	    
	    As an example, we construct the character table of the
	    symmetric group on 3 elements, then use it to decompose
	    the character of the action of the same symmetric group
	    permuting the variables of a standard graded polynomial ring.
	Example
	    s = {2,3,1}
	    M = matrix{{1,1,1},{-1,0,2},{1,-1,1}}
	    P = {1,2,3}
	    T = characterTable(s,M,QQ,P)
	    R = QQ[x_1..x_3]
	    acts = {matrix{{x_2,x_3,x_1}},matrix{{x_2,x_1,x_3}},matrix{{x_1,x_2,x_3}}}
	    A = action(R,acts)
	    c = character(A,0,10)
	    d = c/T
	    c == d*T
    SeeAlso
    	characterTable
	decomposeCharacter

Node
    Key
    	characterTable
    	(characterTable,List,Matrix,Ring,RingMap)
    	(characterTable,List,Matrix,Ring,List)
    Headline
    	construct a character table
    Usage
    	T = characterTable(s,M,F,conj)
    	T = characterTable(s,M,F,perm)
    Inputs
    	s:List
	    of conjugacy class sizes
    	M:Matrix
	    with character table entries
    	F:Ring
	    a field
    	conj:RingMap
	    conjugation in coefficient field
    	perm:List
	    permutation of conjugacy classes
    Outputs
    	T:CharacterTable
    Description
    	Text
	    Use the @TO characterTable@ method to construct
	    the character table of a finite group.
	    
	    The first argument is a list containing the cardinalities
	    of the conjugacy classes of the group.
	    
	    The second argument is a square matrix whose entry in
	    row $i$ and column $j$ is the value of the $i$-th
	    irreducible character of the group at an element
	    of the $j$-th conjugacy class.
	    
	    The third argument is a field, the field of definition
	    of the characters to be decomposed against the character
	    table. Note that the matrix in the second argument must
	    be liftable to this field. (In version 2.1 and earlier,
		this argument used to be a polynomial ring.)
	    
	    Assuming the field in the third argument is a subfield
	    of the complex numbers, then the fourth argument is the
	    restriction of complex conjugation to @TT "F"@.
	    
	    For example, we construct the character table of the
	    alternating group $A_4$ considered as a subgroup of the
	    symmetric group $S_4$. The conjugacy classes are
	    represented by the identity, and the permutations
	    $(12)(34)$, $(123)$, and $(132)$, in cycle notation.
	    These conjugacy classes have cardinalities: 1, 3, 4, 4.
	    The irreducible characters can be constructed over the
	    field $\mathbb{Q}[w]$, where $w$ is a primitive third
	    root of unity. Complex conjugation restricts to
	    $\mathbb{Q}[w]$ by sending $w$ to $w^2$.
    	Example
	    F = toField(QQ[w]/ideal(1+w+w^2))
	    s = {1,3,4,4}
	    M = matrix{{1,1,1,1},{1,1,w,w^2},{1,1,w^2,w},{3,-1,0,0}}
	    conj = map(F,F,{w^2})
	    T = characterTable(s,M,F,conj)
    	Text	    
	    By default, irreducible characters in a character table
	    are labeled as $\chi_0, \chi_1, \dots$, etc.
	    The user may pass custom labels in a list using
	    the option @TO Labels@.
	    
	    When working over a splitting field for a finite group
	    $G$ in the non modular case, the irreducible characters
	    of $G$ form an orthonormal basis for the space of class
	    functions on $G$ with the scalar product given by
	    $$\langle \chi_1, \chi_2 \rangle = \frac{1}{|G|}
	    \sum_{g\in G} \chi_1 (g) \chi_2 (g^{-1}).$$
    	    Over the complex numbers, the second factor in the summation
	    is equal to $\overline{\chi_2 (g)}$. Thus the scalar
	    product can be computed using the conjugation function
	    provided by the user.
	    
    	    If working over coefficient fields of positive characteristic
	    or if one wishes to avoid defining conjugation, one may replace
	    the fourth argument by a list containing a permutation
	    $\pi$ of the integers $1,\dots,r$, where
	    $r$ is the number of conjugacy classes of the group.
	    The permutation $\pi$ is defined as follows:
	    if $g$ is an element of the $j$-th conjugacy class,
	    then $g^{-1}$ is an element of the $\pi (j)$-th class.
	    
	    In the case of $A_4$, the identity and $(12)(34)$ are
	    their own inverses, while $(123)^{-1} = (132)$.
	    Therefore the permutation $\pi$ is the transposition
	    exchanging 3 and 4. Hence the character table of $A_4$
	    may also be constructed as follows, with $\pi$
	    represented in one-line notation by a list passed
	    as the fourth argument.
    	Example
	    perm = {1,2,4,3}
	    T' = characterTable(s,M,F,perm)
	    T' == T
    Caveat
    	This constructor checks orthonormality of the table
	matrix under the standard scalar product of characters.
	However, it may still be possible to construct a table
	that is not an actual character table. Note also that
	there are no further checks when using a character table
	to decompose characters.
    SeeAlso
    	decomposeCharacter
    Subnodes
    	CharacterTable
	Labels

Node
    Key
    	decomposeCharacter
    	(decomposeCharacter,Character,CharacterTable)
	(symbol /,Character,CharacterTable)
    Headline
    	decompose a character into irreducible characters
    Usage
    	decomposeCharacter(c,T)
	c/T
    Inputs
    	c:Character
	    of a finite group
    	T:CharacterTable
	    of the same finite group
    Outputs
    	:CharacterDecomposition
    Description
    	Text
	    Use the @TO decomposeCharacter@ method to decompose
	    a character into a linear combination of irreducible
	    characters in a character table. The shortcut @TT "c/T"@
	    is equivalent to the command @TT "decomposeCharacter(c,T)"@.
	    
	    As an example, we construct the character table of the
	    symmetric group on 3 elements, then use it to decompose
	    the character of the action of the same symmetric group
	    permuting the variables of a standard graded polynomial ring.
	Example
	    s = {2,3,1}
	    M = matrix{{1,1,1},{-1,0,2},{1,-1,1}}
	    P = {1,2,3}
	    T = characterTable(s,M,QQ,P)
	    R = QQ[x_1..x_3]
	    acts = {matrix{{x_2,x_3,x_1}},matrix{{x_2,x_1,x_3}},matrix{{x_1,x_2,x_3}}}
	    A = action(R,acts)
	    c = character(A,0,10)
	    decomposeCharacter(c,T)
	Text
	    The results are shown in a table whose rows are indexed
	    by pairs of homological and internal degrees, and whose
	    columns are labeled by the irreducible characters.
	    By default, irreducible characters in a character table
	    are labeled as $\chi_0, \chi_1, \dots$, etc, and the same
	    labeling is inherited by the character decomposition.
	    The user may pass custom labels in a list using
	    the option @TO Labels@ when constructing the character
	    table.
    SeeAlso
    	characterTable
    Subnodes
    	CharacterDecomposition

Node
    Key
    	(directSum,Character)
	(symbol ++,Character,Character)
	(symbol +,Character,Character)
	(plus,Character,Character)
    Headline
    	direct sum of characters
    Usage
    	character(c)
    	character(c1,c2,...)
    Inputs
    	c:Character
	    or sequence of characters
    Outputs
    	:Character
    Description
    	Text
	    Returns the direct sum of the input characters.
	    The operators @TT "+"@ and @TT "++"@ and the function
	    @TO (plus,Character,Character)@ may be used for the same purpose.
	Example
	    R = QQ[x_1..x_3]
	    I = ideal(x_1+x_2+x_3)
	    J = ideal(x_1-x_2,x_1-x_3)
	    S3 = {matrix{{x_2,x_3,x_1}},
		  matrix{{x_2,x_1,x_3}},
		  matrix{{x_1,x_2,x_3}} }
	    A = action(I,S3)
	    B = action(J,S3)
	    a = character(A,1)
	    b = character(B,1)
	    a ++ b
	    K = ideal(x_1,x_2,x_3)
	    C = action(K,S3)
	    c = character(C,1)
	    a ++ b === c

Node
    Key
    	dual
	(dual,Character,RingMap)
    	(dual,Character,List)
    Headline
    	dual character
    Usage
    	dual(c,conj)
    	dual(c,perm)
    Inputs
    	c:Character
	    of a finite group action
    	conj:RingMap
	    conjugation in coefficient field
    	perm:List
	    permutation of conjugacy classes
    Outputs
    	:Character
    Description
    	Text
	    Returns the dual of a character, i.e., the character
	    of the dual or contragredient representation.
	    
	    The first argument is the original character.
	    
	    Assuming the polynomial ring over which the character
	    is defined has a coefficient field @TT "F"@ which is a subfield
	    of the complex numbers, then the second argument is the
	    restriction of complex conjugation to @TT "F"@.
	    
	    As an example, we construct a character of the
	    alternating group $A_4$ considered as a subgroup of the
	    symmetric group $S_4$. The conjugacy classes are
	    represented by the identity, and the permutations
	    $(12)(34)$, $(123)$, and $(132)$, in cycle notation.
	    The character is constructed over the field $\mathbb{Q}[w]$,
	    where $w$ is a primitive third root of unity.
	    Complex conjugation restricts to $\mathbb{Q}[w]$
	    by sending $w$ to $w^2$. The character is concentrated
	    in homological degree 1, and internal degree 2.
	Example
	    F = toField(QQ[w]/ideal(1+w+w^2))
	    R = F[x_1..x_4]
	    conj = map(F,F,{w^2})
	    X = character(R,hashTable {(1,{2}) => matrix{{1,1,w,w^2}}})
	    X' = dual(X,conj)
    	Text
    	    If working over coefficient fields of positive characteristic
	    or if one wishes to avoid defining conjugation, one may replace
	    the second argument by a list containing a permutation
	    $\pi$ of the integers $1,\dots,r$, where
	    $r$ is the number of conjugacy classes of the group.
	    The permutation $\pi$ is defined as follows:
	    if $g$ is an element of the $j$-th conjugacy class,
	    then $g^{-1}$ is an element of the $\pi (j)$-th class.
	    
	    In the case of $A_4$, the identity and $(12)(34)$ are
	    their own inverses, while $(123)^{-1} = (132)$.
	    Therefore the permutation $\pi$ is the transposition
	    exchanging 3 and 4. Hence the dual of the character in the
	    example above may also be constructed as follows,
	    with $\pi$ represented in one-line notation by a list passed
	    as the second argument.
    	Example
    	    perm = {1,2,4,3}
	    dual(X,perm) === X'
    	Text
	    The page @TO characterTable@ contains some motivation
	    for using conjugation or permutations of conjugacy
	    classes when dealing with characters.
    SeeAlso
    	characterTable

Node
    Key
    	hyperoctahedralGroupActors
    	(hyperoctahedralGroupActors,PolynomialRing)
    Headline
    	standard action of the hyperoctahedral group
    Usage
    	hyperoctahedralGroupActors(R)
    Inputs
    	R:PolynomialRing
    Outputs
    	:List
    Description
    	Text
	    The hyperoctahedral group is the Weyl group of type B. It can be
	    realized as the automorphism group of the hypercube, as the group of
	    signed permutation matrices, or as the semidirect product
	    $\mathbb{Z}_2^n \rtimes S_n$ of the symmetric group $S_n$ acting on
	    $\mathbb{Z}_2^n$ by permutations. The standard action on a polynomial
	    ring in $n$ variables is the multiplication action of the signed
	    $n\times n$ permutation matrices on the vector of the variables.
	    
	    This function returns a list of of matrices, each representing an
	    element of the hyperoctahedral group acting on the variables
	    of the polynomial ring in the input as a signed permutation. This
	    simplifies the setup for hyperoctahedral group actions with the
	    @TO action@ command.
	    
	    The output list
	    contains one element for each conjugacy class of
	    the hyperoctahedral group. The conjugacy classes are
	    in bijection with the bipartitions of $n$, i.e., pairs of partitions
	    of two integers adding up to $n$, where $n$ is the
	    number of variables. The first partition gives the cycle type of a
	    permutation of an initial subset of variables without signs changes.
	    The second partition gives the cycle type of a permutation of the
	    remaining variables with each cycle containing a single sign change.
    	Example
	    R=QQ[x_1..x_3]
	    hyperoctahedralGroupActors(R)

Node
    Key
    	hyperoctahedralGroupTable
    	(hyperoctahedralGroupTable,ZZ,Ring)
    Headline
    	character table of the hyperoctahedral group
    Usage
    	hyperoctahedralGroupTable(n,F)
    Inputs
    	n:ZZ
	    positive
    	F:Ring
	    a field
    Outputs
    	:CharacterTable
    Description
    	Text
	    Returns the character table of the hyperoctahedral group
	    $H_n$ over the field @TT "F"@. The irreducible
	    characters are indexed by bipartitions of $n$, i.e.,
	    pairs of partitions of two integers adding up to $n$.
    	Example
	    hyperoctahedralGroupTable(3,QQ)

Node
    Key
    	Labels
	[characterTable, Labels]
    Headline
    	custom labels for irreducible characters
    Description
    	Text
	    This optional input is used with the method
	    @TO characterTable@  provided by the package
	    @TO BettiCharacters@.
	    
	    By default, irreducible characters in a character table
	    are labeled as $\chi_0, \chi_1, \dots$, etc.
	    The user may pass custom labels in a list using
	    this option. Labels can be passed as a list containing two lists:
	    the first list should contain strings or nets to label
	    characters in a M2 interactive session, while the second list
	    should contain TeX strings to label characters when outputting
	    to TeX format (remember to escape backslashes as needed).
	    Up to version 2.1, a single list of net labels
	    was accepted; this option is maintained for compatibility (the same
		labels are also used for the TeX output).
	    
	    The next example sets up the character table of
	    the dihedral group $D_4$, generated by an order 4 rotation $r$
	    and an order 2 reflection $s$ with the relation $srs=r^3$.
	    The representatives of the conjugacy classes are, in order:
	    the identity, $r^2$, $r$, $s$, and $rs$.
	    Besides the trivial representation, $D_4$ has three irreducible
	    one-dimensional representations, corresponding to the three normal
	    subgroups of index two: $\langle r\rangle$, $\langle r^,,s\rangle$,
	    and $\langle r^2,rs\rangle$. The characters of these representations
	    send the elements of the corresponding subgroup to 1, and the other
	    elements to -1. We denote those characters @TT "rho1,rho2,rho3"@.
	    Finally, there is a unique irreducible representation of dimension 2.
    	Example
	    R = QQ[x,y]
	    D8 = { matrix{{x,y}},
	   	   matrix{{-x,-y}},
	   	   matrix{{-y,x}},
		   matrix{{x,-y}},
		   matrix{{y,x}} }
	    M = matrix {{1,1,1,1,1},
		{1,1,1,-1,-1},
		{1,1,-1,1,-1},
		{1,1,-1,-1,1},
		{2,-2,0,0,0}};
	    T = characterTable({1,1,2,2,2},M,QQ,{1,2,3,4,5},
		Labels=>{{"triv","rho1","rho2","rho3","dim2"},
		    {"triv","\\rho_1","\\rho_2","\\rho_3","\\chi^2"}})
	    tex T
    	Text
	    The same labels are automatically used when decomposing
	    characters against a labeled character table.
    	Example
	    A = action(R,D8)
	    c = character(A,0,5)
	    d = decomposeCharacter(c,T)
	    tex d
    	Text
	    The labels are stored in the character table under the
	    key @TT "Labels"@. In particular, two character tables
	    of the same group that are equal in all aspects except
	    for their labels will fail an equality check.
    SeeAlso
    	characterTable
	decomposeCharacter


Node
    Key
    	(net,Action)
    Headline
    	format for printing, as a net
    Description
    	Text
	    Format objects of type @TO Action@ for printing.
	    See @TO net@ for more information.

Node
    Key
    	(net,Character)
    Headline
    	format for printing, as a net
    Description
    	Text
	    Format objects of type @TO Character@ for printing.
	    See @TO net@ for more information.

Node
    Key
    	(net,CharacterTable)
    Headline
    	format for printing, as a net
    Description
    	Text
	    Format objects of type @TO CharacterTable@ for printing.
	    See @TO net@ for more information.

Node
    Key
    	(net,CharacterDecomposition)
    Headline
    	format for printing, as a net
    Description
    	Text
	    Format objects of type @TO CharacterDecomposition@ for printing.
	    See @TO net@ for more information.

Node
    Key
    	(texMath,Character)
    Headline
    	convert to TeX math format
    Description
    	Text
	    Format objects of type @TO Character@
	    for printing in TeX format.
	    See @TO texMath@ for more information.

Node
    Key
    	(texMath,CharacterTable)
    Headline
    	convert to TeX math format
    Description
    	Text
	    Format objects of type @TO CharacterTable@
	    for printing in TeX format.
	    See @TO texMath@ for more information.

Node
    Key
    	(texMath,CharacterDecomposition)
    Headline
    	convert to TeX math format
    Description
    	Text
	    Format objects of type @TO CharacterDecomposition@
	    for printing in TeX format.
	    See @TO texMath@ for more information.

Node
    Key
    	ringActors
    	(ringActors,Action)
    Headline
    	get action on ring generators
    Usage
    	ringActors(A)
    Inputs
    	A:Action
    Outputs
    	G:List
	    of group elements
    Description
    	Text
	    Returns a @TO List@ of group elements
	    acting on the vector space spanned by the variables
	    of the polynomial ring associated with the object
	    acted upon.
	    These are the same elements originally defined by
	    the user when constructing the action.
	    By default, these elements are
	    expressed as one-row substitution matrices as those
	    accepted by @TO substitute@. One may obtain these elements
	    as square matrices by setting the optional input @TO Sub@
	    to @TT "false"@.
    SeeAlso
    	action


Node
    Key
    	Semidirect
	[action, Semidirect]
	[character, Semidirect]
	[(symbol *, CharacterDecomposition, CharacterTable), Semidirect]
	degreeOrbit
	degreeRepresentative
    Headline
    	action of semidirect product with torus
    Description
    	Text
	    Consider a polynomial ring $R$ and an $R$-module $M$
	    with a $\mathbb{Z}^r$-grading corresponding to the action of a
	    torus $T$. Let $G$ be a finite group acting on $R$
	    and $M$ in a way that is compatible with multiplication.
	    Then the semidirect product $T\rtimes G$ acts on $R$ and $M$.
	    For a given degree $d \in \mathbb{Z}^r$, the graded
	    components $R_d$ and $M_d$ need not be representations of $G$.
	    However, if $\mathcal{O}$ is the orbit of $d$ under the action
	    of $G$ on the character group $\mathbb{Z}^r$ of $T$, then
	    $\bigoplus_{d\in\mathcal{O}} R_d$ and
	    $\bigoplus_{d\in\mathcal{O}} M_d$ are representations of
	    $T\rtimes G$. Starting with version 2.3, the
	    @TO "BettiCharacters"@ package allows one to compute the
	    characters of $G$ on these representations using the
	    @TO "Semidirect"@ option of the @TO "action"@ method,
	    and specifying a single degree $d$ in the orbit $\mathcal{O}$.

	    The value of the @TO "Semidirect"@ option is a list of two
	    functions. The first function takes as input a degree $d$
	    and returns its orbit $\mathcal{O}$ as output. This function is
	    stored in the action under the key @TO "degreeOrbit"@. The second
	    function takes as input a degree $d$ and returns a user-chosen
	    representative $d'$ from the orbit $\mathcal{O}$ of $d$. This
	    function is stored in the action under the key @TO "degreeRepresentative"@.
	    When computing the actors or the characters of $G$ on
	    $\bigoplus_{d\in\mathcal{O}} R_d$ and $\bigoplus_{d\in\mathcal{O}} M_d$,
	    the values are stored only for the chosen representative $d'$,
	    and computing the actors or characters of $G$ for another degree
	    in the same orbit produces the same result as for $d'$.
	    By default, both functions are set to the identity, which
	    corresponds to the action of the direct product $T\times G$.

	    A typical use case is that of the symmetric group $\mathfrak{S}_n$
	    acting on a fine graded polynomial ring $\Bbbk [x_1,\dots,x_n]$ by
	    permuting the variables. The symmetric group also acts by
	    permuting the entries of the degrees $d \in \mathbb{Z}^n$.
	    In this case, the orbit of $d$ consists of all its permutations,
	    which can be obtained with the function @TO "uniquePermutations"@.
	    As a representative of this orbit we choose the unique degree $d$
	    whose entries are sorted in nonincreasing order from left to right;
	    this can be obtained with the function @TO "rsort"@.

	    We illustrate this use case. First, consider the action
	    on the polynomial ring.
    	Example
	    R = QQ[x_1..x_4,Degrees=>{{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}]
	    S4 = symmetricGroupActors R
	    A = action(R,S4,Semidirect=>{uniquePermutations,rsort})
	    actors(A,{1,1,1,0})
	    character(A,{1,1,1,0})
	Text
	    As expected, the character is the same if we compute it
	    for a different degree in the same orbit.
	Example
	    oo == character(A,{1,0,1,1})
	Text
	    Next, consider the quotient by an ideal stable under the group action.
	Example
	    I = ideal apply(subsets(gens R,3),product)
	    M = R/I
	    B = action(M,S4,Semidirect=>{uniquePermutations,rsort})
	    character(B,{2,1,0,0})
    	Text
	    Similarly, the @TO "Semidirect"@ option can be used
	    for actions on complexes and for computing Betti characters
	    of a module.
    	Example
	    RI = res I
	    C = action(RI,S4,Semidirect=>{uniquePermutations,rsort})
	    character C
    SeeAlso
	action
	
Node
    Key
    	Sub
	[action, Sub]
	[ringActors, Sub]
    Headline
    	format ring actors as one-row substitution matrices
    Description
    	Text
	    By default, the group elements acting on a ring are
	    passed as one-row substitution matrices as those
	    accepted by @TO substitute@. Setting @TT "Sub=>false"@
	    allows the user to pass these elements as square
	    matrices.
	    
	    The example below sets up the action of a symmetric
	    group on the resolution of a monomial ideal.
	    The symmetric group acts by permuting the four
	    variables of the ring. The conjugacy classes of
	    permutations are determined by their cycle types,
	    which are in bijection with partitions. In this case,
	    we consider five permutations with cycle types,
	    in order: 4, 31, 22, 211, 1111.
	    For simplicity, we construct these matrices
	    by permuting columns of the identity.
    	Example
	    R = QQ[x_1..x_4]
	    I = ideal apply(subsets(gens R,2),product)
	    RI = res I
	    G = { (id_(R^4))_{1,2,3,0},
    		  (id_(R^4))_{1,2,0,3},
    		  (id_(R^4))_{1,0,3,2},
    		  (id_(R^4))_{1,0,2,3},
    		   id_(R^4) }
	    A = action(RI,G,Sub=>false)
    	Text
	    Similarly, setting @TT "Sub=>false"@
	    causes @TO ringActors@
	    to return the group elements acting on the ring as
	    square matrices. With the default setting
	    @TT "Sub=>true"@, the same elements are returned as
	    one-row substitution matrices.
    	Example
	    ringActors(A,Sub=>false)
	    ringActors(A)


Node
    Key
    	symmetricGroupActors
    	(symmetricGroupActors,PolynomialRing)
    Headline
    	permutation action of the symmetric group
    Usage
    	symmetricGroupActors(R)
    Inputs
    	R:PolynomialRing
    Outputs
    	:List
    Description
    	Text
	    Returns a list of of matrices, each representing an
	    element of the symmetric group permuting the variables
	    of the polynomial ring in the input. This simplifies
	    the setup for symmetric group actions with the
	    @TO action@ command.
	    
	    The output list
	    contains one element for each conjugacy class of
	    the symmetric group. The conjugacy classes are
	    determined by their cycle type and are in bijection
	    with the partitions of $n$, where $n$ is the
	    number of variables. Therefore the first element
	    of the list will always be a cycle of length $n$,
	    and the last element will be the identity.
    	Example
	    R=QQ[x_1..x_4]
	    symmetricGroupActors(R)
	    partitions 4
    SeeAlso
	"BettiCharacters Example 1"
	"BettiCharacters Example 2"

Node
    Key
    	symmetricGroupTable
    	(symmetricGroupTable,ZZ,Ring)
    	(symmetricGroupTable,PolynomialRing)
    Headline
    	character table of the symmetric group
    Usage
    	symmetricGroupTable(n,F)
    Inputs
    	n:ZZ
	    positive
    	F:Ring
	    a field
    Outputs
    	:CharacterTable
    Description
    	Text
	    Returns the character table of the symmetric group
	    $S_n$ over the field @TT "F"@. The irreducible
	    characters are indexed by the partitions of $n$ written
	    using a compact notation where an exponent indicates
	    how many times a part is repeated. The computation uses
	    the recursive Murnaghan-Nakayama formula.
    	Example
	    symmetricGroupTable(4,QQ)
    	Text
	    If @TT "R"@ is a polynomial ring, then
	    @TT "symmetricGroupTable R"@ calls
	    @TT "symmetricGroupTable(numgens R,coefficientRing R)"@.
	    This is kept for compatibility with versions 2.1 and earlier
	    of the package to create the character table of the symmetric
	    group acting on the variables of @TT "R"@ over the
	    coefficient field of @TT "R"@.
    SeeAlso
	"BettiCharacters Example 1"
	"BettiCharacters Example 2"

Node
    Key
    	(tensor,Character,Character)
	(symbol **,Character,Character)
	(symbol ^**,Character,ZZ)
    Headline
    	tensor product of characters
    Usage
    	tensor(c1,c2)
    Inputs
    	c1:Character
    	c2:Character
    Outputs
    	:Character
    Description
    	Text
	    Returns the tensor product of the input characters.
	    The operator @TT "**"@ may be used for the same purpose.
	    
	    We construct the character of the coinvariant algebra
	    of the symmetric group on 3 variables.
	Example
	    R = QQ[x,y,z]
	    I = ideal(x+y+z,x*y+x*z+y*z,x*y*z)
	    S3 = symmetricGroupActors R
	    A = action(R/I,S3)
	    a = character(A,0,3)
    	Text
	    The Gorenstein duality of this character may be
	    observed by tensoring with the character of the
	    sign representation concentrated in degree 3.
    	Example
	    signrep = character(R, hashTable { (0,{3}) => matrix{{1,-1,1}} })
	    dual(a,{1,2,3}) ** signrep === a
    Synopsis
    	Usage
	    c ^** m
    	Inputs
	    A:Character
    	    m:ZZ
    	Outputs
	    :Character
	    	the m-th tensor power of c
    Description
	Text
	    Starting with version 2.5, this package allows nonnegative
	    tensor powers of characters using @TO (symbol ^**,Character,ZZ)@.
	Example
	    a ^** 3

///
	    
----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

-- Test 0 (monomial ideal, symmetric group)
TEST ///
clearAll
R = QQ[x,y,z]
I = ideal(x*y,x*z,y*z)
RI = res I
S3 = {matrix{{y,z,x}},matrix{{y,x,z}},matrix{{x,y,z}}}
assert(S3 == symmetricGroupActors(R))
A = action(RI,S3)
a = character(R,hashTable {
    ((0,{0}), matrix{{1,1,1}}),
    ((1,{2}), matrix{{0,1,3}}),
    ((2,{3}), matrix{{-1,0,2}})
    })
assert((character A) == a)
B = action(R,S3)
b = character(R,hashTable {
    ((0,{0}), matrix{{1,1,1}}),
    ((0,{1}), matrix{{0,1,3}}),
    ((0,{2}), matrix{{0,2,6}}),
    ((0,{3}), matrix{{1,2,10}})
    })
assert(character(B,0,3) == b)
C = action(I,S3)
c = character(R,hashTable {
    ((0,{2}), matrix{{0,1,3}}),
    ((0,{3}), matrix{{1,1,7}})
    })
assert(character(C,0,3) == c)
D = action(R/I,S3)
d = character(R,hashTable {
    ((0,{0}), matrix{{1,1,1}}),
    ((0,{1}), matrix{{0,1,3}}),
    ((0,{2}), matrix{{0,1,3}}),
    ((0,{3}), matrix{{0,1,3}})
    })
assert(character(D,0,3) == d)
assert(b == c++d)
cS3 = symmetricGroupTable(3,QQ)
assert( cS3.table ==
    matrix{{1_QQ,1,1},{-1,0,2},{1,-1,1}})
adec = a/cS3
assert( set keys adec.decompose == set {0,1,2})
CR = a.degreesRing
T = CR_0
assert( adec.decompose#0 == matrix{{1_QQ,0,0}} * T^0)
assert( adec.decompose#1 == matrix{{1_QQ,1,0}} * T^2)
assert( adec.decompose#2 == matrix{{0,1_QQ,0}} * T^3)
ddec = d/cS3
assert( set keys ddec.decompose == set {0})
assert( ddec.decompose#0 == matrix{{1_QQ,0,0}} * T^0
    + matrix{{1_QQ,1,0}} * (T^1+T^2+T^3))
///

-- Test 1 (non-monomial ideal, symmetric group)
TEST ///
clearAll
R = QQ[x_1..x_5]
I = ideal(
    	(x_1-x_4)*(x_2-x_5),
    	(x_1-x_3)*(x_2-x_5),
    	(x_1-x_3)*(x_2-x_4),
    	(x_1-x_2)*(x_3-x_5),
    	(x_1-x_2)*(x_3-x_4)	
    )
RI = res I
S5 = for p in partitions(5) list (
    L := gens R;
    g := for u in p list (
	l := take(L,u);
	L = drop(L,u);
	rotate(1,l)
	);
    matrix { flatten g }
    )
assert(S5 == symmetricGroupActors(R))
A = action(RI,S5)
a = character(R,hashTable {
    ((0,{0}), matrix{{1,1,1,1,1,1,1}}),
    ((1,{2}), matrix{{0,-1,1,-1,1,1,5}}),
    ((2,{3}), matrix{{0,1,-1,-1,1,-1,5}}),
    ((3,{5}), matrix{{1,-1,-1,1,1,-1,1}})
    })
assert((character A) == a)
B = action(R,S5)
b = character(R,hashTable {
    ((0,{0}), matrix{{1,1,1,1,1,1,1}}),
    ((0,{1}), matrix{{0,1,0,2,1,3,5}}),
    ((0,{2}), matrix{{0,1,1,3,3,7,15}}),
    ((0,{3}), matrix{{0,1,1,5,3,13,35}})
    })
assert(character(B,0,3) == b)
C = action(I,S5)
c = character(R,hashTable {
    ((0,{2}), matrix{{0,-1,1,-1,1,1,5}}),
    ((0,{3}), matrix{{0,-2,1,-1,0,4,20}})
    })
assert(character(C,0,3) == c)
D = action(R/I,S5)
d = character(R,hashTable {
    ((0,{0}), matrix{{1,1,1,1,1,1,1}}),
    ((0,{1}), matrix{{0,1,0,2,1,3,5}}),
    ((0,{2}), matrix{{0,2,0,4,2,6,10}}),
    ((0,{3}), matrix{{0,3,0,6,3,9,15}})
    })
assert(character(D,0,3) == d)
assert(b == c++d)
cS5 = symmetricGroupTable(5,QQ)
assert( cS5.table ==
    matrix{{1_QQ,1,1,1,1,1,1},
	{-1,0,-1,1,0,2,4},
	{0,-1,1,-1,1,1,5},
	{1,0,0,0,-2,0,6},
	{0,1,-1,-1,1,-1,5},
	{-1,0,1,1,0,-2,4},
	{1,-1,-1,1,1,-1,1}}
    )
adec = a/cS5
assert( set keys adec.decompose == set {0,1,2,3})
CR = a.degreesRing
T = CR_0
assert( adec.decompose#0 == matrix{{1_QQ,0,0,0,0,0,0}} * T^0)
assert( adec.decompose#1 == matrix{{0,0,1_QQ,0,0,0,0}} * T^2)
assert( adec.decompose#2 == matrix{{0,0,0,0,1_QQ,0,0}} * T^3)
assert( adec.decompose#3 == matrix{{0,0,0,0,0,0,1_QQ}} * T^5)
ddec = d/cS5
assert( set keys ddec.decompose == set {0})
assert( ddec.decompose#0 == matrix{{1_QQ,0,0,0,0,0,0}} * T^0
    + matrix{{1_QQ,1,0,0,0,0,0}} * T^1
    + matrix{{2_QQ,2,0,0,0,0,0}} * T^2
    + matrix{{3_QQ,3,0,0,0,0,0}} * T^3)
///

-- Test 2 (non symmetric group, tests actors)
TEST ///
clearAll
kk = toField(QQ[w]/ideal(sum apply(5,i->w^i)))
R = kk[x,y]
D5 = {
    matrix{{x,y}},
    matrix{{w*x,w^4*y}},
    matrix{{w^2*x,w^3*y}},
    matrix{{y,x}}
    }
A = action(R,D5)
a = {
    map(R^{4:-3},R^{4:-3},{{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}),
    map(R^{4:-3},R^{4:-3},{{w^3,0,0,0},{0,w,0,0},{0,0,w^4,0},{0,0,0,w^2}}),
    map(R^{4:-3},R^{4:-3},{{w,0,0,0},{0,w^2,0,0},{0,0,w^3,0},{0,0,0,w^4}}),
    map(R^{4:-3},R^{4:-3},{{0,0,0,1},{0,0,1,0},{0,1,0,0},{1,0,0,0}})
    }
assert(actors(A,3) == a)
ca = character(R, hashTable {((0,{3}), lift(matrix{apply(a,trace)},kk))})
assert(character(A,3) == ca)
d1=map(R^1,R^{4:-3},{{x^3,x^2*y,x*y^2,y^3}})
d2=map(R^{4:-3},R^{3:-4},{{-y,0,0},{x,-y,0},{0,x,-y},{0,0,x}})
Rm=complex({d1,d2})
B = action(Rm,D5)
assert(actors(B,1) == a)
cb1 = character(R, hashTable {((1,{3}), lift(matrix{apply(a,trace)},kk))})
assert(character(B,1) == cb1)
b = {
    map(R^{3:-4},R^{3:-4},{{1,0,0},{0,1,0},{0,0,1}}),
    map(R^{3:-4},R^{3:-4},{{w^2,0,0},{0,1,0},{0,0,w^3}}),
    map(R^{3:-4},R^{3:-4},{{w^4,0,0},{0,1,0},{0,0,w}}),
    map(R^{3:-4},R^{3:-4},{{0,0,-1},{0,-1,0},{-1,0,0}})
    }
assert(actors(B,2) == b)
cb2 = character(R, hashTable {((2,{4}), lift(matrix{apply(b,trace)},kk))})
assert(character(B,2) == cb2)
///

-- Test 3 (multigraded ideal, product of symmetric groups)
TEST ///
clearAll
R = QQ[x_1,x_2,y_1,y_2,Degrees=>{2:{1,0},2:{0,1}}]
I = intersect(ideal(x_1,x_2),ideal(y_1,y_2))
RI = res I
G = {
    matrix{{x_2,x_1,y_2,y_1}},
    matrix{{x_2,x_1,y_1,y_2}},
    matrix{{x_1,x_2,y_2,y_1}},
    matrix{{x_1,x_2,y_1,y_2}}
    }
A = action(RI,G)
a = character(R,hashTable {
    ((0,{0,0}), matrix{{1,1,1,1}}),
    ((1,{1,1}), matrix{{0,0,0,4}}),
    ((2,{1,2}), matrix{{0,0,-2,2}}),
    ((2,{2,1}), matrix{{0,-2,0,2}}),
    ((3,{2,2}), matrix{{1,-1,-1,1}})
    })
assert((character A) == a)
B = action(R,G)
b = character(R,hashTable {
    ((0,{0,2}), matrix{{1,3,1,3}}),
    ((0,{2,0}), matrix{{1,1,3,3}})
    })
assert(character(B,{0,2}) ++ character(B,{2,0}) == b)
C = action(R/I,G)
assert(character(C,{0,2}) ++ character(C,{2,0}) == b)
///

-- Test 4 (dual and tensor, symmetric group)
TEST ///
clearAll
R = QQ[x_1..x_4]
K = res ideal vars R
S4 = symmetricGroupActors(R)
A = action(K,S4)
c = character A
signrep = character(R, hashTable { (-4,{-4}) => matrix{{-1,1,1,-1,1}} })
-- check duality of representations in Koszul complex
-- which is true up to a twist by a sign representation
assert(dual(c,id_QQ) == c ** signrep)
///

-- Test 5 (additive inverse, scalar multiplication, difference, degree selection)
TEST ///
clearAll
R = QQ[x,y,z]
I = (ideal vars R)^3
J = ideal(x^3,y^3,z^3)
S3 = symmetricGroupActors R
A1 = action(I,S3)
A2 = action(J,S3)
c1 = character(A1,0,10)
c2 = character(A2,0,10)
assert(-c1 == (-1)*c1)
assert(c1 ++ c1 == 2*c1)
c = character(R, hashTable {
	(0,{5}) => matrix{{0,1,3}},
	(0,{6}) => matrix{{1,1,1}}
	})
assert( (c1 - c2)^{{5},{6}} == c)
///

-- Test 6 (fine grading, semidirect product with symmetric group)
TEST ///
clearAll
R = QQ[x,y,z,Degrees=>{{1,0,0},{0,1,0},{0,0,1}}]
I = ideal(x*y,x*z,y*z)
RI = res I
S3 = symmetricGroupActors R
A1 = action(RI,S3,Semidirect=>{uniquePermutations,rsort})
c1 = character(A1)
d1 = character(R, hashTable {
	(0,{0,0,0}) => matrix{{1,1,1}},
	(1,{1,1,0}) => matrix{{0,1,1}},
	(1,{1,0,1}) => matrix{{0,0,1}},
	(1,{0,1,1}) => matrix{{0,0,1}},
	(2,{1,1,1}) => matrix{{-1,0,2}}
	},
    Semidirect=>{uniquePermutations,rsort})
assert( c1 === d1)
A2 = action(R,S3,Semidirect=>{uniquePermutations,rsort})
c2 = character(A2,{0,3,0}) ++ character(A2,{1,0,2}) ++ character(A2,{1,1,1})
T = c2.degreesRing
d2 = map(T^{0},T^{0,0,0},matrix {{T_0*T_1*T_2, T_0*T_1*T_2+T_2^3,
      T_0^3+T_0^2*T_1+T_0^2*T_2+T_0*T_1^2+T_0*T_1*T_2+T_0*T_2^2+T_1^3+T_1^2*T_2+T_1*T_2^2+T_2^3}})
assert( c2.characters#0 == d2)
///

end
