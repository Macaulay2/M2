CpMackeyFunctor = new Type of HashTable
CpMackeyFunctor.synonym = "C_p Mackey Functor"

-- Check if a Mackey functor is well-defined
-- This overloads the isWellDefined method to apply to CpMackeyFunctors
isWellDefined CpMackeyFunctor := Boolean => M -> (
    ------------------------
    -- General type-checking
    ------------------------

    -- Ensure all the keys in the hash table defining a Mackey functor are indeed defined
    if not (M#?PrimeOrder and M#?Res and M#?Trans and M#?Conj and M#?Underlying and M#?Fixed) then (print "-- Hashtable does not have correct keys"; return false);

    -- Check that the input p is actually a prime number
    if not (class M.PrimeOrder === ZZ and isPrime(M.PrimeOrder)) then (print "-- p is not prime"; return false);

    -- Check fixed and underlying modules are Z-modules
    if not (isModule M.Fixed and isModule M.Underlying) then return false;
    if not (ring M.Fixed === ZZ and ring M.Underlying === ZZ) then (print "-- objects are not abelian groups"; return false);

    -- Check source and target of restriction are correct
    if not source(M.Res) == M.Fixed then (print " -- the source of res should be the fixed module"; return false);
    if not target(M.Res) == M.Underlying then (print " -- the target of res should be the underlying module"; return false);

    -- Check source and target of transfer are correct
    if not source(M.Trans) == M.Underlying then (print " -- the source of tr should be the underlying module"; return false);
    if not target(M.Trans) == M.Fixed then (print " -- the target of tr should be the fixed module"; return false);

    -- Check source and target of conjugation are correct
    if not source(M.Conj) == M.Underlying then (print " -- the source of conj should be the underlying module"; return false);
    if not target(M.Conj) == M.Underlying then (print " -- the target of conj should be the underlying module"; return false);

    ---------
    -- Axioms
    ---------

    -- Axiom 1: Conj is an automorphism of order dividing p
    if not isIsomorphism(M.Conj) then return false;
    if not (M.Conj)^(M.PrimeOrder) == id_(M.Underlying) then (print "-- Conj is not an automorphism of order dividing p"; return false);

    -- Axiom 2: res and tr are homomorphisms (item 3 in overleaf)
    if not isWellDefined M.Trans then (print "-- tr is not a homomorphism"; return false);
    if not isWellDefined M.Res then (print "-- res is not a homomorphism"; return false);

    -- Axiom 3: c*res = res and tr*c = tr
    if M.Conj * M.Res != M.Res then (print "-- c * res is not equal to res"; return false);
    if M.Trans * M.Conj != M.Trans then (print "-- tr * c is not equal to tr"; return false);

    -- Axiom 4: res * tr = sum of all conjugates (item 5 in overleaf)
    if not (M.Res * M.Trans == sum for i to M.PrimeOrder-1 list M.Conj^i) then (print "-- res * tr is not equal to the sum of all conjugates"; return false);

    true
)

makeCpMackeyFunctor = method()
-- Ordering for the input is:
-- 1. A prime number p
-- 2. Restriction matrix
-- 3. Transfer matrix
-- 4. Conjugation matrix
makeCpMackeyFunctor(ZZ,Matrix,Matrix,Matrix) := CpMackeyFunctor => (p,R,T,C) ->(
    M := new CpMackeyFunctor from {
        symbol PrimeOrder => p,
        symbol Underlying => source T,          -- extract the underlying module from the transfer homomorphism
        symbol Fixed => target T,               -- extract the fixed module from the transfer homomorphism
        symbol Res => R,
        symbol Trans => T,
        symbol Conj => C,
        symbol cache => new CacheTable
        };
    if assertLevel > 0 and not isWellDefined M then error "Mackey Functor is not well-defined";
    M
)

-- printing behavior
-- This is just some adhoc editing, sorry to anybody trying to decipher this!
-- But the basics are: "string | string" will adjoin things horizontally, and
-- "string || string" will adjoin things vertically. Everything done here is
-- gluing strings together with whitespace depending on the width/heights of
-- the modules/matrices here.
-- For those trying to understand, worth pointing out that the HEIGHT of a string
-- is obtained via "length" and NOT "height".
lineAbove := (s, n) -> concatenate(n : "-") || s
lineBelow := (s, n) -> s || concatenate(n : "-")
vertSpace := n -> (s := ""; if n == 1 then return "" else for i to n-2 do s = s || ""; s)
horSpace := n -> (s := " "; if n == 1 then return s else for i to n-2 do s = s | " "; s)
vertArrows := n -> (s := "^ |"; if n > 1 then for i to n-1 do s = s || "| |"; s || "| V")

net CpMackeyFunctor := M -> (
    n := 6 + max({M.Res,M.Trans}/net/width);
    h := if M.Res == 0 then 1 else numRows M.Res;
    horizontalJoin(
	vertSpace(h) || (net M.Fixed), vertSpace(h) || "  --" || " <--",
	lineBelow("Res : " | (net M.Res), n) || lineAbove( "Tr : " | (net M.Trans), n),
	vertSpace(h) || "--> " || "-- ", vertSpace(h) || (net M.Underlying),
	vertSpace(h) || "  -┐" || "  <┘", vertSpace(h) || (" Conj : " | (net M.Conj))
	)
    )

drawVerticalCpMackeyFunctor = method()
drawVerticalCpMackeyFunctor(CpMackeyFunctor) := Net => M -> (
    hMid := 1 + max {length net M.Res, length net M.Trans};
    hTop := length net M.Fixed + 1;
	firstCol := vertSpace(hTop) || " Res" || (net M.Res);
    w1 := width net M.Fixed;
    w2 := width net M.Underlying;
    w3 := width net M.Conj;
    wm := max{w1, w2, w3};
    w := round(wm / 2)-1;
    w1 = if w1 == wm then 0 else round((wm-w1)/2);
    w2 = if w2 == wm then 0 else round((wm-w2)/2);
    w3 = if w3 == wm then 0 else round((wm-w3)/2);
    secondCol := horizontalJoin(horSpace(w1),net M.Fixed) || horizontalJoin(horSpace(w),vertArrows(hMid)) || horizontalJoin(horSpace(w2),net M.Underlying) || horizontalJoin(horSpace(w), "  ^") || horizontalJoin(horSpace(w), "└-┘") || horizontalJoin(horSpace(w), " c") || horizontalJoin(horSpace(w3),net M.Conj);
    thirdCol := vertSpace(hTop) || " Tr" || (net M.Trans);
    return horizontalJoin(firstCol, " ", secondCol, " ", thirdCol)
)


-- Equality
CpMackeyFunctor == CpMackeyFunctor := Boolean => (M,N) -> (
    M.PrimeOrder == N.PrimeOrder and M.Underlying == N.Underlying and M.Fixed == N.Fixed and M.Res == N.Res and M.Trans == N.Trans and M.Conj == N.Conj
)

-- Pruning
prune CpMackeyFunctor := CpMackeyFunctor => opts -> M -> (
    T := prune M.Fixed;
    B := prune M.Underlying;
    fT := (T.cache.pruningMap);
    fB := (B.cache.pruningMap);
    r := map(B, T, (matrix inverse fB) * (matrix M.Res) * (matrix fT));
    t := map(T, B, (matrix inverse fT) * (matrix M.Trans) * (matrix fB));
    c := map(B, B, (matrix inverse fB) * (matrix M.Conj) * (matrix fB));
    M' := makeCpMackeyFunctor(M.PrimeOrder, r, t, c);
    M'.cache.pruningMap = map(M,M',fB,fT);
    M'
    )

-------------------------
-- Recovering cached data
-------------------------

isCohomological = method()
isCohomological CpMackeyFunctor := Boolean => M -> (
    if not M.cache#?"isCohomological" then (
        M.cache#"isCohomological" = (M.Trans * M.Res == map(M.Fixed, M.Fixed, M.PrimeOrder));
    );
    M.cache#"isCohomological"
)
