--------------------------------
--- PDState commands -----------
--------------------------------
PDState = new Type of MutableHashTable
createPDState = method()
createPDState Ideal := I -> (
    new PDState from {
	"OriginalIdeal"     => I,
	"PrimesSoFar"       => new MutableHashTable from {},
	"IntersectionSoFar" => ideal (1_(ring I)),
	"isPrimeIdeal"      => true,
	"isPrimaryIdeal"    => true,
	"PrunedViaCodim"    => 0
	}
    )

updatePDState = method()
updatePDState (PDState,List,ZZ) := (pdState,L,pruned) -> (
  -- this function updates the pdState with the new primes in the list L which
  -- consists of annotated ideals, all of which are known to be prime
  ansSoFar := pdState#"PrimesSoFar";
  pdState#"PrunedViaCodim" = pdState#"PrunedViaCodim" + pruned;
  for p in L do (
     I := ideal p;
     --pdState#"IntersectionSoFar" = trim intersect(pdState#"IntersectionSoFar", I);
     c := codim p;
     --<< endl << "   Adding codimension " << c << " prime ideal." << endl;
     if not ansSoFar#?c then
        ansSoFar#c = {(p,I)}
     else
        ansSoFar#c = append(ansSoFar#c,(p,I));
  );
  -*
  -- here we update the isPrime flag if L comes in with more than one
  -- prime, then the ideal is neither prime nor primary.
  -- the reason for this is that no single step will produce multiple redundant
  -- primes.  The only possible redundancy occurs when a prime is
  -- already in pdState and also comes into the list L
  if #L > 1 then (
     pdState#"isPrimeIdeal" = false;
     pdState#"isPrimaryIdeal" = false;
  );
  *-
)

numPrimesInPDState = method()
numPrimesInPDState PDState := pdState -> sum apply(pairs (pdState#"PrimesSoFar"), p -> #(p#1))

getPrimesInPDState = method()
getPrimesInPDState PDState := pdState ->
   flatten apply(pairs (pdState#"PrimesSoFar"), p -> (p#1) / last)

isRedundantIdeal = method()
isRedundantIdeal (AnnotatedIdeal,PDState) := (I,pdState) -> (
   -- the reason for this line is that once IndependentSet has been called, then
   -- I.Ideal no longer reflects the ideal
   if I.?IndependentSet then return false;
   primeList := getPrimesInPDState(pdState);
   -- as of now, all ideals are declared not redundant
   false
   -- this commented line takes too long!
   --any(primeList,p -> isSubset(p,I))
   )

flagPrimality = method()
flagPrimality(PDState, Boolean) := (pdState, primality) -> (pdState#"isPrime" = primality;)
