----------------------------------------------
--INTERFACE METHODS FOR THE NCAlgebra PACKAGE
--Including net methods and various notations for   
--NCRingElements
----------------------------------------------

--coefficientHTable returns a Hash table associating the monomials in a nc polynomial to their coefficients
--this is not the same as f.terms, which associated the NCMonomials (an inaccessible type) in f to their coefficients
coefficientHTable = method()
coefficientHTable (NCRingElement) := HashTable => f -> (
        fterms := terms f;
        hashTable(apply(fterms, i -> {leadMonomial i, leadCoefficient i}))
);


--ncMonToList converts a NC monomial to a list representing the corresponding word
--f is a monomial in an NCring
--The output is a list representing the word
ncMonToList = method()
ncMonToList (NCRingElement) := List => f -> (
    fmons := keys f.terms;
    monKey := (keys fmons#0)#1;
    R := ring f;
    varst := hashTable(toList apply(0..length(gens R)-1, i-> (baseName R_i,i+1)));
    (fmons#0)#(monKey) / ( i -> varst#i)
);

--linExt extends functions on words to the whole non commutative polynomial algebra
--It calls fun on each word appearing in w and multiplies the result by the corresponding coefficient
linExt = method(Options=>{CoefficientRing => null});
linExt(FunctionClosure, NCRingElement) := RingElement => opts -> (fun, w) -> (
    lot := apply(terms w, i -> {leadCoefficient i, ncMonToList(i)});
    if (opts.CoefficientRing === null) then sum(length(lot),i->(lot#i)#0 * fun((lot#i)#1)) else 
            sum(length(lot),i->sub((lot#i)#0,opts.CoefficientRing) * fun((lot#i)#1))
)
-- methods for output of nc polynomials

-- the following method is an adaptation of the net function in the NCAlgebra package by Frank Moore, Andrew Conner and Courtney Gibbons.
wordFormat = method();
wordFormat NCRingElement := f -> (
   if #(f.terms) == 0 then return net "0";
   
   firstTerm := true;
   myNet := net "";
   isZp := (class coefficientRing ring f === QuotientRing and ambient coefficientRing ring f === ZZ);
   for t in sort pairs coefficientHTable f do (
      tempNet := (if(instance(t#1, Number)) then (if(t#1 < 0) then (if(firstTerm) then net "- " else net " - ") | net abs(t#1) else net t#1) else net t#1) | net " ";
      printParens := ring t#1 =!= QQ and
  		     ring t#1 =!= ZZ and
                     not isZp and
		     (size t#1 > 1 or (isField ring t#1 and 
			               numgens coefficientRing ring t#1 > 0 and
				       size sub(t#1, coefficientRing ring t#1) > 1));
      myNet = myNet |
              (if isZp and tempNet#0#0 != " - " and not firstTerm then net " + "
	       else if not firstTerm and t#1 > 0 then
                 net " + "
               else 
                 net "") |
              (if printParens then net "(" else net "") | 
              (if t#1 != 1 and t#1 != -1 then
                 tempNet
               else if t#1 == -1 then (if(firstTerm) then net "- " else net " - ")
               else net "") |
              (if printParens then net ") " else net "") |
              (if t#0 === {} and (t#1 == 1 or t#1 == -1) then net "1" else (net new Array from ncMonToList(t#0)));
      firstTerm = false;
   );
   myNet
)

wordString = method();
wordString NCRingElement := f -> (
   toString(wordFormat f)
)


applyDeep = method(); -- auxiliary function for tensorArray
applyDeep (Thing, FunctionClosure) := (l,f) -> (
    if(class l === List) then (
        l1 := apply(l,i->applyDeep(i,f));
        return(l1);
    );
    f(l)
)

-- converts a polynomial f in an nc ring R to a list of multi-dimensional arrays of depth 1,...,k, where k is the degree of f and the entry (j,i_1,...,i_j) is the coefficient of [i_1,...,i_j]_R in f.
tensorArray = method();
tensorArray NCRingElement := f -> (
    H := coefficientHTable f;
    R := ring f;
    genv := gens R;
    k := degree leadTerm f;
    toList apply(1..k,
        i -> applyDeep(product(i,k->genv),w->(if(H#?w) then H#w else 0)
        )
    )
)

-- returns only the depth h component of tensorArray
tensorArray(NCRingElement,ZZ) := (f,h) -> (
    H := coefficientHTable f;
    R := ring f;
    genv := gens R;
    applyDeep(product(h,k->genv),w->(if(H#?w) then H#w else 0))
)

NCRingElement @ ZZ := tensorArray

-- installs operator to work with words [i1,...,ik]_R in an NC ring R
Array _ NCPolynomialRing := (a, R) -> (
    if(max(toList a)>length(gens R)) then (error(toString(net "not enough letters in ring " | net R | ".")));
    
    product(a,i->R_(i-1))
)



