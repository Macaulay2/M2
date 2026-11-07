--- status: DRAFT
--- author(s): MES
--- notes: 

document { 
     Key => {
	  partition,
	 (partition,Function,VisibleList),
	 (partition,Function,VisibleList,VisibleList),
	 (partition,Function,VirtualTally),
	 (partition,Function,VirtualTally,VisibleList)},
     Headline => "partition a set or list by values of a function",
     Usage => "partition(f,L)\npartition(f,L,I)",
     Inputs => {
	  "f" => Function,
	  "L" => {ofClass VisibleList, ", or ", ofClass VirtualTally}
	  },
     Outputs => {
	  HashTable => {"whose keys are the possible values of the function ", TT "f", 
	       " on the elements of the list, set or tally ", TT "L", 
	       ", and the corresponding value of the hash table is the list, set or 
	       tally of elements of ", TT "L", " which take that value under ", TT "f", ".
	       The optional third argument ", TT "I", " is a list whose elements will
	       be included as keys in the return value, allowing empty lists to appear
	       in the partition."}
	  },
     EXAMPLE lines ///
     	  L = {1,3,6,5,3,1,2,8,8,8}
	  partition(odd, L)
	  partition(odd, set L)
	  partition(odd, tally L)
	  partition (even, {3,3,5},{true,false})
	  ///,
     "The following example partitions the generators of an ideal by degree.",
     EXAMPLE lines ///
          R = QQ[a..f]
	  I = ideal"ab,ade,ac3,d4,b3,adf,f4,e10"
	  partition(f -> first degree f, flatten entries gens I)
     	  ///,
     SeeAlso => {tally, partitions, sublists}
     }

doc ///
Key
    (partition,Function,VisibleList,VisibleList,Function)
    (partition,Function,VisibleList,Function)
Headline
    paritition elements by values of a function and apply a function
Usage
    partition(f, xs, ks, g)
    partition(f, xs, g)
Inputs
    f:Function
    xs:VisibleList
    ks:VisibleList
    g:Function
Outputs
    :HashTable
Description
    Text
        Given a list of data, group by one function and apply another one.
    Example
        someprimes = for i from 1 to 10 list nextPrime(100 * i)
        netList pairs partition(p -> legendreSymbol(-1, p), someprimes, p -> p %4)
    Text
        If you have already assembled a table of data this pattern can be
        conveniently used to group by one column while focusing in on another.
    Example
        someprimes = for i from 1 to 10 list nextPrime(100 * i);
        data = apply(someprimes, p -> {p, legendreSymbol(-1, p), p % 4});
        netList data
        netList pairs partition(row -> row_1, data, row -> row_2)
    Text
        Omit the second function and `identity` will be used implicitly.
    Example
        xs = {"apples", "bananas", "avocados", "grapes", "grapefruit"};
        netList pairs partition(x -> x_0, xs)
    Text
        As in use of partition without applying a function to the values, you
        can provide a VisibleList of key-values to ensure appear in the result -
        possibly with an empty container of values.
    Example
        someprimes = for i from 1 to 10 list nextPrime(100 * i);
        data = apply(someprimes, p -> {p, legendreSymbol(-1, p), p % 4});
        netList data
        netList pairs partition(row -> row_1, data, 0..3, row -> row_2)
///
