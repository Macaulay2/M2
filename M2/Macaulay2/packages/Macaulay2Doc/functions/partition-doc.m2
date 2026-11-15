--- status: DRAFT
--- author(s): MES
--- notes:

doc ///
Key
    partition
    (partition,VisibleList,Function,VisibleList,Function)
    (partition,VisibleList,Function)
    (partition,VisibleList,Function,VisibleList)
    (partition,VisibleList,Function,Function)
    (partition,Function,VisibleList,VisibleList,Function)
    (partition,Function,VisibleList)
    (partition,Function,VisibleList,VisibleList)
    (partition,Function,VisibleList,Function)
    (partition,VirtualTally,Function,VisibleList,Function)
    (partition,VirtualTally,Function)
    (partition,VirtualTally,Function,VisibleList)
    (partition,VirtualTally,Function,Function)
    (partition,Function,VirtualTally,VisibleList,Function)
    (partition,Function,VirtualTally)
    (partition,Function,VirtualTally,VisibleList)
    (partition,Function,VirtualTally,Function)
Headline
    partition a set or list by values of a function
Usage
    partition(L,f,I,g)
    partition(L,f)
    partition(L,f,I)
    partition(L,f,g)
    partition(f,L,I,g)
    partition(f,L)
    partition(f,L,I)
    partition(f,L,g)
Inputs
    f:Function
    L:{VisibleList,VirtualTally}
    I:VisibleList
    g:Function
Outputs
    :HashTable
Description
    Text
        Output is a @TO HashTable@ whose keys are the possible values of the function @TT "f"@
        on the elements of the list, set or tally @TT "L"@
        and the corresponding value of the hash table is the list, set or
        tally of elements of @TT "L"@ which take that value under @TT "f"@.
        The optional third argument @TT "I"@ is a list whose elements will
        be included as keys in the return value, allowing empty lists to appear
        in the partition.
    Example
        L = {1,3,6,5,3,1,2,8,8,8}
        partition(L, odd)
        partition(set L, odd)
        partition(tally L, odd)
        partition ({3,3,5},even, {true,false})
    Text
        It is allowed to swap the order of the first two arguments.
    Example
        L = {1,3,6,5,3,1,2,8,8,8}
        partition(odd, L)
        partition(odd, set L)
        partition(odd, tally L)
        partition (even, {3,3,5},{true,false})
    Text
        The following example partitions the generators of an ideal by degree.
    Example
        R = QQ[a..f]
        I = ideal"ab,ade,ac3,d4,b3,adf,f4,e10"
        partition(flatten entries gens I, f -> first degree f)
    Text
        When the second function argument is provided, it will be applied to the
        grouped values after the partition is made.
    Example
        partition(0..10, odd, x -> x^2)
    Text
        This example demonstrates the fact that $x^2 + 1 \equiv 0 \pmod{p}$ is
        solvable iff $p \equiv 1 \pmod{4}$.
    Example
        -- -1 is a square mod p iff p % 4 == 1
        someprimes = for i from 1 to 10 list nextPrime(100 * i)
        netList pairs partition(someprimes, p -> legendreSymbol(-1, p), p -> p %4)
    Text
        If you have already assembled a table of data this pattern can be
        conveniently used to group by one column while focusing in on another.
    Example
        someprimes = for i from 1 to 10 list nextPrime(100 * i);
        data = apply(someprimes, p -> {p, legendreSymbol(-1, p), p % 4});
        netList data
        netList pairs partition(data, row -> row_1, row -> row_2)
    Text
        As in use of partition without applying a function to the values, you
        can provide a List of key-values to ensure appear in the result -
        possibly with an empty container of values.
    Example
        someprimes = for i from 1 to 10 list nextPrime(100 * i);
        data = apply(someprimes, p -> {p, legendreSymbol(-1, p), p % 4});
        netList data
        netList pairs partition(data, row -> row_2, {0, 1, 2, 3}, row -> row_1)
    Text
        When applying a function to map the values of the partition of a
        VirtualTally, the function is applied to the keys of the partitioned
        tallies and any key collisions are combined.
    Example
        t = tally {"apple", "apricot", "apple", "avocado", "beet", "banana", "berry"}
        partition(t, x -> x_0)
        partition(t, x -> x_0, x -> x_{0..1})
        partition(t, x -> x_{0..1}, x -> x_0) -- tallies from different groups are not combined
SeeAlso
    tally
    partitions
    sublists
///
