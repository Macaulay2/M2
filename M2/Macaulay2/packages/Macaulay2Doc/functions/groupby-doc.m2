doc ///
Key
    groupby
    groupby(BasicList, Function)
    groupby(BasicList, Function, Function)
Headline
    group elements in a list by their values under a function
Usage
    groupby(xs, f, g)
Inputs
    xs:BasicList
    f:Function
    g:Function
Outputs
    :MutableHashTable
        A hash table whose keys are obtained from $xs$ by applying $f$ and where
        the value for the key $f(x_0)$ is the list $[g(x) \mid x \in xs, f(x) =
        f(x_0)]$.
Description
    Text
        Given a list of data, group by one function and apply another one.
    Example
        someprimes = for i from 1 to 20 list nextPrime(100 * i)
        netList pairs groupby(someprimes, p -> legendreSymbol(-1, p), p -> p %4)
    Text
        If you have already assembled a table of data this pattern can be used to filter and focus in on particular columns. 
    Example
        someprimes = for i from 1 to 20 list nextPrime(100 * i);
        data = apply(someprimes, p -> {p, legendreSymbol(-1, p), p % 4})
        netList pairs groupby(data, row -> row_1, row -> row_2)
    Text
        Omit the second function and `identity` will be used implicitly.
    Example
        xs = {"apples", "bananas", "avocados", "grapes", "grapefruit"};
        netList pairs groupby(xs, x -> x_0)
///