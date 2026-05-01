doc ///
  Key
    RValue
    (RValue, String)
    (RValue, Sequence)
    Environment
    [RValue, Environment]
  Headline
    evaluate R code
  Usage
    RValue s
  Inputs
    s:{String,Sequence} -- R code to evaluate
    Environment => {RObject, HashTable} -- R environment for evaluation
  Outputs
    :RObject -- result of evaluating @VAR "s"@
  Description
    Text
      @SAMP "RValue"@ parses and evaluates R code given as a string and returns
      the result as an @TO RObject@.
    Example
      RValue "choose(10, 3)"
    Text
      When given a @TO Sequence@, the elements are converted to strings with
      @TO toString@ and concatenated before evaluation.  This is useful for
      programmatically constructing R code.
    Example
      n = 5
      RValue("factorial(", n, ")")
    Text
      The @CODE "Environment"@ option specifies the R environment in which to
      evaluate the code.
    Example
      env = RObject hashTable {"n" => 10_ZZ, "k" => 3_ZZ}
      RValue("choose(n, k)", Environment => env)
    Text
      A Macaulay2 hash table specifying values of variables with the variable
      names given as strings may also be passed to the @CODE "Environment"@
      option.
    Example
      env2 = hashTable {"x" => 3}
      RValue("x + 5", Environment => env2)
  SeeAlso
    RContext
///
