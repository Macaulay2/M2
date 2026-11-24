-*
f = key -> select(value \ unique values Core.Dictionary, f -> instance(f, Function) and options f =!= null and (options f)#?key)
f Truncate
*-

nonnull := core "nonnull";
headline := core "headline";

opts := nonnull \\ options \ select(value \ values Core.Dictionary, f -> instance(f, Function));
opts  = nonnull \\ options \ methods() | opts; -- e.g. catch DegreeLimit from [(minimalBetti, Ideal), DegreeLimit]
opts  = unique opts;

optionalNames := select(unique \\ flatten \\ keys \ opts, s -> instance(s, Symbol) and isGlobalSymbol toString s and
    not isUndocumented(d := makeDocumentTag s) and (isMissingDoc d or headline d === "an optional argument"))
optionalNames = unique join({ Jacobian, Strict, Threads, Tries }, optionalNames)

optionalValues := unique(flatten \\ values \ opts
    | { Prune, Binomial, Test, Center, Right, Left, Flexible, Postfix, Maintainer, FindOne } -- for manually added Symbols
    | last \ last \ hooks methods()) -- e.g. catch Iterate from [(saturate, Ideal, Ideal), Strategy -> Iterate]

optionalValues = select(optionalValues, s -> instance(s, Symbol) and isGlobalSymbol toString s and
    not isUndocumented(d := makeDocumentTag s) and (isMissingDoc d or headline d === "value of an optional argument"))

-- Create generic documentation nodes for undocumented optional arguments and values
-- (optionalSymbol and optionalSymbols are global variables, so that SimpleDoc can see them)
scan(optionalNames, o -> (
	optionalSymbol = o;
	doc ///
  Key
    optionalSymbol
  Headline
    -- if the headline is changed, change it above as well
    an optional argument
  Description
    Text
      A symbol used as the name of an optional argument.
///))

both := set optionalNames * set optionalValues
scan(optionalValues - both, v -> (
	optionalSymbol = v;
	doc ///
  Key
    optionalSymbol
  Headline
    -- if the headline is changed, change it above as well
    value of an optional argument
  Description
    Text
      A symbol used as the value of an optional argument or strategy, for some
      function(s).
///))

-- Add a few special cases with individual documentation
optionalNames  |= { }
optionalValues |= { MinimalGenerators }

-- Make sure orphan nodes above have a parent
optionalSymbols = (sort toList both, sort (optionalNames - both), sort (optionalValues - both))
doc ///
  Key
    "symbols used as the name or value of an optional argument"
  Subnodes
    :Symbols used as an option name or value
    @TO \ optionalSymbols#0@
    :Symbols used as an option name
    @TO \ optionalSymbols#1@
    :Symbols used as an option value
    @TO \ optionalSymbols#2@
///
erase \ {symbol optionalSymbol, symbol optionalSymbols}
