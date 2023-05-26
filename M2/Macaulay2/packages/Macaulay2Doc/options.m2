-*
f = key -> select(value \ unique values Core.Dictionary, f -> instance(f, Function) and options f =!= null and (options f)#?key)
f Truncate
*-

nonnull := core "nonnull";
headline := core "headline";
isMissingDoc := core "isMissingDoc";
isUndocumented := core "isUndocumented";

opts := nonnull \\ options \ select(value \ values Core.Dictionary, f -> instance(f, Function));
opts  = nonnull \\ options \ methods() | opts; -- e.g. catch DegreeLimit from [(minimalBetti, Ideal), DegreeLimit]
opts  = unique opts;

optionalNames := select(select(unique \\ flatten \\ keys \ opts, n -> instance(n, Symbol)), s ->
    not isUndocumented(d := makeDocumentTag s) and (isMissingDoc d or headline d === "an optional argument"))

optionalValues := toList unique({ Prune, Binomial, Test, Center, Right, Left } -- for manually added Symbols
    | last \ last \ hooks methods() -- e.g. catch Iterate from [(saturate, Ideal, Ideal), Strategy -> Iterate]
    | flatten \\ values \ opts)

optionalValues = select(optionalValues, s -> instance(s, Symbol) and isGlobalSymbol toString s and
    not isUndocumented(d := makeDocumentTag s) and (isMissingDoc d or headline d === "value of an optional argument"))

-- Create generic documentation nodes for undocumented optional arguments and values
scan(optionalNames,
    o -> document { Key => o, Headline => "an optional argument", -- if the headline is changed, change it above as well
	"A symbol used as the name of an optional argument." })

scan(optionalValues, v ->
    document { Key => v, Headline => "value of an optional argument", -- if the headline is changed, change it above as well
	"A symbol used as the value of an optional argument or strategy, for some function(s)." })

-- Make sure orphan nodes above have a parent
document { Key => "symbols used as the name or value of an optional argument",
    Subnodes => flatten {
	"Symbols used as an option name",  TO \ optionalNames,
	"Symbols used as an option value", TO \ optionalValues}}
