QQ[x,y_1 .. y_2]
f = (x+y_1+y_2+1)^2
value expression f == f

assert( net BinaryOperation {symbol /, a, BinaryOperation{symbol /,b,c}} == "a/(b/c)" )
assert( net BinaryOperation {symbol \, a, BinaryOperation{symbol \,b,c}} == "a\\b\\c" )
assert( net BinaryOperation {symbol /, BinaryOperation{symbol /,b,c},d} == "b/c/d" )
assert( net BinaryOperation {symbol \, BinaryOperation{symbol \,b,c},d} == "(b\\c)\\d" )

assert( toString BinaryOperation {symbol /, a, BinaryOperation{symbol /,b,c}} == "a/(b/c)" )
assert( toString BinaryOperation {symbol \, a, BinaryOperation{symbol \,b,c}} == "a\\b\\c" )
assert( toString BinaryOperation {symbol /, BinaryOperation{symbol /,b,c},d} == "b/c/d" )
assert( toString BinaryOperation {symbol \, BinaryOperation{symbol \,b,c},d} == "(b\\c)\\d" )

-- added 2018

R=QQ[u]
assert(value expression u == u)
assert(value expression R^0 == R^0)
assert(toString (frac(QQ[v]))^2 == "(frac(QQ[v]))^2")
assert(toString ((expression true) and (expression false)) == "true and false")

-- MatrixExpression
importFrom(Core, "short")
assert(value short (f = map((ZZ/101)^3, (ZZ/101)^5, 0)) == f)
assert(value expression (f = matrix(ZZ/101, {})) == f)
assert(value expression (f = mutableMatrix(ZZ/101, {})) == f)

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR\Macaulay2\test expressions.out"
-- End:
