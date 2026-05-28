isLanguage = RFunction "is.language"
-- unary ~: integer input uses bitwNot
assert Equation(~ RObject 12, RObject(-13))
-- unary ~: non-integer/real input creates a one-sided formula
assert Equation(value isLanguage (~ RObject "x"), true)
-- binary ~: creates a two-sided formula
assert Equation(value isLanguage (RObject "y" ~ RObject "x"), true)
assert Equation(value isLanguage ("y" ~ RObject "x"), true)
assert Equation(value isLanguage (RObject "y" ~ "x"), true)
