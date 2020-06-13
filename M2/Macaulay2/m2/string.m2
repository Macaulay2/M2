-----------------------------------------------------------------------------
-- String processing library
-----------------------------------------------------------------------------

-- previously in methods.m2
lastMatch = null
match(String, String) := List => X -> null =!= (lastMatch = regex X)
replace(String, String, String) := String => replaceStrings

-- previously in html0.m2
lower := "abcdefghijklmnopqrstuvwxyz"
upper := "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
tolower := new HashTable from apply(characters upper, characters lower, identity)
toupper := new HashTable from apply(characters lower, characters upper, identity)
toLower = s -> concatenate apply(characters s, c -> if tolower#?c then tolower#c else c)
toUpper = s -> concatenate apply(characters s, c -> if toupper#?c then toupper#c else c)
