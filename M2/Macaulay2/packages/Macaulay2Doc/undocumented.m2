descendants := core "descendants"
expressiontypes := prepend_Expression descendants Expression
-- These can still be documented, but by default they are undocumented
undocumented select(makeDocumentTag \ unique flatten apply(expressiontypes, methods),
    m -> package m === Macaulay2Doc and not isUndocumented m and isMissingDoc m)
