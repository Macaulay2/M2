TEST /// --a simple monomial test
R = ZZ/5[x,y,z];
compatIdeals = compatibleIdeals(x^4*y^4*z^4);
answerList =  {ideal(x,y,z), ideal(x,y), ideal(x,z), ideal(y,z), ideal(x), ideal(y), ideal(z)};
assert( all(compatIdeals, tt->any(answerList, ss -> tt==ss)));
assert(  all(answerList, tt->any(compatIdeals, ss -> tt==ss)) );
///

TEST /// 
R=ZZ/2[x_{21},x_{31},x_{32},x_{41},x_{42},x_{43}];
u=x_{41}*(x_{31}*x_{42}-x_{41}*x_{32})*(x_{41}-x_{21}*x_{42}-x_{31}*x_{43}+x_{21}*x_{32}*x_{43});
time CompatibleIdeals=compatibleIdeals(u);
answer=  {
    ideal(x_{21}*x_{32}*x_{43}+x_{21}*x_{42}+x_{31}*x_{43}+x_{41}),    
    ideal(x_{32}*x_{41}+x_{31}*x_{42},x_{31}*x_{43}+x_{41},x_{32}*x_{43}+x_{42}),
    ideal(x_{32}*x_{41}+x_{31}*x_{42},x_{31}*x_{43}+x_{41},x_{32}*x_{43}+x_{42},x_{21}*x_{42}+x_{41},x_{21}*x_{32}+x_{31}),
    ideal(x_{31},x_{21},x_{41},x_{32}*x_{43}+x_{42}),
    ideal(x_{42},x_{41},x_{31},x_{21},x_{43}),
    ideal(x_{43},x_{42},x_{41},x_{32},x_{31},x_{21}),
    ideal(x_{42},x_{41},x_{32},x_{31},x_{21}),
    ideal(x_{42},x_{43},x_{41},x_{21}*x_{32}+x_{31}),
    ideal(x_{43},x_{42},x_{41},x_{31},x_{32}), 
    ideal(x_{42},x_{31},x_{32},x_{41}),
    ideal(x_{31},x_{41},x_{32}*x_{43}+x_{42}), 
    ideal(x_{41},x_{31},x_{42},x_{43}),
    ideal(x_{42},x_{41},x_{43}),
    ideal(x_{41},x_{21}*x_{32}*x_{43}+x_{21}*x_{42}+x_{31}*x_{43}),
    ideal(x_{41},x_{42},x_{21}*x_{32}+x_{31}), 
    ideal(x_{42},x_{41},x_{31},x_{21}),
    ideal(x_{41},x_{31},x_{21}),
    ideal(x_{21}*x_{32}+x_{31},x_{32}*x_{41}+x_{31}*x_{42},x_{21}*x_{42}+x_{41}),
    ideal x_{41}, 
    ideal(x_{41},x_{42}), 
    ideal(x_{42},x_{41},x_{31}),
    ideal(x_{41},x_{31}), 
    ideal(x_{32}*x_{41}+x_{31}*x_{42})
}
assert( all(CompatibleIdeals, tt->any(answer, ss -> tt==ss)));
assert( all(answer, tt->any(CompatibleIdeals, ss -> tt==ss)));
///

