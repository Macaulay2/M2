loadPackage( "Bertini",Reload=>true)

makeB'InputFile(storeBM2Files,AffVariableGroup=>{x,y},B'Polynomials=>{"x^2-2","y^2+x+1"})
runBertini(storeBM2Files)

raws=importSolutionsFile(storeBM2Files,NameSolutionsFile=>"raw_solutions");--this is the same as the default
ns=importSolutionsFile(storeBM2Files,NameSolutionsFile=>"nonsingular_solutions")
reals=importSolutionsFile(storeBM2Files,NameSolutionsFile=>"real_finite_solutions")
assert(#raws===4)
assert( abs( abs(raws_0_0)-sqrt 2)<1e-10)

assert(#ns===4)
assert( abs( abs(ns_0_0)-sqrt 2)<1e-10)

assert(#reals===2)
assert( abs( abs(reals_0_0)-sqrt 2)<1e-10)


raws2=importSolutionsFile(storeBM2Files,NameSolutionsFile=>"raw_solutions",
    OrderPaths=>true);
assert(#raws2===4)
assert( abs( abs(raws2_0_0)-sqrt 2)<1e-10)

--Singular solutions can be imported.
makeB'InputFile(storeBM2Files,AffVariableGroup=>{x},B'Polynomials=>{"(x^2-2)*x^3"});
runBertini(storeBM2Files);


raws=importSolutionsFile(storeBM2Files,NameSolutionsFile=>"raw_solutions")--this is the same as the default
ns=importSolutionsFile(storeBM2Files,NameSolutionsFile=>"nonsingular_solutions")
reals=importSolutionsFile(storeBM2Files,NameSolutionsFile=>"real_finite_solutions")
sings=importSolutionsFile(storeBM2Files,NameSolutionsFile=>"singular_solutions")


assert(#raws===5)---there are 5 raw solutions because the polynomial is degree 5. 

assert(#ns===2)
assert( abs( abs(ns_0_0)-sqrt 2)<1e-10)

assert(#reals===5)

assert(#sings===3)
assert( abs( abs(sings_0_0)-0)<1e-10)



----We want to make sure we have lots of precision. 
makeB'InputFile(storeBM2Files,AffVariableGroup=>{x},B'Polynomials=>{"(x^2-2)"},
    BertiniInputConfiguration=>{FinalTol=>1e-300})
runBertini(storeBM2Files)
printingPrecision =256 
rawSolutions=importSolutionsFile(storeBM2Files,
    NameSolutionsFile=>"raw_solutions",
    M2Precision=>256)
assert((#(toString rawSolutions_0_0)>50)===true)
assert((#toExternalString rawSolutions_0>100)===true)

