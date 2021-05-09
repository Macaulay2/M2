needsPackage "Bertini"



--------
--Bertini keeps much more information that just the coordinates of solutions. 
--Bertini determines witness points, and these points have a lot of numerical infomration. 
--Witness points are stored in the main_data file.  The format of the main_data file depends on if you are using regeneration or positive dimensional solves. 

makeB'InputFile(storeBM2Files,AffVariableGroup=>{x,y},B'Polynomials=>{"(x^2-2)*x^3","y"})
runBertini(storeBM2Files)

witnessPoints=importMainDataFile(storeBM2Files)
--Since there are 3 distinct witness 
assert(#witnessPoints==3)
w1=witnessPoints_0
w2=witnessPoints_1
w3=witnessPoints_2

for w in witnessPoints do(
if w#Multiplicity>1 then (
  assert(w#Multiplicity===3);
  assert(abs(w#Coordinates_0)<1e-6);
  assert(abs(w#Coordinates_1)<1e-6);
      )  )


----------------------
--Bertini has a USEREGENERATION option. When Bertini uses regenereation singular solutions are ignored in zero dimensional runs. 
makeB'InputFile(storeBM2Files,AffVariableGroup=>{x,y},B'Polynomials=>{"(x^2-2)*x^3","y"},
    BertiniInputConfiguration=>{UseRegeneration=>1})
runBertini(storeBM2Files)

witnessPoints=importMainDataFile(storeBM2Files)
assert(#witnessPoints==2)

------
---Bertini has a positive dimensional solve option. When this option is envoked Bertini will not produce files that have coordinates of witness points, or in otherwords we cannot use the importSolutionsFile command to get information from a positive dimensional run.
--For positive dimensional runs we must use the importMainDataFile command. 
--Positive dimensional runs automatically use regeneration. Unlike the Zero-dimensional regeneration solver singular solutions are classified in the pos dim case.


makeB'InputFile(storeBM2Files,AffVariableGroup=>{x,y},B'Polynomials=>{"(x^2-2)*x^3","y"},
    BertiniInputConfiguration=>{TrackType=>1})
runBertini(storeBM2Files)

witnessPoints=importMainDataFile(storeBM2Files)
assert(#witnessPoints==3)
p1=witnessPoints_0
p2=witnessPoints_1
p3=witnessPoints_2
keys p1
#keys p1
#keys w1


---One must take care with component numbers when importing main data from a positive dimensional run. 
---Components are indexed by dimension then component number. Indexing also starts at 0. So for a non equidimensional variety there will be multiple component 0's. 

makeB'InputFile(storeBM2Files,
    AffVariableGroup=>{x,y},
    B'Polynomials=>{"x*(x-1)*(x-2)^2","x*(y-3)"},
    BertiniInputConfiguration=>{TrackType=>1});

runBertini(storeBM2Files);
wp=importMainDataFile(storeBM2Files) ;


assert(( sort for i in wp list {i#Dimension,i#ComponentNumber})==={{0, 0}, {0, 1}, {1, 0}})





































-----




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
