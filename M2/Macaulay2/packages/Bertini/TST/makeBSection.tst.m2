needsPackage "Bertini"

--In Numerical Algebraic Geometry the 'slice' is very important. For multiprojective varieties we ant a way to create slices or sections in an easy manner.
--A section corresponds to a linear equation. 
--A slice corresponds to a set of linear equations. 

--The input makeB'Section is a a list. To each element of this list we will apply toString and use the string to create a linear system of equations. 
--We use strings to avoid the need to create a ring with these elements.
s=makeB'Section({x,y,z})
assert(class s===B'Section)
assert(#keys s==2)
--The default is to create 2 keys.  
----One of the keys is B'NumberCoefficients. This is a list of the coefficients of the inputted list.
----The second key is a string which when evaluated is a linear equation with random coefficients and monomials determined by the list.
--The coefficients can be randomly generated as the user sees fit. 
--keys s
randomRealCoefficientGenerator=()->random(RR)
sReal=makeB'Section({x,y,z},RandomCoefficientGenerator=>randomRealCoefficientGenerator)
assert(class sReal#B'NumberCoefficients_0===RR)

randomRationalCoefficientGenerator=()->random(QQ)
sRational=makeB'Section({x,y,z},RandomCoefficientGenerator=>randomRationalCoefficientGenerator)
assert(class sRational#B'NumberCoefficients_0===QQ)

randomIntegerCoefficientGenerator=()->random(-1,1)
sInteger=makeB'Section({x,y,z},RandomCoefficientGenerator=>randomIntegerCoefficientGenerator)
assert(class sInteger#B'NumberCoefficients_0===ZZ)

---These equations are homogeneous. 
--We can consider an affine system by appending a 1 to the inputted list.
affineSection=makeB'Section({x,y,z,1})

--If we set the ContainsPoint option then we will create a linear equation.
----The coefficients are of the (X_i-P_i) where X_i is an element of the usual variable list input and p_i are the coordinates of a point.
----The linear equation vanishes at this point. 
X={x,y,z}
P={1,2,3}
options makeB'Section
affineContainingPoint=makeB'Section({x,y,z},ContainsPoint=>P, B'NumberCoefficients=>{2,5,7})
r= affineContainingPoint#B'SectionString
assert(r=="(2)*(x-(1)*(1))+(5)*(y-(1)*(2))+(7)*(z-(1)*(3))")

--There are two things to notice about the linear equation r above. 
----First it is an affine equation. Second each coordinate P_i is preceded by (1).
--Because Numerical algebraic geometry works on affine charts we want control over how we homogenize. 
--We can homogenize r so that it still contains the point P by setting the hyperplane at infinity, i.e. setting the homogenization. 

rHomogeSection= makeB'Section({x,y,z},ContainsPoint=>P,B'Homogenization=>"x+y+z",B'NumberCoefficients=>{2,5,7})
peek rHomogeSection
assert(rHomogeSection#B'SectionString=="(2)*(x-(x+y+z)*(1))+(5)*(y-(x+y+z)*(2))+(7)*(z-(x+y+z)*(3))")
-----
f="y^3-x*y+1"
s1=makeB'Section({x,y,1},B'NumberCoefficients=>{1.47364+1.1251*ii, .869195+.661049*ii, .115018-.813524*ii})
makeB'InputFile(storeBM2Files,
    AffVariableGroup=>{x,y},
    B'Polynomials=>{f,s1})
runBertini(storeBM2Files)
assert(#importSolutionsFile(storeBM2Files)==3)

------
--To use a section in the B'Function option we need to specify a string to name the section.

l=makeB'Section({x,y,z},NameB'Section=>"l",B'NumberCoefficients=>{.566814+1.5942*ii, -.524471+.609431*ii, .364689+.0869259*ii})
f="S^3+x^3+y^3+z^3-l^3"
s1=makeB'Section({x,y,1},B'NumberCoefficients=>{1.47364+1.1251*ii, .869195+.661049*ii, .115018-.813524*ii})
s2=makeB'Section({x,y,1},B'NumberCoefficients=>{ -.607416+.747061*ii, .939866-.205217*ii, -.814605-.233723*ii})
makeB'InputFile(storeBM2Files,
    B'Functions=>{l,{S,"x+y+z"}},
    AffVariableGroup=>{x,y,z},
    B'Polynomials=>{s1,s2,f})
readFile(storeBM2Files,"input",1000)
runBertini(storeBM2Files)
assert(#importSolutionsFile(storeBM2Files)==3)





