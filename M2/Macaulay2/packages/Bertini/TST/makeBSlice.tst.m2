needsPackage "Bertini"

--When we have a multiprojective variety we can different types of slices.
--To make a slice we need to specify the type of slice we want followed by variable groups.

sliceType={1,1}
variableGroups={{x0,x1},{y0,y1,y2}}
xySlice=makeB'Slice(sliceType,variableGroups,B'NumberCoefficients=>{{2,3},{5,7,11}})
--Our slice consists of two sections. 
--The ith section is in the variables variableGroups_(sliceType_i)
assert(xySlice#B'SectionString_0==="(2)*(x0)+(3)*(x1)")
assert(xySlice#B'SectionString_1==="(5)*(y0)+(7)*(y1)+(11)*(y2)")

----We can use slices to determine multidegrees.

f1="x0*y0+x1*y0+x2*y2"
f2="x0*y0^2+x1*y1*y2+x2*y0*y2"
variableGroups={{x0,x1,x2},{y0,y1,y2}}
xxSlice=makeB'Slice({2,0},variableGroups)

makeB'InputFile(storeBM2Files,
    HomVariableGroup=>variableGroups,
    B'Polynomials=>{f1,f2}|xxSlice#ListB'Sections)
runBertini(storeBM2Files)
xxDegree=#importSolutionsFile(storeBM2Files)
assert(xxDegree==2)

xySlice=makeB'Slice({1,1},variableGroups,ContainsMultiProjectivePoint=>{{1,2,3},{4,5,6}},B'Homogenization=>{hx,hy},B'NumberCoefficients=>{{20,21,22},{30,31,32}})
assert(xySlice#B'SectionString_0==="(20)*(x0-(hx)*(1))+(21)*(x1-(hx)*(2))+(22)*(x2-(hx)*(3))")
assert(xySlice#B'SectionString_1==="(30)*(y0-(hy)*(4))+(31)*(y1-(hy)*(5))+(32)*(y2-(hy)*(6))")

---Commented out below
"makeB'InputFile(storeBM2Files,
    HomVariableGroup=>variableGroups,
    B'Polynomials=>{f1,f2}|xySlice#ListB'Sections)
runBertini(storeBM2Files)
xyDegree=#importSolutionsFile(storeBM2Files)

makeB'InputFile(storeBM2Files,
    HomVariableGroup=>variableGroups,
    B'Polynomials=>{f1,f2}|yySlice#ListB'Sections)
runBertini(storeBM2Files)
yyDegree=#importSolutionsFile(storeBM2Files)"


