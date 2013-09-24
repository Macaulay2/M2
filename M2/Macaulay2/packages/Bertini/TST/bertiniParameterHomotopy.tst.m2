needsPackage "Bertini"

myPoints={ {
      point{{11, 1}},
       point{{13, 1}},
        point{{12, 1}}	  },
 {
      point{{11, 2}},
       point{{13, 2}},
        point{{12, 2}}	  }}

R=CC[u1,u2,u3,x,y]
f1=u1*(y-1)+u2*(y-2)+u3*(y-3)
f2=(x-11)*(x-12)*(x-13)
bPH=bertiniParameterHomotopy(
     {f1,f2},--list of polynomials thathave zero dimensional solutions
     {u1,u2,u3},--your parameters
     {	  {{1,0,0}},	  {{0,1,0}}	  })---list of {list of numbers})
assert(areEqual(sortSolutions myPoints_0,   sortSolutions  bPH_0))
assert(areEqual(sortSolutions myPoints_1,   sortSolutions  bPH_1))

