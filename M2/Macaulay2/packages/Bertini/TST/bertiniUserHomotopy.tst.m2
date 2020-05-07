loadPackage("Bertini",Reload=>true)


---

R=QQ[t0,x,y,T]
--We have a system of two equations.
aUserHomotopy={(t0)^2*(x^2-1)+(1-t0)^2*(x^2-9),y-1}
--the start points for the homotopy are for t=1 and below:
startPoints={point{{1,1}},
     point {{-1,1}}}
--the input for bertiniTrackHomotopy is 
---1) path variable
---2) start system
---3) start points
--the output for bertiniTrackHomotopy is a list of points. 
targetPoints=bertiniUserHomotopy(
    T,{t0=>T}, aUserHomotopy,startPoints,TopDirectory=>storeBM2Files)
assert(areEqual(sortSolutions targetPoints, {point {{-3, 1}}, point {{3, 1}}}))




---The variables of your ring consist of parameters, then unknowns.
R=QQ[t0,x,y]
--We have a system of two equations.
aUserHomotopy={(t0)^2*(x^2-1)+(1-t0)^2*(x^2-9),y-1}
--the start points for the homotopy are for t=1 and below:
startPoints={point({{1,1}}),
     point({{-1,1}})}

--the input for bertiniTrackHomotopy is 
---1) path variable
---2) start system
---3) start points

--the output for bertiniTrackHomotopy is a list of points. 
targetPoints=bertiniUserHomotopy(
    T,{t0=>T}, aUserHomotopy,startPoints,TopDirectory=>storeBM2Files)
assert(areEqual(sortSolutions targetPoints, {point {{-3, 1}}, point {{3, 1}}}))

---Track homotopy tst
R=QQ[t0,x,y]
--We have a system of two equations.
aUserHomotopy={(t0)^2*(x^2-1)+(1-t0)^2*(x^2-9),y-1}
--the start points for the homotopy are for t=1 and below:
startPoints={point({{1,1}}),     point({{-1,1}})}
assert(areEqual(sortSolutions targetPoints, {point {{-3, 1}}, point {{3, 1}}}))	    
	    


H = { "(x^2-y^2)*a +(2*x^2-3*x*y-5*y^2)*(1-a)"}
s=bertiniUserHomotopy(t, {a=>t},H, {point{{1,1}}, point{{ -1,1}}},HomVariableGroup=>{x,y} ) 
assert(#s==2)