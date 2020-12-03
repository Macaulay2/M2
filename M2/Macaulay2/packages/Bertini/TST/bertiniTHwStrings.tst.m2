needsPackage "Bertini"

---The variables of your ring consist of parameters, then unknowns.
R=QQ[t0,x,y]
--We have a system of two equations.
aUserHomotopy={"(t0)^2*(x^2-1)+(1-t0)^2*(x^2-9)","y-1"}
Z--the start points for the homotopy are for t=1 and below:
startPoints={point({{1,1}}),
     point({{-1,1}})}

--the input for bertiniTrackHomotopy is 
---1) start system
---2) parameter
---3) start points

--the output for bertiniTrackHomotopy is a list of points. 
targetPoints=bertiniTrackHomotopy(t0, aUserHomotopy,startPoints,allowStrings=>{x,y})
assert(areEqual(targetPoints, {point {{3, 1}}, point {{-3, 1}}}))
