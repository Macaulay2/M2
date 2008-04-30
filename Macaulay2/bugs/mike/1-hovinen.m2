-- This bug is in ~/Desktop/Brad\ Hovinen

path = prepend("/Users/mike/Desktop/Brad Hovinen/", path)
load "discriminant.m2"
loadPackage "ModuleDeformations"
(IDelta,phi)=Discriminant(4)
IDelta=prune substitute(IDelta,{a_2=>6*a_2,a_3=>4*a_3,a_4=>3*a_4})
R=(ring IDelta)/IDelta
OSigma=QQ[a_2,Degrees=>{2}]
use R
phi=map(R,OSigma,matrix({{a_2}}))
use OSigma
S=R/phi(ideal a_2)
deformMCMModule(module ideal (a_3,a_4),phi)
deformMCMModule(module ideal (a_3,a_4),phi) -- doing it again causes the segmentation fault!

-- the problem: fixed 4/30/08.
-- the second deformMCMModule is over a DIFFERENT ring than the first.
-- in this ring, the compution tries to compute a basis of a non 
-- zero-dimensional module, and crashed.  Now it gives an error.
