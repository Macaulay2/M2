restart
loadPackage ("NumericalAlgebraicGeometry", FileName=>"../../NumericalAlgebraicGeometry.m2", 
     Configuration=>{"PHCpack"=>"../phc"})
debug NumericalAlgebraicGeometry; DBG = 1; printingPrecision = 20; 
scan({"PHCstart", "PHCtarget", "PHCoutput", 
     "PHCstartsols", "PHCtargetsols", "PHCbat", "phc_session.log"}, try removeFile);
 

R = CC[x,y];
S = {x*(x-1)*y,x*(x-1),(x-y)*(x+y+1)};
solsS = {{0,0},{1,1},{1,-1},{2,-2}}
T = {x*(x-1)*y,x*(x-1),y-(x+1)^2};
print track(S,T,solsS, gamma=>0.6+0.8*ii,Software=>PHCpack)

 