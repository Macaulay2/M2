-- this code makes k-basis go into an infinite loop, calling itself recursively, in the engine
-- if k-basis can't work over non-fields, the engine should signal an error

A = ZZ[x]/( 2*x^2 )
basis A
