----------------------------------------------
--joswig101
  -- example suggested by M. Joswig
  R1 = ZZ/101[x4,x3,x2,x1,s,t,MonomialOrder=>Eliminate 4]
  ideal (
       1 + s^2  * x1 * x3 + s^8 * x2 * x3 + s^19 * x1 * x2 * x4,
       x1 + s^8 * x1 * x2 * x3 + s^19 * x2 * x4,
       x2 + s^10 * x3 * x4 + s^11 * x1 * x4,
       x3 + s^4 * x1 * x2 + s^19 * x1 * x3 * x4 + s^24 * x2 * x3 * x4,
       x4 + s^31 * x1 * x2 * x3 * x4
       )
