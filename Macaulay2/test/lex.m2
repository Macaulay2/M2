-- From: Javier Fernandez <jfernand@math.umass.edu>
-- Subject: known problem?
-- To: Macaulay2@math.uiuc.edu
-- Date: Fri, 18 Feb 2000 11:26:46 -0500 (EST)

if getenv "USER" == "dan" then exit 0

gbTrace 3
R = QQ[u,v,x,y,z,MonomialOrder=>Lex]
li = {x - 3*u-3*u*v^2+u^3, y-3*v-3*u^2*v+v^3, z-3*u^2+3*v^2}
gblex = gb ideal li					    -- takes forever, in all versions
