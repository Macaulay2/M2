 R = ZZ/101[a]
 f = a  * id_(R^1)
 g = a^2* id_(R^1)
 assert( degree f == {1} )
 assert isHomogeneous f
 assert( degree g == {2} )
 assert isHomogeneous g
 h = g // f
 assert( degree h == {1} )
 assert isHomogeneous h
-- Local Variables:
-- compile-command: "make lift.okay "
-- End:
