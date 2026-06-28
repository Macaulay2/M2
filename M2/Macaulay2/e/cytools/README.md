Code in this directory is from cytools

- computeGV, computing GV and GW invariants
- box_enum, finding lattice points in a polyhedron
- might include others in the future

Note: the computeGV code is older code, the current version is in
rust.  It works well though.  I do not know if this version is still
in source control.  Need to check on that.  The license is GPL >= 3.

The box_enum code is from the repo:
git@github.com:natemacfadden/latticepts.git, author is Nate MacFadden
the license is GPL >= 3.

cone-interior-point.cpp: uses glpk to get whether cone id feasible and
if so, an interior point, and if not, a dual certificate. This is just test
code, suggested by claude.
