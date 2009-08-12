-- Bug part of this has been fixed (before 12 Jan 2008).  The // still chooses
-- a poor lift, which is then much improved by the G % (syz gens J) line.

-- Over a quotient ring, sometimes // can return elements which are not in normal form:

R = ZZ/101[x,y,t]/(t^2+5)
R = ZZ[x,y,t]/(t^2+5)
J = ideal"3xy+1,(4+2t)y+9"
F = poly"(4t-1)x2y+6txy2+9tx2+3x-4y-9"
F % J
G = F // (gens J) -- WRONG!!  well,  it is ow correct, jusst not very nice.
G % (syz gens J) -- over ZZ, G is more complicated than it shhould be.
((gens J) * G)_(0,0) == F 
