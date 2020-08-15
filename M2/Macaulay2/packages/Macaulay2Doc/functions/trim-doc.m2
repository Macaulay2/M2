document {
     -- this is the old version
     Key => {trim,(trim,Ideal),(trim,Ring),(trim,Module),(trim,QuotientRing)},
     Headline => "minimize generators and relations",
     "There are two ways to present a module ", TT "M", " over a ring.  One way is to take
     a free module F (whose generators are called the generators) and
     form the quotient M = F/H by a submodule H of F (whose generators are called the relations).
     Another way is take a free module F, a submodule G of F (whose generators are called 
     the relations), a submodule H of F (whose generators are called the relations), and
     form the subquotient module M = (G+H)/H, obtained also as the image of G in F/H.
     The purpose of ", TT "trim", " is to minimize presentations of the latter type.
     This applies also to rings and ideals.",
     }

--- status: Draft
--- author(s): Amelia Taylor
--- notes: Be sure to note that trim I and trim R^1/I do 
---        the same thing as the minimal relations for 
---        R^1/I are the minimal generators for I.

-- there seems to be no text in the nodes below ... [drg]



///

R = ZZ/101[x,y,z,u,w]
I = ideal(x^2-x^2-y^2,z^2+x*y,w^2-u^2,x^2-y^2)
trim I
trim (R^1/I)
R = ZZ/32003[a..d]
M = coker matrix {{a,1,b},{c,3,b+d}}
trim M
prune M
///
