--- status: Rewritten July 2018
--- author(s): Lily Silverstein 
--- notes: 

doc ///
 Key
  uniform
 Headline
  whether all elements of a list are the same class
 Usage
  uniform L
 Inputs
  L:List
 Outputs
  :Boolean
   whether all elements of {\tt L} are of the same @TO class@
 Description
  Example
   uniform {2, 5, 0}
   uniform {2*0.5, 5*0.5, 0/2}
  Text
   The second list is not uniform because 0/2 is represented as a rational 
   number (of class @TO QQ@), while 2*0.5 and 5*0.5 are represented as real
   numbers (of class @TO RR@).
  Example
   uniform {hi, "hello"}
   uniform {"hi", "hello"}
   R = QQ[x,y,z]; 
   uniform {x^2*y*z, 5*y, 12/7}
   uniform {x^2*y*z, 5*y, (12/7)_R}
   S = ZZ[t];
   uniform {monomialIdeal(x), monomialIdeal(t)}
   uniform {monomialIdeal(t), ideal(t)}
   uniform {S/monomialIdeal(t), S/ideal(t)}
 SeeAlso
  all
  any
  instance
  same
  select
  "lists and sequences"
///
