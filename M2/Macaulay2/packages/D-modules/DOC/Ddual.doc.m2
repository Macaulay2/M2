needs "D-modules.m2"

document { Ddual,
     TT "Ddual M", " -- 
     computes the holonomic dual of a D-module M",
     BR, NOINDENT,
     TT "Ddual I", " -- 
     computes the holonomic dual of the quotient module D/I",

     PARA,
     "If M is a holonomic left D-module, then Ext^n_D(M,D) is a holonomic
     right D-module.  The holonomic dual is defined to be the left
     module associated to Ext^n_D(M,D).  The dual is obtained by
     computing a free resolution of M, dualizing, and applying
     the standard transposition to the nth homology.",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "I = AppellF1({1,0,-3,2})",
     EXAMPLE "Ddual I",
     
     PARA,
     "Caveats and known problems :",
     MENU{"The input module M should be holonomic.  The user should
	  check this manually with the script Ddim."},
     
     SEEALSO {"Ddim", "Dtransposition"}
     }
