export{"proddeco"}

proddeco = method()
proddeco (Ring) := kk -> (
    z := symbol z;
    R := kk[z_1..z_4];
    {64.632945216*z_1^2*z_4^2 + 55.661869232*z_1^2*z_4 - 129.265890432*z_1*z_2*z_3*z_4- 55.661869232*z_1*z_2*z_3 + 12.201006656*z_1*z_2*z_4 + 48.21149907*z_1*z_3*z_4 + 50.7965557025*z_1*z_3 + 71.3989540236*z_1*z_4^2 + 62.3536335897*z_1*z_4+ 64.632945216*z_2^2*z_3^2 - 12.201006656*z_2^2*z_3 - 48.21149907*z_2*z_3^2- 71.3989540236*z_2*z_3*z_4 + 9.14136787*z_2*z_3 + 17.5403018476*z_2*z_4,
	81.3704061375*z_1^2*z_4^2 + 55.3637563425*z_1^2*z_4 - 162.740812275*z_1*z_2*z_3*z_4- 55.3637563425*z_1*z_2*z_3 + 25.032598875*z_1*z_2*z_4 + 21.781944477*z_1*z_3*z_4+ 24.7967262078*z_1*z_3 + 55.073652285*z_1*z_4^2 + 41.688210899*z_1*z_4+ 81.3704061375*z_2^2*z_3^2 - 25.032598875*z_2^2*z_3 - 21.781944477*z_2*z_3^2- 55.073652285*z_2*z_3*z_4 + 9.56199577*z_2*z_3 + 20.93013785*z_2*z_4,
	35.2817031945*z_1^2*z_4^2 + 42.2584220358*z_1^2*z_4 - 70.563406389*z_1*z_2*z_3*z_4- 42.2584220358*z_1*z_2*z_3 + 23.8847040258*z_1*z_2*z_4 + 8.0797827915*z_1*z_3*z_4+ 13.5030253426*z_1*z_3 + 23.1780987025*z_1*z_4^2 + 37.033791391*z_1*z_4+ 35.2817031945*z_2^2*z_3^2 - 23.8847040258*z_2^2*z_3 - 8.0797827915*z_2*z_3^2- 23.1780987025*z_2*z_3*z_4 + 10.3467518726*z_2*z_3 + 16.946539941*z_2*z_4,
	6.8993630265*z_1^2*z_4^2 + 43.1035038882*z_1^2*z_4 - 13.798726053*z_1*z_2*z_3*z_4 - 43.1035038882*z_1*z_2*z_3 + 56.0575143198*z_1*z_2*z_4 + 2.379925759*z_1*z_3*z_4+ 16.2879242092*z_1*z_3 + 6.40051015*z_1*z_4^2 + 42.97692982*z_1*z_4+ 6.8993630265*z_2^2*z_3^2 - 56.0575143198*z_2^2*z_3 - 2.379925759*z_2*z_3^2- 6.40051015*z_2*z_3*z_4 + 22.5597322388*z_2*z_3 + 55.41365098*z_2*z_4	}
)

beginDocumentation()

doc ///
    Key
    	proddeco
	(proddeco, Ring)
    Headline
    	system with a product-decomposition structure
    Usage
    	prodecco(kk)
    Inputs
    	kk:Ring
	    	the coefficient ring
    Outputs
    	:List
	    	of the polynomials in the system
    Description
    	Text
	    This system was solved in May 2020, using @TO solveSystem@ in Macaulay2 v1.15
	     with an Intel(R) Core(TM) i5-5250U CPU at 1.60GHz.
	   
	    There were 68 solutions found in 7.236 seconds (with a Bezout bound of 256).
	    
	    Reference: "A product-decomposition theorem for bounding Bezout numbers" by A.P. Morgan, A.J. Sommese, and C.W. Wampler (pages 1308-1325).
	        
	    See also: http://homepages.math.uic.edu/~jan/Demo/proddeco.html.
	Example
    	    proddeco(RR_53)
   ///