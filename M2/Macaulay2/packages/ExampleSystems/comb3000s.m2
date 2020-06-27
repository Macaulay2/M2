export {"comb3000s"}


comb3000s = method();
comb3000s(Ring) := kk -> (
    x := symbol x;
    R := kk[x_1..x_10];
    {  1.01815483301669e-1*x_2+ 9.89610100506422e-1*x_6+ 1.34637048100730*x_9 + 3.46970317210432*x_10-2.12454115933396,
       5.76739795357135e-1*x_3+ 7.89949754301577e-1*x_8-2.19492968782850,
       7.19621954936988e-1*x_9+ 9.27261335190857e-1*x_10+ 8.95128807036246e-1*x_3+ 2.45208250541714*x_8+ 8.73159766974099e-2*x_1+ 1.37722259176202*x_5-5.67773314994310,
       2.50030218520604e-3*x_4+ 19.9987913687438*x_7-19.9987913687440,
       -1.37722259176203*x_1^2+ 7.26099038733160e-1*x_5,
       -9.89610100506425e-1*x_2^2+ 1.01049898287039*x_6,
       -19.9987913687438*x_4^2+ 5.00030217607503e-2*x_7,
       -1.93702197268146*x_3*x_1+ 5.16256404988361e-1*x_8,
       -9.68877757611935e-1*x_2*x_1+ 1.03212194948595*x_9,
       -3.21732159608141*x_2^2*x_1+ 3.10817545009477e-1*x_10
       }
)

beginDocumentation()

doc /// 
    Key
    	comb3000s
	(comb3000s,Ring)
    Headline
    	a combustion chemistry example for a temparature of 3000 degrees
    Usage
    	comb3000s(kk)
    Inputs
    	kk:Ring
	    the coefficient ring
    Outputs
    	:List	
	    of the polynomials in the system
    Description
    	Text
	   This system was solved in May 2020, using @TO solveSystem@ in Macaulay2 v1.15
	     with an Intel(R) Core(TM) i5-4258U CPU at 2.40GHz.
	   
	    There were 16 solutions found in 0.707914 seconds (with a Bezout bound of 96).
	    
	    Reference: 
	    A.P. Morgan.
	    Solving Polynomial Systems Using Continuation for Engineering and Scientific Problems',
	    Prentice-Hall, Englewood Cliffs, N.J., 1987. Chapter 9.
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/comb3000s.html
	Example
	    comb3000s(RR_53)
    ///