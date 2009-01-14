--- status: DRAFT
--- author(s): MES
--- notes: 

document { 
     Key => cpuTime,
     Headline => "seconds of cpu time used since Macaulay 2 began",
     Usage => "cpuTime()",
     Outputs => {
	  RR => "the number of seconds of cpu time used since the program was started"
	  },
     EXAMPLE lines ///
          cpuTime()
	  for i from 0 to 1000000 do 223131321321*324234324324;
	  cpuTime()
	  ///
     }
