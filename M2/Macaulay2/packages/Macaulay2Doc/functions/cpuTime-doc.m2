--- status: DRAFT
--- author(s): MES
--- notes: 

doc ///
  Key
    cpuTime
  Headline
    seconds of cpu time used since Macaulay2 began
  Usage
    cpuTime()
  Outputs
    :RR
      the number of seconds of cpu time used since the program was started
  Description
   Example
     t1 = cpuTime()
     for i from 0 to 1000000 do 223131321321*324234324324;
     t2 = cpuTime()
     t2-t1
  Caveat
    Under Linux, the cpu time reported is that consumed by the thread, but under MAC OS X,
    it is that consumed by the process.
  SeeAlso
    "time"
    "timing"
    currentTime
///
