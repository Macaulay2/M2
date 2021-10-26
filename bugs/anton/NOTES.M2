Rings:
  Ring -- abstract high level ring
  ConcreteRing : Ring -- front end "slow" ring
  RingInterface -- abstract low level ring
  ARingBLA -- engine "fast" ring (BLA = RR,CC,ZZ,QQ,...)
Matrices:
  DMat<RT> -- dense matrix with entries in ARing RT   
  SMat<RT> -- sparse matrix with entries in ARing RT 
  MutableMatrix -- (abstract) front end mutable matrix with entries in Ring
  <Mat>    -- refers to DMat or SMat
  MutableMat<Mat> : MutableMat<Mat> -- a wrapper 
  Matrix   -- front end matrix with entries in Ring (very DIFFERENT from MutableMatrix)

Shortcuts:
  Ctrl-u F11 -- send .m2 lines to a different from *M2* buffer 
  Ctrl-u F12 -- start M2 with a specified command
