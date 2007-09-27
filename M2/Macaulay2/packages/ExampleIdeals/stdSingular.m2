--------------------------------------------------------
--alex1, Singular standard bases#1.
R = (ZZ/101){t,x,y,z}
ideal"5t3x2z+2t2y3x5,7y+4x2y+y2x+2zt,3tz+3yz2+2yz4"
{* -- Singular code
  ring R=101,(t,x,y,z),ds;
  ideal i = 5t3x2z+2t2x5y3,7y+2tz+4x2y+xy2,3tz+3yz2+2yz4;
  timer=1;
  option(prot);
  std(i);
*}
--------------------------------------------------------
