#include "brp.h"

map<int,BRP> testLongTest2Example() {
   map<int,BRP> G;
   G[0] = BRP(393216);          ;
   G[1] = BRP(1011713) + BRP(2111) + BRP(4) + BRP(2);
   G[2] = BRP(393510) + BRP(262146) + BRP(2048) + BRP(200);
   G[3] = BRP(690184) + BRP(65704) + BRP(24576);
   G[4] = BRP(2048) + BRP(448) + BRP(12) + BRP(8);
   return G;
}
map<int,BRP> testLongTest2ExampleCorrect() {
   map<int,BRP> correct;
   correct[0] = BRP(4) + BRP(2);          
   correct[1] = BRP(74) + BRP(72) + BRP(10) + BRP(8);
   correct[2] = BRP(138) + BRP(136) + BRP(10) + BRP(8);
   correct[3] = BRP(266) + BRP(264);
   correct[4] = BRP(450) + BRP(448);
   correct[5] = BRP(505);
   correct[6] = BRP(2048) + BRP(448) + BRP(10) + BRP(8);
   correct[7] = BRP(24584) + BRP(24576);
   correct[8] = BRP(24642) + BRP(24640) + BRP(24578) + BRP(24576);
   correct[9] = BRP(24672) + BRP(24640) + BRP(24608) + BRP(24576);
   correct[10] = BRP(24704) + BRP(24576);
   correct[11] = BRP(24834) + BRP(24832);
   correct[12] = BRP(24864) + BRP(24832) + BRP(24610) + BRP(24578);
   correct[13] = BRP(24913) + BRP(24627) + BRP(24595);
   correct[14] = BRP(57376) + BRP(57344) + BRP(24608) + BRP(24576);
   correct[15] = BRP(65721) + BRP(65595) + BRP(65593) + BRP(24627);
   correct[16] = BRP(65768) + BRP(65704) + BRP(24640) + BRP(24576);
   correct[17] = BRP(65960) + BRP(65704) + BRP(65578) + BRP(65576) + BRP(24832) + BRP(24578);
   correct[18] = BRP(90163) + BRP(24627);
   correct[19] = BRP(90176) + BRP(90112) + BRP(24640) + BRP(24576);
   correct[20] = BRP(90368) + BRP(90114) + BRP(24832) + BRP(24578);
   correct[21] = BRP(98346) + BRP(98344) + BRP(65578) + BRP(65576) + BRP(57346) + BRP(57344) + BRP(24578) + BRP(24576);
   correct[22] = BRP(98472) + BRP(65704) + BRP(57344) + BRP(24576);
   correct[23] = BRP(122880) + BRP(90112) + BRP(57344) + BRP(24576);
   correct[24] = BRP(131321) + BRP(131131) + BRP(131129);
   correct[25] = BRP(131520) + BRP(131272) + BRP(131082) + BRP(131080);
   correct[26] = BRP(155680) + BRP(155648) + BRP(24608) + BRP(24576);
   correct[27] = BRP(155729) + BRP(155667) + BRP(155665) + BRP(24627) + BRP(24595);
   correct[28] = BRP(155968) + BRP(155712) + BRP(155650) + BRP(155648);
   correct[29] = BRP(196650) + BRP(196648) + BRP(155650) + BRP(155648) + BRP(65578) + BRP(65576) + BRP(24578) + BRP(24576);
   correct[30] = BRP(196776) + BRP(155648) + BRP(65704) + BRP(24576);
   correct[31] = BRP(221184) + BRP(155648) + BRP(90112) + BRP(24576);
   correct[32] = BRP(262146) + BRP(448) + BRP(200) + BRP(10) + BRP(8);
   correct[33] = BRP(262216) + BRP(262152);
   correct[34] = BRP(262280) + BRP(262152);
   correct[35] = BRP(262408);
   correct[36] = BRP(262592) + BRP(456) + BRP(448);
   correct[37] = BRP(286752) + BRP(286720);
   correct[38] = BRP(286784) + BRP(286720);
   correct[39] = BRP(286976);
   correct[40] = BRP(327720) + BRP(286720);
   correct[41] = BRP(352256) + BRP(286720);
   correct[42] = BRP(393216);
   correct[43] = BRP(548896) + BRP(548864) + BRP(24608) + BRP(24576);
   correct[44] = BRP(589866) + BRP(589864) + BRP(548866) + BRP(548864) + BRP(65578) + BRP(65576) + BRP(24578) + BRP(24576);
   correct[45] = BRP(589992) + BRP(548864) + BRP(65704) + BRP(24576);
   correct[46] = BRP(614400) + BRP(548864) + BRP(90112) + BRP(24576);
   correct[47] = BRP(688138) + BRP(688136) + BRP(65578) + BRP(65576) + BRP(24578) + BRP(24576);
   correct[48] = BRP(688328) + BRP(65704) + BRP(24576);
   correct[49] = BRP(712706) + BRP(712704) + BRP(90146) + BRP(90144) + BRP(24578) + BRP(24576);
   correct[50] = BRP(712768) + BRP(90144) + BRP(24576);
   return correct;
}
