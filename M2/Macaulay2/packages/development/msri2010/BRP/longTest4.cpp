#include "brp.h"

map<int,BRP> testLongTest4Example() {
   map<int,BRP> G;
   G[0] = BRP(1015808);        
   G[1] = BRP(393216) + BRP(262144) + BRP(131072) + BRP(98304) + BRP(65536);
   G[2] = BRP(786432) + BRP(524289) + BRP(196608) + BRP(49152) + BRP(16384) + BRP(14336) + BRP(5120) + BRP(3072) + BRP(2048) + BRP(6);
   G[3] = BRP(524544) + BRP(131584) + BRP(16384) + BRP(8192) + BRP(6154) + BRP(192) + BRP(48) + BRP(32) + BRP(7);
   G[4] = BRP(196608) + BRP(2048) + BRP(1024);
   G[5] = BRP(1011713) + BRP(2111) + BRP(4) + BRP(2);
   G[6] = BRP(192) + BRP(48);
   G[7] = BRP(662529) + BRP(262146) + BRP(2048) + BRP(208) + BRP(8);
   G[8] = BRP(393510) + BRP(262146) + BRP(2048) + BRP(200);
   G[9] = BRP(262656) + BRP(448) + BRP(64) + BRP(8);
   G[10] = BRP(690184) + BRP(262144) + BRP(65704) + BRP(65536) + BRP(24576);
   G[11] = BRP(262146) + BRP(2048) + BRP(1027) + BRP(200) + BRP(2);
   G[12] = BRP(262656) + BRP(3072) + BRP(384) + BRP(64) + BRP(12);
   G[13] = BRP(262656) + BRP(65728) + BRP(2048);
   G[14] = BRP(262144) + BRP(67584) + BRP(448) + BRP(8);
   G[15] = BRP(524800) + BRP(147776);
   G[16] = BRP(131072) + BRP(2048) + BRP(448) + BRP(12) + BRP(8);
   return G;
}
map<int,BRP> testLongTest4ExampleCorrect() {
   map<int,BRP> correct;
   correct[0] = BRP(2); 
   correct[1] = BRP(4);
   correct[2] = BRP(8);
   correct[3] = BRP(48);
   correct[4] = BRP(64);
   correct[5] = BRP(384);
   correct[6] = BRP(1024);
   correct[7] = BRP(2048);
   correct[8] = BRP(16417) + BRP(8225) + BRP(33);
   correct[9] = BRP(16512) + BRP(8320) + BRP(160);
   correct[10] = BRP(16640) + BRP(16384) + BRP(8448) + BRP(8192) + BRP(288) + BRP(32);
   correct[11] = BRP(16896) + BRP(8704) + BRP(544);
   correct[12] = BRP(24577) + BRP(8225) + BRP(8193);
   correct[13] = BRP(40977) + BRP(8209);
   correct[14] = BRP(40993) + BRP(40961) + BRP(8225) + BRP(8193);
   correct[15] = BRP(41089) + BRP(41088) + BRP(32929) + BRP(32928) + BRP(8321) + BRP(8320) + BRP(161) + BRP(160);
   correct[16] = BRP(41104) + BRP(8336);
   correct[17] = BRP(41120) + BRP(41088) + BRP(8352) + BRP(8320);
   correct[18] = BRP(41216) + BRP(40961) + BRP(40960) + BRP(33056) + BRP(32801) + BRP(32800) + BRP(8448) + BRP(8193) + BRP(8192) + BRP(288) + BRP(33) + BRP(32);
   correct[19] = BRP(41472) + BRP(33312) + BRP(8704) + BRP(544);
   correct[20] = BRP(49152) + BRP(40961) + BRP(32801) + BRP(16385) + BRP(16384);
   correct[21] = BRP(65536) + BRP(24576);
   correct[22] = BRP(131072);
   correct[23] = BRP(262144);
   correct[24] = BRP(524289) + BRP(40961) + BRP(32801) + BRP(16385);
   correct[25] = BRP(524544) + BRP(16384) + BRP(8192) + BRP(32);
   correct[26] = BRP(524800);
   correct[27] = BRP(540672) + BRP(532480) + BRP(524320) + BRP(16384) + BRP(8192) + BRP(32);
   correct[28] = BRP(565248) + BRP(557088) + BRP(532480) + BRP(524320) + BRP(40960) + BRP(32800) + BRP(8192) + BRP(32);
   return correct;
}
