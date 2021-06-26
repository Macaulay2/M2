// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "mathicgb/stdinc.h"
#include "ideals.hpp"

#include <sstream>

using namespace mgb;

std::string smallIdealComponentLastDescending() {
  return 
    "32003 6\n"
    "1 1 1 1 1 1 1\n"
    "_revlex revcomponent\n"
    "3\n"
    "-bc+ad\n"
    "-b2+af\n"
    "-bc2+a2e\n";
}

const char* idealSmallBasis = "\
0 <0>  bc-ad\n\
1 <1>  b2-af\n\
2 <2>  bc2-a2e\n\
3 c<0>  acd-a2e\n\
4 b<0>  abd-acf\n\
5 c2<1>  a2be-ac2f\n\
6 bc<0>  a2d2-ac2f\n\
7 c3<1>  a3de-ac3f\n\
8 c4<1>  a4e2-ac4f\n\
";

const char* idealSmallSyzygies =
  "  0: b2  bc2  \n  1: c2d  bc2  \n";

const char* idealSmallInitial =
  "  bc\n  b2\n  acd\n  abd\n  a2be\n  a2d2\n  a3de\n  a4e2\n";

std::string liuIdealComponentLastDescending() {
  return 
    "2 6\n"
    "1 1 1 1 1 1 1\n"
    "_revlex revcomponent\n"
    "4\n"
    "bc+bd+af+ef\n"
    "ac+cd+bf+ef\n"
    "ad+bd+cf+ef\n"
    "ab+ac+df+ef\n";
}

const char* liu_gb_strat0_free1 = 
"\
0 <0>  bc+bd+af+ef\n\
1 <1>  ac+cd+bf+ef\n\
2 <2>  ad+bd+cf+ef\n\
3 <3>  ab+ac+df+ef\n\
4 c<2>  bd2+cd2+c2f+cef+cf2+ef2\n\
5 b<2>  b2d+cd2+d2f+bef+af2+ef2\n\
6 b<1>  c2d+cd2+b2f+c2f+cdf+bef+cef+def+af2+ef2\n\
7 a<0>  a2f+b2f+c2f+d2f+aef+bef+cef+def\n\
8 ad<0>  cd2f+d3f+cdef+d2ef+b2f2+c2f2+bdf2+aef2+def2+e2f2+bf3+ef3\n\
9 c2<2>  c3f+d3f+c2ef+bdef+cdef+d2ef+b2f2+c2f2+d2f2+aef2+bef2+e2f2+af3+bf3\n\
10 b2<1>  b3f+d3f+b2ef+d2ef+b2f2+c2f2+bef2+cef2+bf3+df3\n\
11 c2d<2>  b2ef2+d2ef2+be2f2+de2f2+b2f3+d2f3+aef3+cef3+af4+bf4+cf4+df4\n\
12 c2d2<2>  bde2f2+cde2f2+c2ef3+d2ef3+ae2f3+e3f3+c2f4+bdf4+cdf4+d2f4+aef4+def4+df5+ef5\n\
13 c2d3<2>  c2e2f3+d2e2f3+ce3f3+de3f3+c2ef4+d2ef4+be2f4+ce2f4+aef5+cef5+af6+bf6+cf6+df6\n\
14 c2d4<2>  cde2f4+d2e2f4+ae3f4+be3f4+ce3f4+e4f4+ce2f5+e3f5+cdf6+d2f6+aef6+bef6+cef6+e2f6+cf7+ef7\n\
15 c2d5<2>  d2e3f4+de4f4+ae3f5+be3f5+d2ef6+be2f6+aef7+bef7+bf8+df8\n\
";

const char* liu_syzygies_strat0_free1 =
  "  0: ac  ab  a2d  \n  1: ab  b2d  b2c  \n  2: bc  ac  ab  c3d  \n";

const char* liu_initial_strat0_free1 =
  "  ad\n  bc\n  ac\n  ab\n  a2f\n  bd2\n  c2d\n  b2d\n  cd2f\n  c3f\n  b3f\n  b2ef2\n  bde2f2\n  c2e2f3\n  cde2f4\n  d2e3f4\n";

std::string weispfennig97IdealComponentLast(bool componentsAscending) {
  std::ostringstream out;
  out << "7583 4 schreyer revlex 1\n";
  if (componentsAscending)
    out << "1 1 1 1 _revlex component\n";
  else
    out << "1 1 1 1 _revlex revcomponent\n";
  out <<
    "3\n"
    "b4+ab2c+a2d2-2abd2+b2d2+c2d2 \n"
    "a2b2c-bc4+a3d2+ab2d2+ac2d2+3d5 \n"
    "a3b2-abc3+a2d3+b2d3+c2d3\n";
  return out.str();
}


const char* weispfennig97_gb_strat0_free4 = "\
0 <0>  b4+ab2c+a2d2-2abd2+b2d2+c2d2\n\
1 <1>  a2b2c-bc4+a3d2+ab2d2+ac2d2+3d5\n\
2 <2>  a3b2-abc3+a2d3+b2d3+c2d3\n\
3 c<2>  a4d2+a2b2d2+a2c2d2-a2cd3-b2cd3-c3d3+3ad5\n\
4 b2<1>  b3c4+abc5-2a3bcd2-ab2c2d2-abc3d2+bc4d2-2a2bd4+a2d5-2b2d5-3acd5+c2d5-3d7\n\
5 b2<2>  ab3c3+a2bc4+2a2b3d2+2a2bc2d2-a2b2d3-2a2bcd3+ab2cd3-2b3cd3-b2c2d3-2bc3d3-2a2d5+4abd5+b2d5+c2d5\n\
6 b3<1>  ab3c2d2+ab2c3d2+a2c4d2+c6d2+2a2b2d4-a2bd5+2b3d5-2a2cd5+3abcd5-2b2cd5-bc2d5-2c3d5+3bd7\n\
7 b2c<2>  a2bc3d2-abc4d2+bc5d2+ab2c2d3-b2c3d3-bc4d3+2a3bd4-a3cd4-ab2cd4-ac3d4+3ab2d5+a2cd5-2abcd5+b2cd5+c3d5+3ad7-3cd7+3d8\n\
8 b3<2>  a3c3d2+ab2c3d2+ac5d2+a2b3d3-ab3cd3+b3c2d3-2a3d5+2a2bd5-b3d5-2ac2d5-bc2d5\n\
9 ab2<2>  a3bc4+b2c6+a3bc2d2+3ab2c3d2+a2c4d2-abc4d2+c6d2-2a3bcd3-2ab3cd3-ab2c2d3-3abc3d3+bc4d3+2a2b2d4-3a3d5+a2bd5-2a2cd5+3abcd5-2b2cd5-6bc2d5-2c3d5+a2d6+b2d6+c2d6+3bd7-3d8\n\
10 b3c<1>  a2bc4d2-ab2c4d2-a2c5d2-c7d2+2a2b3d4+2a2bc2d4-2bc4d4-a2b2d5-a2bcd5+ab2cd5-4b3cd5+2a2c2d5-3abc2d5+b2c2d5-bc3d5+2c4d5+2a3d6+2ab2d6+2ac2d6-2a2d7+4abd7+b2d7-3bcd7+c2d7+6d9\n\
11 b2c2<2>  ab2c4d2+a2c5d2-abc5d2+bc6d2+c7d2+ab2c3d3-b2c4d3-bc5d3-2a2b3d4+2a3bcd4-a3c2d4-2a2bc2d4-ab2c2d4-ac4d4+2bc4d4+a2b2d5+a2bcd5+2ab2cd5+4b3cd5-a2c2d5+abc2d5+bc3d5-c4d5-2a3d6-2ab2d6-2ac2d6+2a2d7-4abd7-b2d7+3acd7+3bcd7-4c2d7+3cd8-6d9\n\
12 ab3<1>  a3c4d2+b2c5d2+ac6d2+bc6d2-a3bcd4-ab3cd4-a3c2d4-ab2c2d4+abc3d4-ac4d4-a3bd5+2ab3d5-2a3cd5+3a2bcd5-2ab2cd5-abc2d5-2ac3d5-2a2d7+3abd7-2b2d7-3bcd7-5c2d7\n\
13 b3c<2>  a2c5d2-abc5d2+b2c5d2+2bc6d2+c7d2-b3c3d3-a2c4d3-2b2c4d3-bc5d3-c6d3-2a2b3d4+a3bcd4-ab3cd4-2a3c2d4-2a2bc2d4-2ab2c2d4+abc3d4-2ac4d4+2bc4d4-a2b2d5+3ab3d5+2a2bcd5+5b3cd5-a2c2d5+abc2d5+2bc3d5-c4d5-2a3d6+a2bd6-2ab2d6-2b3d6+2a2cd6-3abcd6+2b2cd6-2ac2d6+bc2d6+2c3d6-abd7-3b2d7+3acd7-9c2d7+3cd8-6d9\n\
14 a2b2<1>  a3bc5+b2c7+2b2c5d2-2a3bc2d3-2ab2c3d3-2b3c3d3-2abc4d3+2bc5d3+4a2b3d4-6a3bcd4-2ab3cd4+4a2bc2d4+2abc3d4-4a2b2d5+6ab3d5-3a3cd5+2a2bcd5-9ab2cd5-4b3cd5-a2c2d5+4abc2d5-2b2c2d5-7bc3d5-c4d5+a2cd6+b2cd6+c3d6-9a2d7+16abd7-3b2d7-6acd7-6bcd7-3c2d7-3ad8+6bd8-6cd8\n\
15 ab3<2>  a3bd5-ac3d5-b3d6-abcd6+2ad8\n\
16 ab2c2<2>  b2c6d2-bc7d2+abc5d3+b2c5d3-a3bc2d4+3ab2c3d4+a2c4d4+abc4d4+2b2c4d4-2bc5d4+c6d4-a2b3d5-2a3bcd5-ab3cd5-6ab2c2d5-3b3c2d5-abc3d5+2b2c3d5-2bc4d5+2a2b2d6-2ab3d6+2a3cd6+2ab2cd6-2abc2d6-4ac3d6+2a3d7-a2bd7-3ab2d7-5b3d7-7a2cd7+7abcd7-4b2cd7+2ac2d7-5bc2d7-c3d7+a2d8+b2d8-3acd8+c2d8+6ad9-3bd9+6cd9\n\
17 a2b3<1>  abc6d2+bc7d2-2b2c5d3-bc6d3-ab2c3d4+b3c3d4+a2c4d4+2abc4d4-b2c4d4+c6d4+4a2b3d5+4a3bcd5+5ab3cd5+a3c2d5+2ab2c2d5+3b3c2d5-a2c3d5-abc3d5-b2c3d5-2ac4d5+3bc4d5-c5d5+2a2b2d6-ab3d6-a2bcd6+2ab2cd6-4b3cd6-abc2d6+2ac3d6-bc3d6-2a3d7-3ab2d7+5b3d7-a2cd7+2abcd7-4b2cd7-2ac2d7-7c3d7+2a2d8-b2d8+6acd8+3bcd8+5c2d8-4ad9+6bd9-3d10\n\
18 a2b3<2>  a2b3d5+a2bc2d5+a2c3d5+ab3d6-b3cd6-bc3d6-2a2d8+3abd8\n\
19 bc5<2>  a2bc6d2-ab2c6d2+b2c7d2+2abc5d4+2b2c5d4-4bc6d4-5ab2c3d5-2b3c3d5-a2c4d5+b2c4d5+4bc5d5-c6d5-4ab3cd6+3a3c2d6+a2bc2d6+6ab2c2d6+3b3c2d6-3abc3d6+b2c3d6-4ac4d6+2bc4d6+c5d6-3a2b2d7+4ab3d7-a3cd7-3a2bcd7-8ab2cd7-5b3cd7-6a2c2d7+5abc2d7-4b2c2d7-3ac3d7-bc3d7-3c4d7-3a3d8+3a2bd8-ab2d8-5b3d8+6a2cd8-11abcd8+3b2cd8-3ac2d8+3c3d8+a2d9-2abd9-b2d9+8acd9-9bcd9+11c2d9+ad10-15cd10\n\
20 b3c3<1>  a2c7d2+b2c7d2+c9d2-4ab2c3d5+2b3c3d5-3a2c4d5+4abc4d5-2b2c4d5+3bc5d5-3c6d5-2ab3cd6-a3c2d6+a2bc2d6+ab2c2d6+3b3c2d6-abc3d6+2b2c3d6-2ac4d6+3bc4d6+c5d6-3a2b2d7+4ab3d7-a3cd7-3a2bcd7-2ab2cd7+b3cd7-2a2c2d7+3abc2d7-3b2c2d7-5ac3d7+2bc3d7-2c4d7-3a3d8+3a2bd8-4ab2d8-7b3d8+5a2cd8-11abcd8+2b2cd8-3ac2d8+2c3d8+a2d9-2abd9-b2d9+2acd9-3bcd9-c2d9+2ad10-9cd10-3d11\n\
21 b3c3<2>  abc7d2-2bc8d2-abc5d4+2b2c5d4-3bc6d4+2a3bc2d5-7ab2c3d5-4b3c3d5-2a2c4d5+abc4d5-2b2c4d5+3bc5d5-2c6d5-3ab3cd6+3a3c2d6-a2bc2d6+12ab2c2d6+b3c2d6+2a2c3d6-2b2c3d6+3ac4d6+6ab3d7+2a3cd7-6a2bcd7-4ab2cd7+2b3cd7-4a2c2d7+9abc2d7-2b2c2d7+ac3d7+2bc3d7+5c4d7+a2bd8+2b3d8+9a2cd8-14abcd8+9b2cd8+6bc2d8+6c3d8-2a2d9-2abd9+2b2d9+3acd9-9bcd9+8c2d9-ad10+3bd10-15cd10+3d11\n\
22 a2b3c<1>  bc8d2-bc7d3-2526abc5d4-b2c5d4+2531ab2c3d5+3b3c3d5+4b2c4d5-ac5d5-bc5d5+ab3cd6+a2bc2d6-6ab2c2d6-4b3c2d6-2528a2c3d6-2529abc3d6+2b2c3d6-2bc4d6-a2b2d7-2534ab3d7+a2bcd7+2530ab2cd7-2b3cd7+2529a2c2d7-6abc2d7+2527b2c2d7-5ac3d7-2bc3d7+2524c4d7+2a3d8-2ab2d8-7b3d8-5a2cd8+5abcd8-4b2cd8+4ac2d8-3bc2d8+c3d8+a2d9+abd9+2530b2d9-3acd9+6bcd9+2530c2d9+7ad10-12bd10+7cd10\n\
23 a3b3<1>  a2c4d5+abc4d5+b2c4d5-bc5d5+ab3cd6-ab2c2d6-b3c2d6+b2c3d6-ab3d7+a3cd7+ab2cd7-abc2d7-2ac3d7-3ab2d8-3b3d8-3a2cd8+2abcd8-b2cd8-c3d8+3ad10-3bd10+3cd10-3d11\n\
24 abc5<2>  abc8d2-2bc9d2+abc5d5-11b2c5d5-3bc6d5+12ab2c3d6+7b3c3d6-8abc4d6-11b2c4d6+2ac5d6+12bc5d6+7c6d6+12ab3cd7+8a3c2d7-9a2bc2d7+21ab2c2d7+19b3c2d7-8a2c3d7+3abc3d7-17b2c3d7+9ac4d7+16bc4d7+4c5d7+10a2b2d8-6ab3d8-14a3cd8-3a2bcd8-4ab2cd8+5b3cd8+7a2c2d8+9abc2d8+9b2c2d8+29ac3d8+bc3d8+4c4d8-4a3d9-20a2bd9+29ab2d9+63b3d9+23a2cd9-8abcd9-5ac2d9-4bc2d9-3c3d9+18a2d10-20abd10-3b2d10-11acd10+27bcd10+15c2d10-50ad11+48bd11-33cd11+3d12\n\
25 ab3c3<2>  ac6d5+bc6d5+b3c3d6-b2c4d6-a3c2d7-ab2c2d7+2a2c3d7+ac4d7+3a2b2d8+2ab3d8+3ab2cd8+2b3cd8-2ac3d8-a2d10-acd10-3c2d10+3ad11\n\
26 a3b3c<1>  abc5d5+3790bc6d5+3791c7d5-ab2c3d6-3790b2c4d6+3792bc5d6-3790a3c2d7-3790ab2c2d7-a2c3d7-abc3d7+3791ac4d7-bc4d7+3791a2b2d8+3789ab3d8-a2bcd8+3790ab2cd8+3788b3cd8-a2c2d8+3791b2c2d8+a3d9+ab2d9+ac2d9+2a2d10+3789abd10-3790b2d10+acd10+3790bcd10+6c2d10+3790bd11-3cd11+3d12\n\
27 a2bc5<2>  bc10d2+2540bc7d5+2521b2c5d6-2535bc6d6-846c7d6+2511ab2c3d7+2523b3c3d7+2509abc4d7-17b2c4d7+3ac5d7+852bc5d7-2525c6d7+3ab3cd8+2534a3c2d8-2536a2bc2d8+2570ab2c2d8+2569b3c2d8+1669a2c3d8-3371abc3d8-2554b2c3d8-3372ac4d8-1668bc4d8-3c5d8-3357a2b2d9-841ab3d9-2548a3cd9+856a2bcd9+1663ab2cd9+1675b3cd9+2518a2c2d9+2566abc2d9+845b2c2d9-2473ac3d9+2538bc3d9+1688c4d9-3392a3d10-2530a2bd10-792ab2d10+85b3d10+40a2cd10+2517abcd10+2536b2cd10-3395ac2d10+2528bc2d10-2559c3d10-1654a2d11+793abd11-855b2d11-830acd11-2537bcd11-819c2d11-75ad12-2444bd12-53cd12-2507d13\n\
28 a2b3c3<1>  ac7d5+bc7d5-b2c5d6+3790bc6d6+3791c7d6-ab2c3d7-2abc4d7+3791b2c4d7+2ac5d7-3789bc5d7-ab3cd8-3790a3c2d8-a2bc2d8-3784ab2c2d8+5b3c2d8-2a2c3d8-2b2c3d8+3791ac4d8+bc4d8+3791a2b2d9+3790ab3d9-2a3cd9-a2bcd9+3788ab2cd9+3791b3cd9-a2c2d9+4abc2d9+3791b2c2d9+4ac3d9+bc3d9-4a3d10+4a2bd10+4ab2d10+5b3d10+5a2cd10-4abcd10+2b2cd10-5ac2d10-bc2d10-c3d10+3a2d11+3786abd11-3788b2d11+3acd11+3790bcd11+5c2d11-6ad12-3787bd12-9cd12+3d13\n\
29 a3b3c2<1>  bc7d5-3033c8d5-b2c5d6+3033bc6d6-3034ab2c3d7+3034abc4d7+3032b2c4d7+3034ac5d7+3034bc5d7-1515ab3cd8-3035a2bc2d8+1518ab2c2d8-1513b3c2d8+3031a2c3d8+1516b2c3d8+1518bc4d8-1517c5d8-ab3d9-3034a3cd9-3034ab2cd9-3031b3cd9-3032abc2d9-3032ac3d9-3031bc3d9+3031a3d10-1515a2bd10+3033ab2d10+3035b3d10+3034a2cd10+3033abcd10-b2cd10+1514ac2d10+1517bc2d10-4c3d10+1521a2d11-1522abd11+3032b2d11-1516bcd11-3032c2d11-3036ad12-1513bd12-1519cd12+1516d13\n\
30 a3b3c3<1>  c9d5-3033c8d6-b2c5d7+3030bc6d7+2528c7d7-3032ab2c3d8+3b3c3d8+3040abc4d8+3036b2c4d8+3034ac5d8+507bc5d8-c6d8-1516ab3cd9+3a3c2d9-3035a2bc2d9+1520ab2c2d9-1512b3c2d9-2025a2c3d9+2527abc3d9+1517b2c3d9+2528ac4d9-1007bc4d9-1517c5d9+2523a2b2d10+2525ab3d10-3034a3cd10-2528a2bcd10-511ab2cd10-505b3cd10-2a2c2d10-3029abc2d10-2529b2c2d10-3043ac3d10-3028bc3d10+2526c4d10-2025a3d11-1516a2bd11-2031ab2d11+3025b3d11+3033a2cd11+3024abcd11-3542ac2d11+1514bc2d11-c3d11-1006a2d12+3533abd12-2023b2d12+2529acd12-1515bcd12-495c2d12-3032ad13-1521bd13-1522cd13+1511d14\n\
";

const char* weispfennig97_syzygies_strat0_free4 =
  "  1: b4  a4b3  \n  2: b4  a2b2c  a3b3  b3c4  a3bc5  \n";

const char* weispfennig97_initial_strat0_free4 =
  "  b4\n  a2b2c\n  a3b2\n  a4d2\n  b3c4\n  ab3c3\n  a2bc3d2\n  a3c3d2\n  ab3c2d2\n  a3bc4\n  a3bd5\n  a2c5d2\n  ab2c4d2\n  a2b3d5\n  b2c6d2\n  abc6d2\n  a2c4d5\n  bc8d2\n  ac6d5\n  abc5d5\n  bc7d5\n  c9d5\n";

const char* weispfennig97_gb_strat0_free5 = "\
0 <0>  b4+ab2c+a2d2-2abd2+b2d2+c2d2\n\
1 <1>  a2b2c-bc4+a3d2+ab2d2+ac2d2+3d5\n\
2 <2>  a3b2-abc3+a2d3+b2d3+c2d3\n\
3 a<1>  a4d2+a2b2d2+a2c2d2-a2cd3-b2cd3-c3d3+3ad5\n\
4 a2c<0>  b3c4+abc5-2a3bcd2-ab2c2d2-abc3d2+bc4d2-2a2bd4+a2d5-2b2d5-3acd5+c2d5-3d7\n\
5 a3<0>  ab3c3+a2bc4+2a2b3d2+2a2bc2d2-a2b2d3-2a2bcd3+ab2cd3-2b3cd3-b2c2d3-2bc3d3-2a2d5+4abd5+b2d5+c2d5\n\
6 a2bc<0>  ab3c2d2+ab2c3d2+a2c4d2+c6d2+2a2b2d4-a2bd5+2b3d5-2a2cd5+3abcd5-2b2cd5-bc2d5-2c3d5+3bd7\n\
7 ab2<1>  a2bc3d2-abc4d2+bc5d2+ab2c2d3-b2c3d3-bc4d3+2a3bd4-a3cd4-ab2cd4-ac3d4+3ab2d5+a2cd5-2abcd5+b2cd5+c3d5+3ad7-3cd7+3d8\n\
8 a3b<0>  a3c3d2+ab2c3d2+ac5d2+a2b3d3-ab3cd3+b3c2d3-2a3d5+2a2bd5-b3d5-2ac2d5-bc2d5\n\
9 a4<0>  a3bc4+b2c6+a3bc2d2+3ab2c3d2+a2c4d2-abc4d2+c6d2-2a3bcd3-2ab3cd3-ab2c2d3-3abc3d3+bc4d3+2a2b2d4-3a3d5+a2bd5-2a2cd5+3abcd5-2b2cd5-6bc2d5-2c3d5+a2d6+b2d6+c2d6+3bd7-3d8\n\
10 a2bc2<0>  a2bc4d2-ab2c4d2-a2c5d2-c7d2+2a2b3d4+2a2bc2d4-2bc4d4-a2b2d5-a2bcd5+ab2cd5-4b3cd5+2a2c2d5-3abc2d5+b2c2d5-bc3d5+2c4d5+2a3d6+2ab2d6+2ac2d6-2a2d7+4abd7+b2d7-3bcd7+c2d7+6d9\n\
11 ab2c<1>  ab2c4d2+a2c5d2-abc5d2+bc6d2+c7d2+ab2c3d3-b2c4d3-bc5d3-2a2b3d4+2a3bcd4-a3c2d4-2a2bc2d4-ab2c2d4-ac4d4+2bc4d4+a2b2d5+a2bcd5+2ab2cd5+4b3cd5-a2c2d5+abc2d5+bc3d5-c4d5-2a3d6-2ab2d6-2ac2d6+2a2d7-4abd7-b2d7+3acd7+3bcd7-4c2d7+3cd8-6d9\n\
12 ab3<1>  a2c5d2-abc5d2+b2c5d2+2bc6d2+c7d2-b3c3d3-a2c4d3-2b2c4d3-bc5d3-c6d3-2a2b3d4+a3bcd4-ab3cd4-2a3c2d4-2a2bc2d4-2ab2c2d4+abc3d4-2ac4d4+2bc4d4-a2b2d5+3ab3d5+2a2bcd5+5b3cd5-a2c2d5+abc2d5+2bc3d5-c4d5-2a3d6+a2bd6-2ab2d6-2b3d6+2a2cd6-3abcd6+2b2cd6-2ac2d6+bc2d6+2c3d6-abd7-3b2d7+3acd7-9c2d7+3cd8-6d9\n\
13 a4b<0>  a3bd5-ac3d5-b3d6-abcd6+2ad8\n\
14 a2b2c<1>  b2c6d2-bc7d2+abc5d3+b2c5d3-a3bc2d4+3ab2c3d4+a2c4d4+abc4d4+2b2c4d4-2bc5d4+c6d4-a2b3d5-2a3bcd5-ab3cd5-6ab2c2d5-3b3c2d5-abc3d5+2b2c3d5-2bc4d5+2a2b2d6-2ab3d6+2a3cd6+2ab2cd6-2abc2d6-4ac3d6+2a3d7-a2bd7-3ab2d7-5b3d7-7a2cd7+7abcd7-4b2cd7+2ac2d7-5bc2d7-c3d7+a2d8+b2d8-3acd8+c2d8+6ad9-3bd9+6cd9\n\
15 a2b3<1>  abc6d2+bc7d2-2b2c5d3-bc6d3-ab2c3d4+b3c3d4+a2c4d4+2abc4d4-b2c4d4+c6d4+4a2b3d5+a3bcd5+5ab3cd5+a3c2d5+2ab2c2d5+3b3c2d5-a2c3d5-abc3d5-b2c3d5+ac4d5+3bc4d5-c5d5+2a2b2d6-ab3d6-a2bcd6+2ab2cd6-b3cd6+2abc2d6+2ac3d6-bc3d6-2a3d7-3ab2d7+5b3d7-a2cd7+2abcd7-4b2cd7-2ac2d7-7c3d7+2a2d8-b2d8+3bcd8+5c2d8-4ad9+6bd9-3d10\n\
16 a5b<0>  a2b3d5+a2bc2d5+a2c3d5+ab3d6-b3cd6-bc3d6-2a2d8+3abd8\n\
17 abc4<1>  a2bc6d2-ab2c6d2+b2c7d2+2abc5d4+2b2c5d4-4bc6d4-5ab2c3d5-2b3c3d5-a2c4d5+b2c4d5+4bc5d5-c6d5-4ab3cd6+3a3c2d6+a2bc2d6+6ab2c2d6+3b3c2d6-3abc3d6+b2c3d6-4ac4d6+2bc4d6+c5d6-3a2b2d7+4ab3d7-a3cd7-3a2bcd7-8ab2cd7-5b3cd7-6a2c2d7+5abc2d7-4b2c2d7-3ac3d7-bc3d7-3c4d7-3a3d8+3a2bd8-ab2d8-5b3d8+6a2cd8-11abcd8+3b2cd8-3ac2d8+3c3d8+a2d9-2abd9-b2d9+8acd9-9bcd9+11c2d9+ad10-15cd10\n\
18 a2bc4<0>  a2c7d2+b2c7d2+c9d2-4ab2c3d5+2b3c3d5-3a2c4d5+4abc4d5-2b2c4d5+3bc5d5-3c6d5-2ab3cd6-a3c2d6+a2bc2d6+ab2c2d6+3b3c2d6-abc3d6+2b2c3d6-2ac4d6+3bc4d6+c5d6-3a2b2d7+4ab3d7-a3cd7-3a2bcd7-2ab2cd7+b3cd7-2a2c2d7+3abc2d7-3b2c2d7-5ac3d7+2bc3d7-2c4d7-3a3d8+3a2bd8-4ab2d8-7b3d8+5a2cd8-11abcd8+2b2cd8-3ac2d8+2c3d8+a2d9-2abd9-b2d9+2acd9-3bcd9-c2d9+2ad10-9cd10-3d11\n\
19 ab3c2<1>  abc7d2-2bc8d2-abc5d4+2b2c5d4-3bc6d4+2a3bc2d5-7ab2c3d5-4b3c3d5-2a2c4d5+abc4d5-2b2c4d5+3bc5d5-2c6d5-3ab3cd6+3a3c2d6-a2bc2d6+12ab2c2d6+b3c2d6+2a2c3d6-2b2c3d6+3ac4d6+6ab3d7+2a3cd7-6a2bcd7-4ab2cd7+2b3cd7-4a2c2d7+9abc2d7-2b2c2d7+ac3d7+2bc3d7+5c4d7+a2bd8+2b3d8+9a2cd8-14abcd8+9b2cd8+6bc2d8+6c3d8-2a2d9-2abd9+2b2d9+3acd9-9bcd9+8c2d9-ad10+3bd10-15cd10+3d11\n\
20 a2b3c<1>  bc8d2-bc7d3-2526abc5d4-b2c5d4-a3bc2d5+2531ab2c3d5+3b3c3d5+4b2c4d5-bc5d5+ab3cd6+a2bc2d6-6ab2c2d6-3b3c2d6-2528a2c3d6-2528abc3d6+2b2c3d6-2bc4d6-a2b2d7-2534ab3d7+a2bcd7+2530ab2cd7-2b3cd7+2529a2c2d7-6abc2d7+2527b2c2d7-5ac3d7-2bc3d7+2524c4d7+2a3d8-2ab2d8-7b3d8-5a2cd8+5abcd8-4b2cd8+2ac2d8-3bc2d8+c3d8+a2d9+abd9+2530b2d9-3acd9+6bcd9+2530c2d9+7ad10-12bd10+7cd10\n\
21 a5bc<0>  a2c4d5+abc4d5+b2c4d5-bc5d5+ab3cd6-ab2c2d6-b3c2d6+b2c3d6-ab3d7+a3cd7+ab2cd7-abc2d7-2ac3d7-3ab2d8-3b3d8-3a2cd8+2abcd8-b2cd8-c3d8+3ad10-3bd10+3cd10-3d11\n\
22 a2bc4<1>  abc8d2-2bc9d2+abc5d5-11b2c5d5-3bc6d5+12ab2c3d6+7b3c3d6-8abc4d6-11b2c4d6+2ac5d6+12bc5d6+7c6d6+12ab3cd7+8a3c2d7-9a2bc2d7+21ab2c2d7+19b3c2d7-8a2c3d7+3abc3d7-17b2c3d7+9ac4d7+16bc4d7+4c5d7+10a2b2d8-6ab3d8-14a3cd8-3a2bcd8-4ab2cd8+5b3cd8+7a2c2d8+9abc2d8+9b2c2d8+29ac3d8+bc3d8+4c4d8-4a3d9-20a2bd9+29ab2d9+63b3d9+23a2cd9-8abcd9-5ac2d9-4bc2d9-3c3d9+18a2d10-20abd10-3b2d10-11acd10+27bcd10+15c2d10-50ad11+48bd11-33cd11+3d12\n\
23 a4bc3<0>  ac6d5+bc6d5+b3c3d6-b2c4d6-a3c2d7-ab2c2d7+2a2c3d7+ac4d7+3a2b2d8+2ab3d8+3ab2cd8+2b3cd8-2ac3d8-a2d10-acd10-3c2d10+3ad11\n\
24 a5bc2<0>  abc5d5+3790bc6d5+3791c7d5-ab2c3d6-3790b2c4d6+3792bc5d6-3790a3c2d7-3790ab2c2d7-a2c3d7-abc3d7+3791ac4d7-bc4d7+3791a2b2d8+3789ab3d8-a2bcd8+3790ab2cd8+3788b3cd8-a2c2d8+3791b2c2d8+a3d9+ab2d9+ac2d9+2a2d10+3789abd10-3790b2d10+acd10+3790bcd10+6c2d10+3790bd11-3cd11+3d12\n\
25 a3bc4<1>  bc10d2+2540bc7d5+2521b2c5d6-2535bc6d6-846c7d6+2511ab2c3d7+2523b3c3d7+2509abc4d7-17b2c4d7+3ac5d7+852bc5d7-2525c6d7+3ab3cd8+2534a3c2d8-2536a2bc2d8+2570ab2c2d8+2569b3c2d8+1669a2c3d8-3371abc3d8-2554b2c3d8-3372ac4d8-1668bc4d8-3c5d8-3357a2b2d9-841ab3d9-2548a3cd9+856a2bcd9+1663ab2cd9+1675b3cd9+2518a2c2d9+2566abc2d9+845b2c2d9-2473ac3d9+2538bc3d9+1688c4d9-3392a3d10-2530a2bd10-792ab2d10+85b3d10+40a2cd10+2517abcd10+2536b2cd10-3395ac2d10+2528bc2d10-2559c3d10-1654a2d11+793abd11-855b2d11-830acd11-2537bcd11-819c2d11-75ad12-2444bd12-53cd12-2507d13\n\
26 a5bc3<0>  bc7d5-3033c8d5-b2c5d6+3033bc6d6-3034ab2c3d7+3034abc4d7+3032b2c4d7+3034ac5d7+3034bc5d7-1515ab3cd8-3035a2bc2d8+1518ab2c2d8-1513b3c2d8+3031a2c3d8+1516b2c3d8+1518bc4d8-1517c5d8-ab3d9-3034a3cd9-3034ab2cd9-3031b3cd9-3032abc2d9-3032ac3d9-3031bc3d9+3031a3d10-1515a2bd10+3033ab2d10+3035b3d10+3034a2cd10+3033abcd10-b2cd10+1514ac2d10+1517bc2d10-4c3d10+1521a2d11-1522abd11+3032b2d11-1516bcd11-3032c2d11-3036ad12-1513bd12-1519cd12+1516d13\n\
27 a5bc4<0>  c9d5-3033c8d6-b2c5d7+3030bc6d7+2528c7d7-3032ab2c3d8+3b3c3d8+3040abc4d8+3036b2c4d8+3034ac5d8+507bc5d8-c6d8-1516ab3cd9+3a3c2d9-3035a2bc2d9+1520ab2c2d9-1512b3c2d9-2025a2c3d9+2527abc3d9+1517b2c3d9+2528ac4d9-1007bc4d9-1517c5d9+2523a2b2d10+2525ab3d10-3034a3cd10-2528a2bcd10-511ab2cd10-505b3cd10-2a2c2d10-3029abc2d10-2529b2c2d10-3043ac3d10-3028bc3d10+2526c4d10-2025a3d11-1516a2bd11-2031ab2d11+3025b3d11+3033a2cd11+3024abcd11-3542ac2d11+1514bc2d11-c3d11-1006a2d12+3533abd12-2023b2d12+2529acd12-1515bcd12-495c2d12-3032ad13-1521bd13-1522cd13+1511d14\n\
";

const char* weispfennig97_syzygies_strat0_free5 =
  "  0: a2b2c  a3b2  a6b  \n  1: ab4  a3b2  ab3c3  a4bc4  \n";

const char* weispfennig97_initial_strat0_free5 =
  "  b4\n  a2b2c\n  a3b2\n  a4d2\n  b3c4\n  ab3c3\n  a2bc3d2\n  a3c3d2\n  ab3c2d2\n  a3bc4\n  a3bd5\n  a2c5d2\n  ab2c4d2\n  a2b3d5\n  b2c6d2\n  abc6d2\n  a2c4d5\n  bc8d2\n  ac6d5\n  abc5d5\n  bc7d5\n  c9d5\n";

const char* Gert93RawIdeal = 
  "3\n"
  "ab-b2-4bc+ae\n"
  "a2c-6bc2+a2f\n"
  "a3+b2c-a2d\n";


std::string gerdt93IdealComponentLast(bool componentsAscending, bool schreyer) {
  std::ostringstream out;
  out << "7583 6\n";
  if (schreyer)
    out << "schreyer ";
  out << "revlex 1\n";
  if (componentsAscending)
    out << " 1 1 1 1 1 1 _revlex component\n";
  else
    out << " 1 1 1 1 1 1 _revlex revcomponent\n";
  out << Gert93RawIdeal;
  return out.str();
}

std::string gerdt93IdealComponentFirst(bool componentsAscending) {
  std::ostringstream out;
  out << "7583 6 schreyer revlex 2\n";
  if (componentsAscending)
    out << " component\n 1 1 1 1 1 1\n";
  else
    out << " revcomponent\n 1 1 1 1 1 1\n";
  out << Gert93RawIdeal;
  return out.str();
}

std::string gerdt93IdealComponentMiddle(bool componentsAscending) {
  std::ostringstream out;
  out << "7583 6 schreyer revlex 2\n";
  if (componentsAscending)
    out << "1 1 1 1 1 1\n component\n";
  else
    out << "1 1 1 1 1 1\n revcomponent\n";
  out << Gert93RawIdeal;
  return out.str();
}

const char* gerdt93_gb_strat0_free1 = "\
0 <0>  ab-b2-4bc+ae\n\
1 <1>  a2c-6bc2+a2f\n\
2 <2>  a3+b2c-a2d\n\
3 a<1>  abc2+1264b2c2-bc2d+1264b2cf\n\
4 c2<0>  b2c2+2170bc3+3249bc2d+3249ac2e+3250b2cf\n\
5 ac<0>  b3c+3259bc3+1085bc2d-b2ce+1081ac2e-10bc2e+ace2+b3f+1091b2cf+16bc2f-b2ef-4acef-4bcef+ae2f\n\
6 a2<1>  b3c2-216b2c3-12b2c2d+36bc2d2-6ab2cf+b3cf+216b2c2f-6b2cdf-36a2bf2\n\
7 a2<0>  b4-2386bc3-b3d-8b2cd-3247bc2d-2b3e-6b2ce-3195ac2e+58bc2e+b2de+4acde+4bcde+a2e2+2b2e2-ace2+8bce2-ade2-2ae3-13b3f-3357b2cf-208bc2f+4a2ef+13b2ef+52acef+52bcef-13ae2f\n\
8 bc2<0>  bc4-563bc3d-2577bc2d2-1896ac3e-469bc3e-2229ac2de+3088bc2de+2013ac2e2-1829bc3f-3362b2cdf-2179bc2df+3088b2cef+2353ac2ef+3515bc2ef-1075ace2f-785b3f2+2930b2cf2+2606bc2f2-348a2ef2+785b2ef2+3140acef2+3140bcef2-785ae2f2\n\
9 ac2<0>  bc3e-3140ac2de-1718bc2de+2814ac2e2-1718b2cef+947bc2ef-3051ace2f+3140a2ef2\n\
10 ac3<0>  ac3de+1155ac2d2e-2873bc2d2e+3790ac3e2-1570ac2de2+3362bc2de2-3088ac2e3+2814b2cdef+2370bc2def+3362b2ce2f-3790ac2e2f-1659bc2e2f-3599acde2f+1133ace3f-1896b3ef2-1898b2cef2-10bc2ef2-1155a2def2+2681a2e2f2+1896b2e2f2+ace2f2+bce2f2-1896ae3f2+a2ef3\n\
";

const char* gerdt93_syzygies_strat0_free1 = "\
  0: a2c  a3  b2c2  abc2  \n\
  1: a3  \n\
";

const char* gerdt93_initial_strat0_free1 =
  "  ab\n  a2c\n  a3\n  b2c2\n  b3c\n  b4\n  bc3e\n  bc4\n  ac3de\n";

const char* gerdt93_gb_strat0_free2 = "\
0 <0>  ab-b2-4bc+ae\n\
1 <1>  a2c-6bc2+a2f\n\
2 <2>  a3+b2c-a2d\n\
3 b<1>  b3c+2b2c2+16bc3-b2ce-4ac2e-10bc2e+ace2+b3f+8b2cf+16bc2f-b2ef-4acef-4bcef+ae2f\n\
4 c<2>  b2c2+2170bc3+3249bc2d+3249ac2e+3250b2cf\n\
5 b<2>  b4-2386bc3-b3d-8b2cd-3247bc2d-2b3e-6b2ce-3195ac2e+58bc2e+b2de+4acde+4bcde+a2e2+2b2e2-ace2+8bce2-ade2-2ae3-13b3f-3357b2cf-208bc2f+4a2ef+13b2ef+52acef+52bcef-13ae2f\n\
6 bc<2>  bc4-563bc3d-2577bc2d2-1896ac3e-2681bc3e-2577ac2de-3362bc2de+3088ac2e2-1829bc3f-3362b2cdf-2179bc2df-3362b2cef+2353ac2ef+1659bc2ef-1133ace2f-785b3f2+2930b2cf2+2606bc2f2+785b2ef2+3140acef2+3140bcef2-785ae2f2\n\
7 ac<2>  bc3e-3140ac2de-1718bc2de+2814ac2e2-1718b2cef+947bc2ef-3051ace2f+3140a2ef2\n\
8 ac2<2>  ac3de+1155ac2d2e-2873bc2d2e+3790ac3e2-1570ac2de2+3362bc2de2-3088ac2e3+2814b2cdef+2370bc2def+3362b2ce2f-3790ac2e2f-1659bc2e2f-3599acde2f+1133ace3f-1896b3ef2-1898b2cef2-10bc2ef2-1155a2def2+2681a2e2f2+1896b2e2f2+ace2f2+bce2f2-1896ae3f2+a2ef3\n\
";

const char* gerdt93_syzygies_strat0_free2 = "\
  1: ab  \n\
  2: ab  b2c  a2c  \n\
";

const char* gerdt93_initial_strat0_free2 =
  "  ab\n  a2c\n  a3\n  b2c2\n  b3c\n  b4\n  bc3e\n  bc4\n  ac3de\n";

const char* gerdt93_gb_strat0_free3 = "\
0 <0>  ab-b2-4bc+ae\n\
1 <1>  a2c-6bc2+a2f\n\
2 <2>  a3+b2c-a2d\n\
3 a<1>  abc2+1264b2c2-bc2d+1264b2cf\n\
4 c2<0>  b2c2+2170bc3+3249bc2d+3249ac2e+3250b2cf\n\
5 ac<0>  b3c+3259bc3+1085bc2d-b2ce+1081ac2e-10bc2e+ace2+b3f+1091b2cf+16bc2f-b2ef-4acef-4bcef+ae2f\n\
6 a2<0>  b4-2386bc3-b3d-8b2cd-3247bc2d-2b3e-6b2ce-3195ac2e+58bc2e+b2de+4acde+4bcde+a2e2+2b2e2-ace2+8bce2-ade2-2ae3-13b3f-3357b2cf-208bc2f+4a2ef+13b2ef+52acef+52bcef-13ae2f\n\
7 a2<1>  b3c2-216b2c3-12b2c2d+36bc2d2-6ab2cf+b3cf+216b2c2f-6b2cdf-36a2bf2\n\
8 bc2<0>  bc4-563bc3d-2577bc2d2-1896ac3e-469bc3e-2229ac2de+3088bc2de+2013ac2e2-1829bc3f-3362b2cdf-2179bc2df+3088b2cef+2353ac2ef+3515bc2ef-1075ace2f-785b3f2+2930b2cf2+2606bc2f2-348a2ef2+785b2ef2+3140acef2+3140bcef2-785ae2f2\n\
9 ac2<0>  bc3e-3140ac2de-1718bc2de+2814ac2e2-1718b2cef+947bc2ef-3051ace2f+3140a2ef2\n\
10 ac3<0>  ac3de+1155ac2d2e-2873bc2d2e+3790ac3e2-1570ac2de2+3362bc2de2-3088ac2e3+2814b2cdef+2370bc2def+3362b2ce2f-3790ac2e2f-1659bc2e2f-3599acde2f+1133ace3f-1896b3ef2-1898b2cef2-10bc2ef2-1155a2def2+2681a2e2f2+1896b2e2f2+ace2f2+bce2f2-1896ae3f2+a2ef3\n\
";

const char* gerdt93_syzygies_strat0_free3 = "\
  0: a2c  a3  b2c2  abc2  \n\
  1: a3  \n\
";

const char* gerdt93_initial_strat0_free3 =
  "  ab\n  a2c\n  a3\n  b2c2\n  b3c\n  b4\n  bc3e\n  bc4\n  ac3de\n";

const char* gerdt93_gb_strat0_free4 = "\
0 <0>  ab-b2-4bc+ae\n\
1 <1>  a2c-6bc2+a2f\n\
2 <2>  a3+b2c-a2d\n\
3 b<1>  b3c+2b2c2+16bc3-b2ce-4ac2e-10bc2e+ace2+b3f+8b2cf+16bc2f-b2ef-4acef-4bcef+ae2f\n\
4 c<2>  b2c2+2170bc3+3249bc2d+3249ac2e+3250b2cf\n\
5 b<2>  b4-2386bc3-b3d-8b2cd-3247bc2d-2b3e-6b2ce-3195ac2e+58bc2e+b2de+4acde+4bcde+a2e2+2b2e2-ace2+8bce2-ade2-2ae3-13b3f-3357b2cf-208bc2f+4a2ef+13b2ef+52acef+52bcef-13ae2f\n\
6 bc<2>  bc4-563bc3d-2577bc2d2-1896ac3e-2681bc3e-2577ac2de-3362bc2de+3088ac2e2-1829bc3f-3362b2cdf-2179bc2df-3362b2cef+2353ac2ef+1659bc2ef-1133ace2f-785b3f2+2930b2cf2+2606bc2f2+785b2ef2+3140acef2+3140bcef2-785ae2f2\n\
7 ac<2>  bc3e-3140ac2de-1718bc2de+2814ac2e2-1718b2cef+947bc2ef-3051ace2f+3140a2ef2\n\
8 ac2<2>  ac3de+1155ac2d2e-2873bc2d2e+3790ac3e2-1570ac2de2+3362bc2de2-3088ac2e3+2814b2cdef+2370bc2def+3362b2ce2f-3790ac2e2f-1659bc2e2f-3599acde2f+1133ace3f-1896b3ef2-1898b2cef2-10bc2ef2-1155a2def2+2681a2e2f2+1896b2e2f2+ace2f2+bce2f2-1896ae3f2+a2ef3\n\
";

const char* gerdt93_syzygies_strat0_free4 = "\
  1: ab  \n\
  2: ab  b2c  a2c  \n\
";

const char* gerdt93_initial_strat0_free4 =
  "  ab\n  a2c\n  a3\n  b2c2\n  b3c\n  b4\n  bc3e\n  bc4\n  ac3de\n";

const char* gerdt93_gb_strat0_free5 = "\
0 <0>  ab-b2-4bc+ae\n\
1 <1>  a2c-6bc2+a2f\n\
2 <2>  a3+b2c-a2d\n\
3 ac<0>  b3c+2b2c2+16bc3-b2ce-4ac2e-10bc2e+ace2+b3f+8b2cf+16bc2f-b2ef-4acef-4bcef+ae2f\n\
4 a<1>  b2c2+2170bc3+3249bc2d+3249ac2e+3250b2cf\n\
5 a2<0>  b4-2386bc3-b3d-8b2cd-3247bc2d-2b3e-6b2ce-3195ac2e+58bc2e+b2de+4acde+4bcde+a2e2+2b2e2-ace2+8bce2-ade2-2ae3-13b3f-3357b2cf-208bc2f+4a2ef+13b2ef+52acef+52bcef-13ae2f\n\
6 ab<1>  bc4-563bc3d-2577bc2d2-1896ac3e-2681bc3e-2577ac2de-3362bc2de+3088ac2e2-1829bc3f-3362b2cdf-2179bc2df-3362b2cef+2353ac2ef+1659bc2ef-1133ace2f-785b3f2+2930b2cf2+2606bc2f2+785b2ef2+3140acef2+3140bcef2-785ae2f2\n\
7 a2<1>  bc3e-3140ac2de-1718bc2de+2814ac2e2-1718b2cef+947bc2ef-3051ace2f+3140a2ef2\n\
8 a2c<1>  ac3de+1155ac2d2e-2873bc2d2e+3790ac3e2-1570ac2de2+3362bc2de2-3088ac2e3+2814b2cdef+2370bc2def+3362b2ce2f-3790ac2e2f-1659bc2e2f-3599acde2f+1133ace3f-1896b3ef2-1898b2cef2-10bc2ef2-1155a2def2+2681a2e2f2+1896b2e2f2+ace2f2+bce2f2-1896ae3f2+a2ef3\n\
";

const char* gerdt93_syzygies_strat0_free5 = "\
  0: a2c  a3  \n\
  1: ab2  a2b  a3  \n\
";
const char* gerdt93_initial_strat0_free5 =
  "  ab\n  a2c\n  a3\n  b2c2\n  b3c\n  b4\n  bc3e\n  bc4\n  ac3de\n";

const char* gerdt93_gb_strat0_free6 = "0 <0>  ab-b2-4bc+ae\n1 <1>  a2c-6bc2+a2f\n2 <2>  a3+b2c-a2d\n3 b<1>  b3c+2b2c2+16bc3-b2ce-4ac2e-10bc2e+ace2+b3f+8b2cf+16bc2f-b2ef-4acef-4bcef+ae2f\n4 c<2>  b2c2+2170bc3+3249bc2d+3249ac2e+3250b2cf\n5 b<2>  b4-2386bc3-b3d-8b2cd-3247bc2d-2b3e-6b2ce-3195ac2e+58bc2e+b2de+4acde+4bcde+a2e2+2b2e2-ace2+8bce2-ade2-2ae3-13b3f-3357b2cf-208bc2f+4a2ef+13b2ef+52acef+52bcef-13ae2f\n6 bc<2>  bc4-563bc3d-2577bc2d2-1896ac3e-2681bc3e-2577ac2de-3362bc2de+3088ac2e2-1829bc3f-3362b2cdf-2179bc2df-3362b2cef+2353ac2ef+1659bc2ef-1133ace2f-785b3f2+2930b2cf2+2606bc2f2+785b2ef2+3140acef2+3140bcef2-785ae2f2\n7 ac<2>  bc3e-3140ac2de-1718bc2de+2814ac2e2-1718b2cef+947bc2ef-3051ace2f+3140a2ef2\n8 ac2<2>  ac3de+1155ac2d2e-2873bc2d2e+3790ac3e2-1570ac2de2+3362bc2de2-3088ac2e3+2814b2cdef+2370bc2def+3362b2ce2f-3790ac2e2f-1659bc2e2f-3599acde2f+1133ace3f-1896b3ef2-1898b2cef2-10bc2ef2-1155a2def2+2681a2e2f2+1896b2e2f2+ace2f2+bce2f2-1896ae3f2+a2ef3\n";
const char* gerdt93_syzygies_strat0_free6 =
  "  1: ab  \n  2: ab  b2c  a2c  \n";
const char* gerdt93_initial_strat0_free6 =
  "  ab\n  a2c\n  a3\n  b2c2\n  b3c\n  b4\n  bc3e\n  bc4\n  ac3de\n";

const char* gerdt93_gb_strat0_free7 = "\
0 <0>  ab-b2-4bc+ae\n\
1 <1>  a2c-6bc2+a2f\n\
2 <2>  a3+b2c-a2d\n\
3 a<1>  abc2+1264b2c2-bc2d+1264b2cf\n\
4 a2<1>  b3c2-216b2c3-12b2c2d+36bc2d2-6ab2cf+b3cf+216b2c2f-6b2cdf-36a2bf2\n\
5 c2<0>  b2c2+2170bc3+3249bc2d+3249ac2e+3250b2cf\n\
6 ac<0>  b3c+3259bc3+1085bc2d-b2ce+1081ac2e-10bc2e+ace2+b3f+1091b2cf+16bc2f-b2ef-4acef-4bcef+ae2f\n\
7 a2<0>  b4-2386bc3-b3d-8b2cd-3247bc2d-2b3e-6b2ce-3195ac2e+58bc2e+b2de+4acde+4bcde+a2e2+2b2e2-ace2+8bce2-ade2-2ae3-13b3f-3357b2cf-208bc2f+4a2ef+13b2ef+52acef+52bcef-13ae2f\n\
8 bc2<0>  bc4-563bc3d-2577bc2d2-1896ac3e-469bc3e-2229ac2de+3088bc2de+2013ac2e2-1829bc3f-3362b2cdf-2179bc2df+3088b2cef+2353ac2ef+3515bc2ef-1075ace2f-785b3f2+2930b2cf2+2606bc2f2-348a2ef2+785b2ef2+3140acef2+3140bcef2-785ae2f2\n\
9 ac2<0>  bc3e-3140ac2de-1718bc2de+2814ac2e2-1718b2cef+947bc2ef-3051ace2f+3140a2ef2\n\
10 ac3<0>  ac3de+1155ac2d2e-2873bc2d2e+3790ac3e2-1570ac2de2+3362bc2de2-3088ac2e3+2814b2cdef+2370bc2def+3362b2ce2f-3790ac2e2f-1659bc2e2f-3599acde2f+1133ace3f-1896b3ef2-1898b2cef2-10bc2ef2-1155a2def2+2681a2e2f2+1896b2e2f2+ace2f2+bce2f2-1896ae3f2+a2ef3\n\
";

const char* gerdt93_syzygies_strat0_free7 =
  "  0: a2c  a3  b2c2  abc2  \n  1: a3  \n";
const char* gerdt93_initial_strat0_free7 =
  "  ab\n  a2c\n  a3\n  b2c2\n  b3c\n  b4\n  bc3e\n  bc4\n  ac3de\n";
