#include "EZZp.hpp"

EZZ *EZZ::_ZZ = 0;
void EZZp::gcd_extended(int a, int b, int &u, int &v, int &g) const
{
 int q ;
 int u1, v1, g1;
 int utemp, vtemp, gtemp;
 
 g1 = b;     u1 = 0;         v1 = 1;
 g  = a;     u  = 1;         v  = 0;
 while (g1 != 0)
   {
     q = g / g1 ;
     gtemp = g - q * g1;
     utemp = u - q * u1;
     vtemp = v - q * v1;
     g  = g1;    u  = u1;     v  = v1 ;
     g1 = gtemp; u1 = utemp;  v1 = vtemp;
   }
}
