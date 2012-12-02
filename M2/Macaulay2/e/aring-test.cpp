// Tests for the code in

#include "aring-zz.hpp"
using namespace M2;

void test_zzp()
{
  RingZZp R(101);
  RingZZp::ElementType a,b;
  R.init_set_int(a, 5);
  R.init_set_int(b, 102);
  R.add_to(a,b);
  R.display(std::out, a);
}

int main(int argc, char **argv)
{
  test_zzp();
  return 0;
}
