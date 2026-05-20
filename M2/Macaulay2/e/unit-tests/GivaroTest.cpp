// Copyright (c) 994-2009 by The Givaro group
// This file is part of Givaro.
// Givaro is governed by the CeCILL-B license under French law
// and abiding by the rules of distribution of free software.
// see the COPYRIGHT file for more details.
#include <iostream>
#include <givaro/modular.h>
#include <givaro/montgomery.h>
#include <givaro/gfq.h>
#include <givaro/gfqext.h>
using namespace Givaro;
template <class Field>
void TestField(const Field& F)
{
  std::cerr << "Within ";
  F.write(std::cerr);
  std::cerr << " : " << std::flush;
  typename Field::Element a, b, c, d;
  F.init(a, 7U);
  F.init(b, -29.3);
  F.init(c);       // empty constructor
  F.init(d);       // empty constructor
  F.add(c, a, b);  // c = a+b
  // Separate output writing
  F.write(std::cout, a) << " + " << std::flush;
  F.write(std::cout, b) << " = " << std::flush;
  F.write(std::cerr, c) << std::endl;
  F.mul(c, a, b);      // c = a*b
  F.axpy(d, a, b, c);  // d = a*b + c;
  // Writing all outputs in a single command line
  F.write(std::cerr << "Within ") << " : " << std::flush;
  F.write(F.write(F.write(F.write(std::cout, c) << " + ", a) << " * ", b)
              << " = ",
          d)
      << std::endl;
  {
    typename Field::Element e;
    F.init(e);
    F.assign(e, d);
    F.maxpy(e, a, b, d);  // e = d-a*b
    // Writing all outputs in a single command line
    F.write(std::cerr << "Within ") << " : " << std::flush;
    F.write(F.write(F.write(F.write(std::cout, d) << " - ", a) << " * ", b)
                << " = ",
            e)
        << std::endl;
  }
  {
    typename Field::Element e;
    F.init(e);
    F.assign(e, d);
    F.maxpyin(e, a, b);  // e = d - a*b;
    // Writing all outputs in a single command line
    F.write(std::cerr << "Within ") << " : " << std::flush;
    F.write(F.write(F.write(F.write(std::cout, d) << " - ", a) << " * ", b)
                << " = ",
            e)
        << std::endl;
  }
  {
    typename Field::Element e;
    F.init(e);
    F.assign(e, d);
    F.axmy(e, a, b, d);  // e = a*b -d;
    // Writing all outputs in a single command line
    F.write(std::cerr << "Within ") << " : " << std::flush;
    F.write(F.write(F.write(F.write(std::cout, a) << " * ", b) << " - ", d)
                << " = ",
            e)
        << std::endl;
  }
  {
    typename Field::Element e;
    F.init(e);
    F.assign(e, d);
    F.maxpyin(e, a, b);  // e = d - a*b;
    // Writing all outputs in a single command line
    F.write(std::cerr << "Within ") << " : " << std::flush;
    F.write(F.write(F.write(F.write(std::cout, d) << " - ", a) << " * ", b)
                << " = ",
            e)
        << std::endl;
  }
  // Four operations
  F.write(F.write(std::cout, a) << " += ", b) << " is ";
  F.write(std::cout, F.addin(a, b)) << "   ;   ";
  F.write(F.write(std::cout, a) << " -= ", b) << " is ";
  F.write(std::cout, F.subin(a, b)) << "   ;   ";
  F.write(F.write(std::cout, a) << " *= ", b) << " is ";
  F.write(std::cout, F.mulin(a, b)) << "   ;   ";
  F.write(F.write(std::cout, a) << " /= ", b) << " is ";
  F.write(std::cout, F.divin(a, b)) << std::endl;
  F.init(a, 22996);
  F.inv(b, a);
  F.write(F.write(std::cout << "1/", a) << " is ", b) << std::endl;
  F.mul(c, b, a);
  F.write(std::cout << "1 is ", c) << std::endl;
  F.init(a, 22996);
  F.init(b, 22996);
  F.write(std::cout << "1/", a) << " is ";
  F.invin(a);
  F.write(std::cout, a) << std::endl;
  F.mulin(a, b);
  F.write(std::cout << "1 is ", a) << std::endl;
  F.init(a, 37403);
  F.inv(b, a);
  F.write(F.write(std::cout << "1/", a) << " is ", b) << std::endl;
  F.mul(c, b, a);
  F.write(std::cout << "1 is ", c) << std::endl;
  F.init(a, 37403);
  F.init(b, 37403);
  F.write(std::cout << "1/", a) << " is ";
  F.invin(a);
  F.write(std::cout, a) << std::endl;
  F.mulin(a, b);
  F.write(std::cout << "1 is ", a) << std::endl;
}
extern "C" {
#include <sys/time.h>
#include <sys/resource.h>
}
int main(int argc, char** argv)
{
  // modulo 13 over 16 bits
  Modular<int16_t> C13(13);
  TestField(C13);
  // modulo 13 over 32 bits
  Modular<int32_t> Z13(13);
  TestField(Z13);
  // modulo 13 over unsigned 32 bits
  Modular<uint32_t> U13(13);
  TestField(U13);
#ifdef __USE_Givaro_SIXTYFOUR__
  // modulo 13 over 64 bits
  Modular<int64_t> LL13(13U);
  TestField(LL13);
#endif
  // modulo 13 fully tabulated
  Modular<Log16> L13(13);
  TestField(L13);
  // modulo 13 over 32 bits with Montgomery reduction
  Montgomery<int32_t> M13(13);
  TestField(M13);
  Montgomery<int32_t> M3(39989);
  TestField(M3);
  // modulo 13 with primitive root representation
  GFqDom<int> GF13(13);
  TestField(GF13);
  // modulo 13 over arbitrary size
  Modular<Integer> IntZ13(13);
  TestField(IntZ13);
  // Zech log finite field with 5^4 elements
  GFqDom<int> GF625(5, 4);
  TestField(GF625);
#if 0 // Possibly related: https://github.com/cr-marcstevens/m4gb/issues/8
  // Zech log finite field with 256 elements
  // and prescribed irreducible polynomial
  std::vector<GFqDom<unsigned long long>::Residu_t> Irred(9);
  Irred[0] = 1;
  Irred[1] = 1;
  Irred[2] = 0;
  Irred[3] = 1;
  Irred[4] = 1;
  Irred[5] = 0;
  Irred[6] = 0;
  Irred[7] = 0;
  Irred[8] = 1;
  GFqDom<unsigned long long> F256(2, 8, Irred);
  TestField(F256);
#endif
  // Zech log finite field with 3^4 elements
  // Using the Q-adic Transform
  GFqExt<int32_t> GF81(3, 4);
  TestField(GF81);
  // Zech log finite field with 2Mb tables
  struct rusage tmp1;
  getrusage(RUSAGE_SELF, &tmp1);
  // user time
  double tim = (double)tmp1.ru_utime.tv_sec +
               ((double)tmp1.ru_utime.tv_usec) / (1000000.0);
  ;
  getrusage(RUSAGE_SELF, &tmp1);
  tim = (double)tmp1.ru_utime.tv_sec +
        ((double)tmp1.ru_utime.tv_usec) / (1000000.0) - tim;
  std::cerr << "Initialization took " << tim
            << " cpu seconds and : " << std::endl;
  std::cerr << tmp1.ru_maxrss << " maximum resident set size" << std::endl
            << tmp1.ru_ixrss << " integral shared memory size" << std::endl
            << tmp1.ru_idrss << " integral unshared data size" << std::endl
            << tmp1.ru_isrss << " integral unshared stack size" << std::endl
            << tmp1.ru_minflt << " page reclaims" << std::endl
            << tmp1.ru_majflt << " page faults" << std::endl
            << tmp1.ru_nswap << " swaps" << std::endl
            << tmp1.ru_inblock << " block input operations" << std::endl
            << tmp1.ru_oublock << " block output operations" << std::endl;
  return 0;
}
