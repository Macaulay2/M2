// Copyright 2012-2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "ZZp.hpp"
#include "aring-ffpack.hpp"
#include "aring-zzp.hpp"
#include "ARingTest.hpp"

const int nelements = 200;
int randomVals[nelements] = {
  2666036, 85344567, 71531106, 8755168, 53852118, 88167705, 22475268, 41297550, 91010248, 44033907, 
  33751711, 95042283, 701143, 62443598, 55048281, 17723663, 85240587, 25979106, 47819861, 90244528, 
  11275250, 22798387, 68996632, 76613634, 48760025, 24733508, 27522895, 56224559, 66749385, 49236242, 
  42314935, 56483458, 17870101, 84017847, 4627176, 94565033, 93275532, 76351828, 90611837, 77643518, 
  59276468, 26344784, 73201185, 91083238, 85501801, 85345398, 83160965, 89062810, 37815670, 8227378, 
  19118805, 63640713, 84232050, 24326912, 73922619, 70743421, 19090900, 59358457, 17634169, 86322817, 
  37134114, 97886677, 82613353, 13603908, 61119326, 40173934, 24107001, 90616769, 72088106, 47299394, 
  97111628, 22494627, 6547534, 68935260, 57921076, 3521860, 45351552, 87950309, 87755396, 43573207, 
  77361055, 97023982, 36678347, 66937610, 3708760, 36009386, 84861416, 84983584, 99073162, 25592499, 
  55121036, 217404, 45246974, 87694278, 18631417, 97176622, 66230759, 70817866, 48984802, 99776399, 
  11107075, 83804819, 1210456, 72894434, 36929177, 57482178, 26142753, 88954986, 53609557, 80607781, 
  98680672, 2989714, 89692551, 96341984, 70402005, 50025094, 3450389, 24030230, 94134112, 85324655, 
  35404622, 74519773, 85307471, 87613072, 80434774, 55494037, 34945135, 36300411, 44911039, 42734386, 
  69273889, 6537915, 18710819, 33093559, 68308135, 29544635, 65475048, 51852879, 85499965, 65398813, 
  61670710, 98961531, 70981648, 24941547, 8504747, 80452330, 45113018, 60607115, 65165225, 73257098, 
  2992669, 51802181, 65378474, 2825238, 78916325, 8474068, 2695455, 53942610, 94297201, 37662100, 
  45567374, 30141840, 32491957, 91837138, 28261048, 71359280, 11933270, 16587656, 64093661, 12235770, 
  16195573, 99396594, 35549941, 98074540, 45023021, 15205552, 3304979, 34666480, 89209262, 10261916, 
  35340937, 98935118, 77343644, 78522496, 46395773, 35429063, 54767177, 14130046, 2726640, 44257782, 
  31615869, 83095327, 15062803, 92772905, 25189126, 86464567, 43372313, 24240507, 96790882, 99639739};

#if defined(HAVE_FFLAS_FFPACK)
template<>
bool getElement<M2::ARingZZpFFPACK>(const M2::ARingZZpFFPACK& R, int index, M2::ARingZZpFFPACK::ElementType& result)
{
  if (index >= nelements) return false;
  //  int idx = index % R.cardinality();
  R.power(result, R.getGenerator(), randomVals[index]);
  return true;
}
#endif

template<>
bool getElement<M2::ARingZZp>(const M2::ARingZZp& R, int index, M2::ARingZZp::ElementType& result)
{
  if (index >= nelements) return false;
  R.set_var(result, 0);
  //  std::cout << "generator = " << result << std::endl;
  R.power(result, result, randomVals[index]);
  //std::cout << "gen^" << randomVals[index] << " = " << result << std::endl;
  return true;
}

TEST(RingZZp, create) {
  const Z_mod *R = Z_mod::create(101);
  EXPECT_FALSE(R == 0);
  buffer o;
  o << "Ring being tested: ";
  R->text_out(o);
  fprintf(stdout, "%s\n", o.str());
}

#if defined(HAVE_FFLAS_FFPACK)
TEST(ARingZZpFFPACK, create) {
  M2::ARingZZpFFPACK R(101);
  
  EXPECT_EQ(ringName(R), "ZZpFPACK(101,1)");
  testSomeMore(R);

  std::cout << "max modulus for ffpack zzp: " << M2::ARingZZpFFPACK::getMaxModulus() << std::endl;
}

TEST(ARingZZpFFPACK, arithmetic) {
  M2::ARingZZpFFPACK R(101);
  testFiniteField(R);
}
#endif 

TEST(ARingZZp, create) {
  M2::ARingZZp R(101);
  M2::ARingZZp::ElementType a;
  buffer o;

  EXPECT_EQ(ringName(R), "AZZ/101");
  EXPECT_EQ(R.cardinality(), 101);
  EXPECT_EQ(R.characteristic(), 101);
  // Now check what the generator is, as an integer
  R.init(a);
  R.set_var(a, 0);
  R.elem_text_out(o, a, true, true, false);
  std::cout << "generator is " << o.str() << std::endl;
  R.clear(a);

}

TEST(ARingZZp, arithmetic101) {
  M2::ARingZZp R(101);
  testFiniteField(R);
}

TEST(ARingZZp, arithmetic2) {
  M2::ARingZZp R(2);
  testFiniteField(R);
}

TEST(ARingZZp, arithmetic3) {
  M2::ARingZZp R(3);
  testFiniteField(R);
}

#if defined(HAVE_FFLAS_FFPACK)
TEST(ARingZZpFFPACK, arithmetic101) {
  M2::ARingZZpFFPACK R(101);
  testFiniteField(R);
}

//TODO: commented out because it appears wrong.  Perhaps p=2 isn't allowed here?
//TEST(ARingZZpFFPACK, arithmetic2) {
//  M2::ARingZZpFFPACK R(2);
//  testFiniteField(R);
//}

TEST(ARingZZpFFPACK, arithmetic3) {
  M2::ARingZZpFFPACK R(3);
  testFiniteField(R);
}

//TODO: commented out because it takes too long:
//TEST(ARingZZpFFPACK, arithmetic67108879) {
//  M2::ARingZZpFFPACK R(67108879);
//  testFiniteField(R);
//}
#endif 

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
