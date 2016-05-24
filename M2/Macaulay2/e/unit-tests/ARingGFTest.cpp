// Copyright 2013 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "debug.hpp"
#include "aring-gf-givaro.hpp"
#include "ARingTest.hpp"

static const int nelements = 200;
static int randomVals[nelements] = {
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

#if 1
template<>
void getElement<M2::ARingGFGivaro>(const M2::ARingGFGivaro& R, 
                             int index, 
                             M2::ARingGFGivaro::ElementType& result)
{
  M2::ARingGFGivaro::ElementType gen;
  R.init(gen);
  R.getGenerator(gen);
  if (index >= nelements) 
    R.power(result, gen, rawRandomInt(static_cast<int32_t>(R.cardinality())));
  else 
    R.power(result, gen, randomVals[index]);
  R.clear(gen);
}

  TEST(ARingGFGivaroGivaro, create) {
    M2::ARingGFGivaro R(5,3);
      
    EXPECT_EQ(ringName(R), "GF(5,3,Givaro)");
    EXPECT_EQ(R.cardinality(), 125);
    EXPECT_EQ(R.characteristic(), 5);

    M2_arrayint gen_modpoly = R.getModPolynomialCoeffs();
    std::cout << "minimal polynomial = ";
    dintarray(gen_modpoly);
    std::cout << std::endl;

    M2_arrayint gen_coeffs = R.getGeneratorCoeffs();
    std::cout << "generator polynomial = ";
    dintarray(gen_coeffs);
    std::cout << std::endl;

    // Check what values integers go to
    M2::ARingGFGivaro::ElementType a;
    R.init(a);
    for (int i=-5; i<R.characteristic(); i++)
      {
        R.set_from_long(a, i);
        M2_arrayint coeffs = R.fieldElementToM2Array(a);
        if (i >= 0) EXPECT_EQ(coeffs->array[0], i);
        EXPECT_EQ(coeffs->len, 3);
        for (int j=1; j<3; j++)
          EXPECT_EQ(coeffs->array[j], 0);
        std::cout << i << " = ";
        dintarray(coeffs);
        std::cout << std::endl;
      }
    R.clear(a);

    testSomeMore(R);
  }

  TEST(ARingGFGivaroGivaro, random) {
    M2::ARingGFGivaro R(7,2);
    M2::ARingGFGivaro::ElementType a;
    R.init(a);
    int counts[49];
    for (int i=0; i<49; i++)
      counts[i] = 0;
    for (int i=0; i<49*200; i++)
      {
        R.random(a);
        EXPECT_TRUE(a >= 0);
        EXPECT_TRUE(a < 49);
        counts[a]++;
      }
#if 0
    double sum = 0;
    for (int i=0; i<49; i++)
      {
        std::cout << i << "  " << counts[i] << std::endl;
        sum += (counts[i]-200)^2;
      }

    sum /= (49*200);
    std::cout << "chi = " << sum << std::endl;
    for (int i=0; i<49; i++)
      {
        a = i;
        M2_arrayint f = R.fieldElementToM2Array(a);
        dintarray(f);
        std::cout << std::endl;
      }
#endif
    R.clear(a);
  }

  TEST(ARingGFGivaroGivaro, arithmetic) {
    M2::ARingGFGivaro R(5,3);
    testFiniteField(R, ntrials);
  }

#endif 

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
