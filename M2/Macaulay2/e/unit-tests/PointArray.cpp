// Copyright 2016 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

#include "NAG.hpp"

TEST(PointArray, constructor)
{
  PointArray p(0.001,{0.3,0.7});
}
