// Copyright 2011 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>
#include <mpfr.h>

bool testfcn() { return true; }

TEST(Nothing, ideal) {
    EXPECT_EQ(true, testfcn());
    EXPECT_FALSE(!(testfcn()));
}
