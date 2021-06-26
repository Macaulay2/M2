#include "divsim/stdinc.h"
#include "mathic/KDTree.h"
#include <gtest/gtest.h>

#include "mathic/DivList.h"
#include "divsim/KDTreeModel.h"

TEST(DivFinder, NoOp) {
  KDTreeModel<1,1,1,1,1> model(1, 1, 0, 0, 1.0, 1000);
};
