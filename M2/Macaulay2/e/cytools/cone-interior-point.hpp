// Copyright 2026.  The Macaulay2 authors.

#ifndef _cone_interior_point_hpp_
#define _cone_interior_point_hpp_

#include <vector>

struct ConeResult {
    bool fullDimensional;
    double tStar;
    std::vector<double> interiorPoint;  // valid if fullDimensional
    std::vector<double> dualCert;       // valid if !fullDimensional: y >= 0, y^T A = 0
};

ConeResult coneInteriorPoint(int m, int n, const std::vector<int>& A);

#endif // _cone_interior_point_hpp_
