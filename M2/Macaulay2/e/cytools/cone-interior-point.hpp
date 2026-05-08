#pragma once
#include <vector>

struct ConeResult {
    bool fullDimensional;
    double tStar;
    std::vector<double> interiorPoint;  // valid if fullDimensional
    std::vector<double> dualCert;       // valid if !fullDimensional: y >= 0, y^T A = 0
};

ConeResult coneInteriorPoint(int m, int n, const std::vector<int>& A);
