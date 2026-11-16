// Copyright 2026.  The Macaulay2 authors.
#include "cone-interior-point.hpp"
#include <glpk.h>
#include <cmath>

// A is m x n, row-major.  Tests whether {x : Ax >= 0} is full-dimensional.
ConeResult coneInteriorPoint(int m, int n, const std::vector<int>& A) {
    glp_prob* lp = glp_create_prob();
    glp_set_obj_dir(lp, GLP_MAX);

    // Rows: A_i x - t >= 0
    glp_add_rows(lp, m);
    for (int i = 1; i <= m; ++i)
        glp_set_row_bnds(lp, i, GLP_LO, 0.0, 0.0);

    // Cols: x_1..x_n bounded in [-1, 1], then t >= 0
    glp_add_cols(lp, n + 1);
    for (int j = 1; j <= n; ++j) {
        glp_set_col_bnds(lp, j, GLP_DB, -1.0, 1.0);
        glp_set_obj_coef(lp, j, 0.0);
    }
    glp_set_col_bnds(lp, n + 1, GLP_LO, 0.0, 0.0);
    glp_set_obj_coef(lp, n + 1, 1.0);  // maximize t

    // Build sparse matrix in GLPK's 1-indexed triplet form
    std::vector<int> ia(1, 0), ja(1, 0);
    std::vector<double> ar(1, 0.0);
    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            int aij = A[i * n + j];
            if (aij != 0) {
                ia.push_back(i + 1);
                ja.push_back(j + 1);
                ar.push_back(static_cast<double>(aij));
            }
        }
        // -1 coefficient on t
        ia.push_back(i + 1);
        ja.push_back(n + 1);
        ar.push_back(-1.0);
    }
    glp_load_matrix(lp, static_cast<int>(ia.size()) - 1,
                    ia.data(), ja.data(), ar.data());

    glp_smcp params;
    glp_init_smcp(&params);
    params.msg_lev = GLP_MSG_OFF;
    glp_simplex(lp, &params);

    ConeResult res;
    res.tStar = glp_get_col_prim(lp, n + 1);

    const double tol = 1e-8;
    if (res.tStar > tol) {
        res.fullDimensional = true;
        res.interiorPoint.resize(n);
        for (int j = 0; j < n; ++j)
            res.interiorPoint[j] = glp_get_col_prim(lp, j + 1);
    } else {
        res.fullDimensional = false;
        res.dualCert.resize(m);
        for (int i = 0; i < m; ++i)
            // GLPK row duals for >= constraints in a max problem may come
            // out negative depending on convention; take |.| so y >= 0.
            res.dualCert[i] = std::abs(glp_get_row_dual(lp, i + 1));
    }
    glp_delete_prob(lp);
    return res;
}

// int main() {
//     // Example 1: full-dimensional in R^2
//     // A = [[1,0],[0,1],[1,1]]
//     {
//         std::vector<int> A = {1,0,  0,1,  1,1};
//         auto r = coneInteriorPoint(3, 2, A);
//         std::cout << "Example 1: t* = " << r.tStar << "\n";
//         if (r.fullDimensional) {
//             std::cout << "  interior: ";
//             for (double v : r.interiorPoint) std::cout << v << " ";
//             std::cout << "\n";
//         }
//     }
//     // Example 2: NOT full-dimensional (forces x1 = 0)
//     // A = [[1,0],[-1,0],[0,1]]
//     {
//         std::vector<int> A = {1,0,  -1,0,  0,1};
//         auto r = coneInteriorPoint(3, 2, A);
//         std::cout << "Example 2: t* = " << r.tStar << "\n";
//         if (!r.fullDimensional) {
//             std::cout << "  dual cert y: ";
//             for (double v : r.dualCert) std::cout << v << " ";
//             std::cout << "\n  (expect y_1 = y_2 > 0, y_3 = 0; check y^T A = 0)\n";
//         }
//     }
// }
// g++ -O2 -o cone cone-interior-point.cpp -lglpk -I`brew --prefix glpk`/include
// -L`brew --prefix glpk`/lib suggested by claude.
