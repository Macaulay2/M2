/*****************************************************************************
 *   Copyright (C) 2011 by Mikhail V. Zinin                                  *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#include "launcher.hpp"
#include "matrix.hpp"

extern "C" const Matrix* rawBIBasis(const Matrix* matrix, int toGroebner)
{
    BIBasis::Launcher launcher;
    
    return launcher.GetBIBasisMatrix(matrix, toGroebner);
}
