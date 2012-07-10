/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#ifndef BIBASIS_LAUNCHER_HPP
#define BIBASIS_LAUNCHER_HPP

#include "involutive.hpp"
#include "matrix.hpp"

namespace BIBasis
{
    class Launcher
    {
    public:
        const M2_Matrix* GetBIBasisMatrix(const M2_Matrix* matrix, int toGroebner) const;
    
    private:
        bool CheckMatrix(const M2_Matrix* matrix) const;
    };
}

#endif // BIBASIS_LAUNCHER_HPP
