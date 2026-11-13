/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#ifndef M2_BIBASIS_LAUNCHER_HPP
#define M2_BIBASIS_LAUNCHER_HPP

#include "bibasis/involutive.hpp"
#include "matrices/matrix.hpp"

namespace BIBasis
{
    class Launcher
    {
    public:
        const Matrix* GetBIBasisMatrix(const Matrix* matrix, int toGroebner) const;
    
    private:
        bool CheckMatrix(const Matrix* matrix) const;
    };
}

#endif // BIBASIS_LAUNCHER_HPP
