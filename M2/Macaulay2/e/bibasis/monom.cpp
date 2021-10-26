/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#include "monom.hpp"

namespace BIBasis
{
    void Monom::SetDimIndepend(Monom::Integer independ)
    {
        Monom::DimIndepend = independ;
    }

    Monom::Integer Monom::DimIndepend = 0;
    FastAllocator Monom::VarsListNode::Allocator(sizeof(Monom::VarsListNode));
}
