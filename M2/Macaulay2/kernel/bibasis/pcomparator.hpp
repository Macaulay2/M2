/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#ifndef BIBASIS_PCOMPARATOR_HPP
#define BIBASIS_PCOMPARATOR_HPP

#include <string>
#include "error.h"

namespace BIBasis
{
    template<class T> class PointerLessComparator
    {
    public:
        bool operator() (const T* const& p1, const T* const& p2) 
        {
            if (!p1 || !p2)
            {
                throw std::string("BIBasis::PointerLessComparator::operator(): at least one argument is NULL.");
            }
            return *p1 < *p2;
        }
    };

    template<class T> class PointerMoreComparator
    {
    public:
        bool operator() (const T* const& p1, const T* const& p2) 
        {
            if (!p1 || !p2)
            {
                throw std::string("BIBasis::PointerMoreComparator::operator(): at least one argument is NULL.");
            }
            return *p1 > *p2;
        }
    };
}

#endif //BIBASIS_PCONPARATOR_HPP
