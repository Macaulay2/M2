/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#include <M2/config.h>
#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif
#include "monomDRL.hpp"

namespace BIBasis
{
    int MonomDRL::Compare(const MonomDRL& anotherMonom)
    {
        if (TotalDegree < anotherMonom.TotalDegree)
        {
            return -1;
        }
        else if (TotalDegree > anotherMonom.TotalDegree)
        {
            return 1;
        }
        else
        {
            VarsListNode *iterator(ListHead),
                         *iteratorAnother(anotherMonom.ListHead);
            while (iterator)
            {
                if (iterator->Value < iteratorAnother->Value)
                {
                    return 1;
                }
                if (iterator->Value > iteratorAnother->Value)
                {
                    return -1;
                }
                iterator = iterator->Next;
                iteratorAnother = iteratorAnother->Next;
            }
            return 0;
        }
    }

    FastAllocator MonomDRL::Allocator(sizeof(MonomDRL));
}
