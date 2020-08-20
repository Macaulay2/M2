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
#include "monomLex.hpp"

namespace BIBasis
{
    int MonomLex::Compare(const MonomLex& anotherMonom)
    {
        VarsListNode *iterator = ListHead,
                     *iteratorAnother = anotherMonom.ListHead;
        while (iterator && iteratorAnother)
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
        
        if (iterator)
        {
            return 1;
        }
        else if (iteratorAnother)
        {
            return -1;
        }
        else
        {
            return 0;
        }
    }

    FastAllocator MonomLex::Allocator(sizeof(MonomLex));
}
