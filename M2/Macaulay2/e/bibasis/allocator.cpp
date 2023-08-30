/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#include <cstdlib>
#include <cmath>
#include <string>

#include "allocator.hpp"
#include "error.h"

#define int_divide_ceil(x,y) (((x)+(y)-1)/(y))
#define int_divide_floor(x,y) ((x)/(y))

namespace BIBasis
{
    FastAllocator::FastAllocator(size_t blockSize)
        : MemoryPageSize(1048576)
        , TSize(int_divide_ceil(blockSize,sizeof(void*)))
        , PageSize(int_divide_floor(MemoryPageSize,blockSize))
        , FreeBlock(nullptr) 
    {
    }

    FastAllocator::~FastAllocator()
    {
    }

    void FastAllocator::ExpandMemory()
    {
        void **begin = static_cast<void**>(malloc(MemoryPageSize)),
             **end = begin + TSize * (PageSize - 1),
             **tmp;
        
        if (!begin)
        {
            throw std::string("BIB::FastAllocator::ExpandMemory(): failed to allocate memory.");
        }

        *(reinterpret_cast<void***>(end)) = FreeBlock;
        FreeBlock = static_cast<void**>(begin);
        do 
        {
            tmp = begin + TSize;
            *(reinterpret_cast<void***>(begin)) = tmp;
            begin = tmp;
        } while(begin < end);
    }
}
