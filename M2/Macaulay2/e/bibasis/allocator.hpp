/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#ifndef BIBASIS_FAST_ALLOCATOR_HPP
#define BIBASIS_FAST_ALLOCATOR_HPP

namespace BIBasis
{
    class FastAllocator
    {
    private:
        const size_t MemoryPageSize;
        const size_t TSize;
        const size_t PageSize;
        void**       FreeBlock;

    public:
        FastAllocator(const size_t blockSize);
        ~FastAllocator();

        void* Allocate();
        void Free(void* pointer);

    private:
        void ExpandMemory();
    };

    inline void* FastAllocator::Allocate()
    {
        if (!FreeBlock)
        {
            ExpandMemory();
        }
    
        void* pointer = static_cast<void*>(FreeBlock);
        FreeBlock = static_cast<void**>(*FreeBlock);
        return pointer;
    }

    inline void FastAllocator::Free(void* pointer)
    {
        *(static_cast<void***>(pointer)) = FreeBlock;
        FreeBlock = static_cast<void**>(pointer);
    }
}

#endif // BIBASIS_FAST_ALLOCATOR_HPP