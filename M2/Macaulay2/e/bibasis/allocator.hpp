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

/**
 * @file bibasis/allocator.hpp
 * @brief `BIBasis::FastAllocator` --- per-size-class slab allocator for BIBasis's small objects.
 *
 * Declares the slab pool that supplies storage for the millions
 * of tiny `Monom`, `Polynom`, and `Triple` records the involutive-
 * basis algorithm churns through. Each `FastAllocator` instance
 * is bound to a single fixed `blockSize` at construction and
 * maintains a free-list of that-sized chunks carved out of large
 * memory pages; `Allocate()` pops the head of the free-list (and
 * calls `ExpandMemory()` to slab in a new page if the list is
 * empty), `Free()` pushes back, both inlined for the inner loop.
 *
 * Memory is never returned to the OS --- the slabs persist for
 * the life of the allocator and are reclaimed only on process
 * exit. The subsystem deliberately bypasses M2's Boehm GC and the
 * engine's `our_new_delete` pattern; BIBasis classes opt in by
 * overloading `operator new` / `operator delete` to route through
 * a static `FastAllocator` for their size class.
 *
 * @see bibasis.cpp
 * @see polynom.hpp
 * @see monom.hpp
 */

namespace BIBasis
{
    /**
     * @brief Slab allocator handing out fixed-size blocks for one BIBasis
     * type per instance.
     *
     * @details Allocates `MemoryPageSize`-byte pages from the system heap
     * and carves each page into `TSize`-byte cells (typically the
     * size of one `VarsListNode` or `MonomDL` etc.). Free cells are
     * threaded into a `FreeBlock` LIFO so `Allocate()` /
     * `Free(p)` are O(1) and avoid hitting `malloc` on the BIBasis
     * inner loop. `ExpandMemory()` grabs a fresh page when the
     * free list is empty.
     */
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