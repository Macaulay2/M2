/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#ifndef BIBASIS_MONOM_HPP
#define BIBASIS_MONOM_HPP

/**
 * @file bibasis/monom.hpp
 * @brief `BIBasis::Monom` --- abstract squarefree-monomial base for the three Janet orderings.
 *
 * Declares the polymorphic root of the BIBasis monomial
 * hierarchy: a singly-linked list of `VarsListNode` records
 * (each carrying a `short int Integer` variable index plus
 * `Next`) with a cached `TotalDegree`, and the static
 * `DimIndepend` that fixes the variable count for the whole
 * subsystem. Each `VarsListNode` allocates from a static
 * `FastAllocator` slab so the millions of nodes the
 * prolongation loop creates stay cache-resident. Because the
 * ground field is `F_2[x]/(x_i^2 - x_i)` every exponent is 0
 * or 1, so the list records exactly the *set* of variables that
 * appear; no exponent slots are needed.
 *
 * The pure virtuals `MultiplyBy`, `SetOne`, `operator[]`,
 * `FirstMultiVar`, and `GetVariablesSet` are filled in by the
 * three concrete subclasses --- `MonomLex`, `MonomDL`,
 * `MonomDRL` --- each implementing one monomial ordering. The
 * bibasis algorithm templates on the concrete type rather than
 * dispatching through `Monom*`, so the abstract surface here is
 * mostly a shared layout and a documentation anchor for the
 * ordering variants.
 *
 * @see allocator.hpp
 * @see monomLex.hpp
 * @see monomDL.hpp
 * @see monomDRL.hpp
 * @see polynom.hpp
 */

#include <set>
#include <iostream>
#include "allocator.hpp"

namespace BIBasis
{
    /**
     * @brief Abstract base for the BIBasis boolean-coefficient monomial
     * types --- a sorted, singly linked list of variable indices.
     *
     * @details `Integer` is a 16-bit type because BIBasis caps the number of
     * variables (`DimIndepend`) at 32k. Subclasses
     * (`MonomLex`, `MonomDL`, `MonomDRL`) implement the comparison
     * for their respective monomial orders; the storage layout is
     * shared in this base. Memory comes from a per-`VarsListNode`
     * `FastAllocator`, so list insertion is bump-pointer cheap.
     * `TotalDegree` caches the sum of exponents for fast degree
     * lookups; in this boolean (square-free) setting that equals
     * the list length.
     */
    class Monom
    {
    public:
        typedef short int Integer;
        enum Order
        {
            Lex,
            DegLex,
            DegRevLex
        };

    protected:
        /**
         * @brief Singly linked-list node of a `Monom`'s variable list, with
         * a per-class slab allocator.
         *
         * @details `Value` is the variable index, `Next` chains to the next
         * variable in increasing order. The static `Allocator`
         * (a `FastAllocator` shared across all `VarsListNode`s)
         * supplies fixed-size slots so new/delete avoid the system
         * heap on the BIBasis inner loop.
         */
        struct VarsListNode
        {
            Integer Value;
            VarsListNode* Next;
            static FastAllocator Allocator;

            VarsListNode();
            ~VarsListNode();

            void* operator new(size_t);
            void operator delete(void* ptr);
        };
        VarsListNode* ListHead;
        Integer TotalDegree;

        static Integer DimIndepend;

    protected:
        Monom();
        virtual void MultiplyBy(Integer var) = 0;
        virtual VarsListNode* Find(const Integer var) const = 0;

    public:
        virtual ~Monom();
        virtual Integer Degree() const;

        virtual void SetOne() = 0;
        virtual Integer operator[](const Integer var) const = 0;
        
        virtual Integer FirstMultiVar() const = 0;
        virtual std::set<Integer> GetVariablesSet() const = 0;

        static Integer GetDimIndepend();
        static void SetDimIndepend(Integer independ);
    };

    inline Monom::VarsListNode::VarsListNode()
        : Value(0)
        , Next(nullptr)
    {
    }

    inline Monom::VarsListNode::~VarsListNode()
    {
    }

    inline void* Monom::VarsListNode::operator new(std::size_t)
    {
        return Allocator.Allocate();
    }

    inline void Monom::VarsListNode::operator delete(void* ptr)
    {
        Allocator.Free(ptr);
    }

    inline Monom::Monom()
        : ListHead(nullptr)
        , TotalDegree(0)
    {
    }

    inline Monom::~Monom()
    {
    }

    inline Monom::Integer Monom::Degree() const
    {
        return TotalDegree;
    }

    inline Monom::Integer Monom::GetDimIndepend()
    {
        return DimIndepend;
    }
}

#endif //BIBASIS_MONOM_HPP
