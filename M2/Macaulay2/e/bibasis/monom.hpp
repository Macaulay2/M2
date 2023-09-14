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

#include <set>
#include <iostream>
#include "allocator.hpp"

namespace BIBasis
{
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
        , Next(0)
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
        : ListHead(0)
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
