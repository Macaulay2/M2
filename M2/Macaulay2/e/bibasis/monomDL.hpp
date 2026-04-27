/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#ifndef BIBASIS_MONOM_DL_HPP
#define BIBASIS_MONOM_DL_HPP

#include <set>
#include "allocator.hpp"
#include "monom.hpp"

namespace BIBasis
{
    class MonomDL : public Monom
    {
    private:
        static FastAllocator Allocator;

    public:
        MonomDL* Next;

    public:
        MonomDL();
        MonomDL(const MonomDL& anotherMonom);
        ~MonomDL();

        void* operator new(std::size_t);
        void operator delete(void* ptr);

        void SetOne();
        Integer operator[](Integer var) const;

        const MonomDL& operator=(const MonomDL& anotherMonom);
        const MonomDL& operator*=(Integer var);
        const MonomDL& operator*=(const MonomDL& anotherMonom);
        const MonomDL& operator/=(const MonomDL& anotherMonom);
        void SetQuotientOf(const MonomDL& monomA, const MonomDL& monomB);

        bool operator==(const MonomDL& anotherMonom) const;
        bool operator!=(const MonomDL& anotherMonom) const;

        bool operator<(const MonomDL& anotherMonom) const;
        bool operator>(const MonomDL& anotherMonom) const;
        int Compare(const MonomDL& anotherMonom);
        
        bool IsDivisibleBy(const MonomDL& anotherMonom) const;
        bool IsTrueDivisibleBy(const MonomDL& anotherMonom) const;
        bool IsPommaretDivisibleBy(const MonomDL& anotherMonom) const;

        Integer FirstMultiVar() const;
        std::set<Integer> GetVariablesSet() const;

    private:
        void MultiplyBy(Integer var);
        VarsListNode* Find(const Integer var) const;
    };


    inline MonomDL::MonomDL()
        : Monom()
        , Next(nullptr)
    {
    }

    inline MonomDL::MonomDL(const MonomDL& anotherMonom)
        : Monom()
        , Next(nullptr)
    {
        if (!anotherMonom.ListHead)
        {
            return;
        }
        else
        {
            TotalDegree = anotherMonom.TotalDegree;
            VarsListNode **iterator = &ListHead,
                         *iteratorAnother = anotherMonom.ListHead;
            while (iteratorAnother)
            {
                *iterator = new VarsListNode();
                (*iterator)->Value = iteratorAnother->Value;

                iterator = &((*iterator)->Next);
                iteratorAnother = iteratorAnother->Next;
            }
        }
    }

    inline MonomDL::~MonomDL()
    {
        SetOne();
    }

    inline void* MonomDL::operator new(std::size_t) 
    {
        return Allocator.Allocate();
    }

    inline void MonomDL::operator delete(void* ptr) 
    {
        Allocator.Free(ptr);
    }

    inline MonomDL::VarsListNode* MonomDL::Find(const MonomDL::Integer var) const
    {
        if (!ListHead || ListHead->Value > var)
        {
            return nullptr;
        }
        
        VarsListNode* position = ListHead;
        while (position && position->Next && position->Next->Value <= var)
        {
            position = position->Next;
        }
        return position;
    }

    inline void MonomDL::SetOne() 
    {
        TotalDegree = 0;
        if (ListHead)
        {
            VarsListNode* tmpNode;
            while (ListHead)
            {
                tmpNode = ListHead;
                ListHead = ListHead->Next;
                delete tmpNode;
            }
        }
    }

    inline MonomDL::Integer MonomDL::operator[](MonomDL::Integer var) const
    {
        VarsListNode* varPosition = Find(var);
        return varPosition && varPosition->Value == var;
    }

    inline const MonomDL& MonomDL::operator=(const MonomDL& anotherMonom)
    {
        if (this == &anotherMonom)
        {
            return *this;
        }

        if (!anotherMonom.ListHead)
        {
            SetOne();
        }
        else
        {
            TotalDegree = anotherMonom.TotalDegree;

            VarsListNode *iteratorAnother = anotherMonom.ListHead,
                         **iterator = &ListHead;
            while (*iterator && iteratorAnother)
            {
                (*iterator)->Value = iteratorAnother->Value;
                iterator = &((*iterator)->Next);
                iteratorAnother = iteratorAnother->Next;
            } 

            if (*iterator)
            {
                VarsListNode *nodeToDelete = (*iterator)->Next;
                *iterator = nullptr;
                while (nodeToDelete)
                {
                    iteratorAnother = nodeToDelete;
                    nodeToDelete = nodeToDelete->Next;
                    delete iteratorAnother;
                }
            }
            else while (iteratorAnother)
            {
                *iterator = new VarsListNode();
                (*iterator)->Value = iteratorAnother->Value;

                iterator = &((*iterator)->Next);
                iteratorAnother = iteratorAnother->Next;
            }
        }
        return *this;
    }

    inline void MonomDL::MultiplyBy(MonomDL::Integer var)
    {
        //inserted variable is the only one
        if (!ListHead)
        {
            ListHead = new VarsListNode();
            ListHead->Value = var;
            ++TotalDegree;
        }
        else
        {
            VarsListNode* position = Find(var);
            //inserted variable is the eldest one
            if (!position)
            {
                position = new VarsListNode();
                position->Value = var;
                position->Next = ListHead;
                ListHead = position;
                ++TotalDegree;
            }
            //all other cases
            else if(position->Value != var)
            {
                VarsListNode* newNode = new VarsListNode();
                newNode->Value = var;
                newNode->Next = position->Next;
                position->Next = newNode;
                ++TotalDegree;
            }
        }
    }

    inline const MonomDL& MonomDL::operator*=(MonomDL::Integer var)
    {
        MultiplyBy(var);
        return *this;
    }

    inline const MonomDL& MonomDL::operator*=(const MonomDL& anotherMonom)
    {
        if (!ListHead) 
        {
            *this = anotherMonom;
        }
        else 
        {
            if (anotherMonom.ListHead)
            {
                VarsListNode **iterator = &ListHead,
                             *anotherIterator = anotherMonom.ListHead;

                while (*iterator && anotherIterator)
                {
                    if ((*iterator)->Value == anotherIterator->Value)
                    {
                        iterator = &((*iterator)->Next);
                        anotherIterator = anotherIterator->Next;
                    }
                    else if ((*iterator)->Value < anotherIterator->Value)
                    {
                        iterator = &((*iterator)->Next);
                    }
                    else
                    {
                        VarsListNode* newNode = new VarsListNode();
                        newNode->Value = anotherIterator->Value;
                        newNode->Next = *iterator;
                        *iterator = newNode;
                        ++TotalDegree;

                        iterator = &(newNode->Next);
                        anotherIterator = anotherIterator->Next;
                    }
                }

                while (anotherIterator)
                {
                    *iterator = new VarsListNode();
                    (*iterator)->Value = anotherIterator->Value;
                    ++TotalDegree;

                    iterator = &((*iterator)->Next);
                    anotherIterator = anotherIterator->Next;
                }
            }
        }

        return *this;
    }

    inline const MonomDL& MonomDL::operator/=(const MonomDL& anotherMonom)
    {
        VarsListNode **iterator = &ListHead,
                     *anotherIterator = anotherMonom.ListHead;

        while (*iterator && anotherIterator)
        {
            if ((*iterator)->Value == anotherIterator->Value)
            {
                VarsListNode* nodeToDelete = *iterator;
                *iterator = (*iterator)->Next;
                delete nodeToDelete;
                --TotalDegree;
                anotherIterator = anotherIterator->Next;
            }
            else if ((*iterator)->Value < anotherIterator->Value)
            {
                iterator = &((*iterator)->Next);
            }
        }

        return *this;
    }

    inline void MonomDL::SetQuotientOf(const MonomDL& monomA, const MonomDL& monomB)
    {
        SetOne();
        VarsListNode **iterator = &ListHead,
                     *iteratorA = monomA.ListHead,
                     *iteratorB = monomB.ListHead;

        while (iteratorA && iteratorB)
        {
            if (iteratorA->Value == iteratorB->Value)
            {
                iteratorA = iteratorA->Next;
                iteratorB = iteratorB->Next;
            }
            else
            {
                ++TotalDegree;
                *iterator = new VarsListNode();
                (*iterator)->Value = iteratorA->Value;
                iterator = &((*iterator)->Next);
                if (iteratorA->Value < iteratorB->Value)
                {
                    iteratorA = iteratorA->Next;
                }
            }
        }
        
        while (iteratorA)
        {
            ++TotalDegree;
            *iterator = new VarsListNode();
            (*iterator)->Value = iteratorA->Value;
            iterator = &((*iterator)->Next);
            iteratorA = iteratorA->Next;
        }
    }

    inline bool MonomDL::operator==(const MonomDL& anotherMonom) const
    {
        if (TotalDegree != anotherMonom.TotalDegree)
        {
            return false;
        }
        else
        {
            VarsListNode *iterator(ListHead),
                         *anotherIterator(anotherMonom.ListHead);
            while (anotherIterator)
            {
                if (iterator->Value != anotherIterator->Value)
                {
                    break;
                }
                iterator = iterator->Next;
                anotherIterator = anotherIterator->Next;
            }
            return !anotherIterator;
        }
    }

    inline bool MonomDL::operator!=(const MonomDL& anotherMonom) const
    {
        if (TotalDegree != anotherMonom.TotalDegree)
        {
            return true;
        }
        else
        {
            VarsListNode *iterator(ListHead),
                         *anotherIterator(anotherMonom.ListHead);
            while (anotherIterator)
            {
                if (iterator->Value != anotherIterator->Value)
                {
                    break;
                }
                iterator = iterator->Next;
                anotherIterator = anotherIterator->Next;
            }
            return anotherIterator;
        }
    }

    inline bool MonomDL::operator<(const MonomDL& anotherMonom) const
    {
        if (TotalDegree < anotherMonom.TotalDegree)
        {
            return true;
        }
        else if (TotalDegree > anotherMonom.TotalDegree)
        {
            return false;
        }
        else
        {
            VarsListNode *iterator(ListHead),
                         *anotherIterator(anotherMonom.ListHead);
            while (anotherIterator)
            {
                if (iterator->Value < anotherIterator->Value)
                {
                    return false;
                }
                if (iterator->Value > anotherIterator->Value)
                {
                    return true;
                }
                iterator = iterator->Next;
                anotherIterator = anotherIterator->Next;
            }
            return false;
        }
    }

    inline bool MonomDL::operator>(const MonomDL& anotherMonom) const
    {
        if (TotalDegree < anotherMonom.TotalDegree)
        {
            return false;
        }
        else if (TotalDegree > anotherMonom.TotalDegree)
        {
            return true;
        }
        else
        {
            VarsListNode *iterator(ListHead),
                         *anotherIterator(anotherMonom.ListHead);
            while (anotherIterator)
            {
                if (iterator->Value < anotherIterator->Value)
                {
                    return true;
                }
                if (iterator->Value > anotherIterator->Value)
                {
                    return false;
                }
                iterator = iterator->Next;
                anotherIterator = anotherIterator->Next;
            }
            return false;
        }
    }

    inline bool MonomDL::IsDivisibleBy(const MonomDL& anotherMonom) const
    {
        VarsListNode *iterator = ListHead,
                     *anotherIterator = anotherMonom.ListHead;
        while (iterator && anotherIterator)
        {
            if (iterator->Value == anotherIterator->Value)
            {
                iterator = iterator->Next;
                anotherIterator = anotherIterator->Next;
            }
            else if (iterator->Value < anotherIterator->Value)
            {
                iterator = iterator->Next;
            }
            else
            {
                break;
            }
        }

        return !anotherIterator;
    }

    inline bool MonomDL::IsTrueDivisibleBy(const MonomDL& anotherMonom) const 
    {
        if (TotalDegree <= anotherMonom.TotalDegree)
        {
            return false;
        }

        VarsListNode *iterator(ListHead),
                     *anotherIterator(anotherMonom.ListHead);
        while (iterator && anotherIterator)
        {
            if (iterator->Value == anotherIterator->Value)
            {
                iterator = iterator->Next;
                anotherIterator = anotherIterator->Next;
            }
            else if (iterator->Value < anotherIterator->Value)
            {
                iterator = iterator->Next;
            }
            else
            {
                break;
            }
        }

        return !anotherIterator;
    }

    inline bool MonomDL::IsPommaretDivisibleBy(const MonomDL& anotherMonom) const
    {
        if (TotalDegree < anotherMonom.TotalDegree)
        {
            return false;
        }
        if (!anotherMonom.TotalDegree)
        {
            return true;
        }

        VarsListNode *iterator = ListHead,
                     *anotherIterator = anotherMonom.ListHead;
        while (iterator && anotherIterator)
        {
            if (iterator->Value != anotherIterator->Value)
            {
                break;
            }
            iterator = iterator->Next;
            anotherIterator = anotherIterator->Next;
        }

        return !anotherIterator;
    }

    inline MonomDL::Integer MonomDL::FirstMultiVar() const
    {
        if (!ListHead)
        {
            return 0;
        }
        
        VarsListNode* iterator(ListHead);
        while (iterator->Next)
        {
            iterator = iterator->Next;
        }
        return iterator->Value;
    }

    inline std::set<MonomDL::Integer> MonomDL::GetVariablesSet() const
    {
        std::set<Integer> result;
        VarsListNode *iterator = ListHead;
        while (iterator)
        {
            result.insert(iterator->Value);
            iterator = iterator->Next;
        }
        return result;
    }
}

#endif // BIBASIS_MONOM_DL_HPP
