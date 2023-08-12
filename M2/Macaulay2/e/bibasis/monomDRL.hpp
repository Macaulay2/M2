/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#ifndef BIBASIS_MONOM_DRL_HPP
#define BIBASIS_MONOM_DRL_HPP

#include <set>
#include "allocator.hpp"
#include "monom.hpp"

namespace BIBasis
{
    class MonomDRL : public Monom
    {
    private:
        static FastAllocator Allocator;

    public:
        MonomDRL* Next;

    public:
        MonomDRL();
        MonomDRL(const MonomDRL& anotherMonom);
        ~MonomDRL();

        void* operator new(std::size_t);
        void operator delete(void* ptr);

        void SetOne();
        Integer operator[](const Integer var) const;

        const MonomDRL& operator=(const MonomDRL& anotherMonom);
        const MonomDRL& operator*=(Integer var);
        const MonomDRL& operator*=(const MonomDRL& anotherMonom);
        const MonomDRL& operator/=(const MonomDRL& anotherMonom);
        void SetQuotientOf(const MonomDRL& monomA, const MonomDRL& monomB);

        bool operator==(const MonomDRL& anotherMonom) const;
        bool operator!=(const MonomDRL& anotherMonom) const;

        bool operator<(const MonomDRL& anotherMonom) const;
        bool operator>(const MonomDRL& anotherMonom) const;
        int Compare(const MonomDRL& anotherMonom);

        bool IsDivisibleBy(const MonomDRL& anotherMonom) const;
        bool IsTrueDivisibleBy(const MonomDRL& anotherMonom) const;
        bool IsPommaretDivisibleBy(const MonomDRL& anotherMonom) const;

        Integer FirstMultiVar() const;
        std::set<Integer> GetVariablesSet() const;

    private:
        void MultiplyBy(Integer var);
        VarsListNode* Find(const Integer var) const;
    };


    inline MonomDRL::MonomDRL()
        : Monom()
        , Next(nullptr)
    {
    }

    inline MonomDRL::MonomDRL(const MonomDRL& anotherMonom)
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

    inline MonomDRL::~MonomDRL()
    {
        SetOne();
    }

    inline void* MonomDRL::operator new(std::size_t) 
    {
        return Allocator.Allocate();
    }

    inline void MonomDRL::operator delete(void* ptr) 
    {
        Allocator.Free(ptr);
    }

    inline MonomDRL::VarsListNode* MonomDRL::Find(const Monom::Integer var) const
    {
        if (!ListHead || ListHead->Value < var)
        {
            return nullptr;
        }

        VarsListNode* position = ListHead;
        while (position && position->Next && position->Next->Value >= var)
        {
            position = position->Next;
        }
        return position;
    }

    inline void MonomDRL::SetOne() 
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

    inline MonomDRL::Integer MonomDRL::operator[](MonomDRL::Integer var) const
    {
        VarsListNode* varPosition = Find(var);
        return varPosition && varPosition->Value == var;
    }

    inline const MonomDRL& MonomDRL::operator=(const MonomDRL& anotherMonom)
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

    inline void MonomDRL::MultiplyBy(MonomDRL::Integer var)
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

    inline const MonomDRL& MonomDRL::operator*=(MonomDRL::Integer var)
    {
        MultiplyBy(var);
        return *this;
    }

    inline const MonomDRL& MonomDRL::operator*=(const MonomDRL& anotherMonom)
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
                    else if ((*iterator)->Value > anotherIterator->Value)
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

    inline const MonomDRL& MonomDRL::operator/=(const MonomDRL& anotherMonom)
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
            else if ((*iterator)->Value > anotherIterator->Value)
            {
                iterator = &((*iterator)->Next);
            }
        }

        return *this;
    }

    inline void MonomDRL::SetQuotientOf(const MonomDRL& monomA, const MonomDRL& monomB)
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
                if (iteratorA->Value > iteratorB->Value)
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

    inline bool MonomDRL::operator==(const MonomDRL& anotherMonom) const
    {
        if (TotalDegree != anotherMonom.TotalDegree)
        {
            return false;
        }
        else
        {
            VarsListNode *iterator = ListHead,
                         *anotherIterator = anotherMonom.ListHead;
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

    inline bool MonomDRL::operator!=(const MonomDRL& anotherMonom) const
    {
        if (TotalDegree != anotherMonom.TotalDegree)
        {
            return true;
        }
        else
        {
            VarsListNode *iterator = ListHead,
                         *anotherIterator = anotherMonom.ListHead;
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

    inline bool MonomDRL::operator<(const MonomDRL& anotherMonom) const
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
            VarsListNode *iterator = ListHead,
                         *anotherIterator = anotherMonom.ListHead;
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

    inline bool MonomDRL::operator>(const MonomDRL& anotherMonom) const
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
            VarsListNode *iterator = ListHead,
                         *anotherIterator = anotherMonom.ListHead;
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

    inline bool MonomDRL::IsDivisibleBy(const MonomDRL& anotherMonom) const
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
            else if (iterator->Value > anotherIterator->Value)
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

    inline bool MonomDRL::IsTrueDivisibleBy(const MonomDRL& anotherMonom) const 
    {
        if (TotalDegree <= anotherMonom.TotalDegree)
        {
            return false;
        }

        VarsListNode *iterator = ListHead,
                     *anotherIterator = anotherMonom.ListHead;
        while (iterator && anotherIterator)
        {
            if (iterator->Value == anotherIterator->Value)
            {
                iterator = iterator->Next;
                anotherIterator = anotherIterator->Next;
            }
            else if (iterator->Value > anotherIterator->Value)
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

    inline bool MonomDRL::IsPommaretDivisibleBy(const MonomDRL& anotherMonom) const
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
        while (iterator && iterator->Value > anotherIterator->Value)
        {
            iterator = iterator->Next;
        }

        while (iterator && anotherIterator)
        {
            if (iterator->Value != anotherIterator->Value)
            {
                break;
            }
            iterator = iterator->Next;
            anotherIterator = anotherIterator->Next;
        }

        return !iterator && !anotherIterator;
    }

    inline MonomDRL::Integer MonomDRL::FirstMultiVar() const
    {
        if (!ListHead)
        {
            return 0;
        }
        else
        {
            return ListHead->Value;
        }
    }

    inline std::set<MonomDRL::Integer> MonomDRL::GetVariablesSet() const
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

#endif // BIBASIS_MONOM_DRL_HPP
