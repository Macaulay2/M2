/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#include "launcher.hpp"
#include "involutive.hpp"
#include "monom.hpp"
#include "monomDL.hpp"
#include "monomDRL.hpp"
#include "monomLex.hpp"
#include "settings-manager.hpp"
#include "matrix.hpp"

namespace BIBasis
{
    const Matrix* Launcher::GetBIBasisMatrix(const Matrix* matrix, int toGroebner) const
    {
        if (!CheckMatrix(matrix))
        {
            return nullptr;
        }
        
        switch(GetSettingsManager().GetMonomialOrder())
        {
            case Monom::Lex:
                {
                    BooleanInvolutiveBasis<MonomLex> basis(matrix, toGroebner);
                    return basis.ToMatrix();
                }
            case Monom::DegLex:
                {
                    BooleanInvolutiveBasis<MonomDL> basis(matrix, toGroebner);
                    return basis.ToMatrix();
                }
            case Monom::DegRevLex:
                {
                    BooleanInvolutiveBasis<MonomDRL> basis(matrix, toGroebner);
                    return basis.ToMatrix();
                }
            default:
                return nullptr;
        };
    }

    bool Launcher::CheckMatrix(const Matrix* matrix) const
    {
        if (!matrix)
        {
            ERROR("BIBasis::CheckMatrix(): no input matrix.");
            return false;
        }

        const Ring* matrixRing = matrix->get_ring();
        if (!matrixRing)
        {
            ERROR("BIBasis::CheckMatrix(): failed to get input matrix ring.");
            return false;
        }
  
        if (matrixRing->characteristic() != 2)
        {
            ERROR("BIBasis::CheckMatrix(): input matrix ring is not ZZ/2.");
            return false;
        }
  
        const PolynomialRing* polynomialRing = matrixRing->cast_to_PolynomialRing();
        if (!polynomialRing)
        {
            ERROR("BIBasis::CheckMatrix(): failed to cast matrix ring to polynomial ring.");
            return false;
        }
  
        const Ring* coefficientRing = polynomialRing->getCoefficientRing();
        if (!coefficientRing)
        {
            ERROR("BIBasis::CheckMatrix(): failed to get coefficient ring.");
            return false;
        }
  
        if (!coefficientRing->isFinitePrimeField())
        {
            ERROR("BIBasis::CheckMatrix(): coefficient ring is not ZZ/2.");
            return false;
        }
  
        const Monoid* monoid = polynomialRing->getMonoid();
        if (!monoid)
        {
            ERROR("BIBasis::CheckMatrix(): failed to get monoid.");
            return false;
        }
  
        const MonomialOrdering* monomialOrdering = monoid->getMonomialOrdering();
        if (!monomialOrdering)
        {
            ERROR("BIBasis::CheckMatrix(): failed to get monomial ordering.");
            return false;
        }
  
        if (!monomialOrdering->array[0])
        {
            ERROR("BIBasis::CheckMatrix(): monomial ordering is unknown.");
            return false;
        }
  
        switch(monomialOrdering->array[0]->type)
        {
            case MO_LEX:
                GetSettingsManager().SetMonomialOrder(Monom::Lex);
                break;
            case MO_GREVLEX:
                GetSettingsManager().SetMonomialOrder(Monom::DegRevLex);
                break;
            case MO_WEIGHTS:
                GetSettingsManager().SetMonomialOrder(Monom::DegLex);
                break;
            default:
                ERROR("BIBasis::CheckMatrix(): monomial ordering is unsupported: %d.", monomialOrdering->array[0]->type);
                return false;
        }
        
        GetSettingsManager().SetNumberOfVariables(static_cast<BIBasis::Monom::Integer>(polynomialRing->n_vars()));
        
        return true;
    }
}
