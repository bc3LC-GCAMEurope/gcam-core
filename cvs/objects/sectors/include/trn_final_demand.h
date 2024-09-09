/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


#ifndef _TRN_FINAL_DEMAND_H_
#define _TRN_FINAL_DEMAND_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file trn_final_demand.h
 * \ingroup Objects
 * \brief The TrnFinalDemand class header file.
 * \author Josh Lurz
 */

#include "sectors/include/afinal_demand.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"
#include "sectors/include/energy_final_demand.h"

// Forward declarations
class Demographic;

/*! 
 * \ingroup Objects
 * \brief A class which represents a single end use of an energy product or
 *        service.
 * \details Energy final demands consume an energy derived good and are counted
 *          towards the total final energy of the region.
 */

class TrnFinalDemand : public EnergyFinalDemand
{
    friend class XMLDBOutputter;
    friend class EnergyBalanceTable; // TODO: currently only to get mServiceDemands

public:
    static const std::string& getXMLNameStatic();

    virtual const std::string& getXMLName() const;

    TrnFinalDemand();

    virtual ~TrnFinalDemand();

    virtual void toDebugXML(const int aPeriod,
        std::ostream& aOut,
        Tabs* aTabs) const;

    virtual const std::string& getName() const;

    virtual void completeInit(const std::string& aRegionName,
        const IInfo* aRegionInfo);

    virtual void initCalc(const std::string& aRegionName,
        const Demographic* aDemographics,
        const int aPeriod);

    //virtual double trnSubregPop(const string& aRegionName,
     //   const Demographic* aDemographics,
      //  const int aPeriod);

    virtual void setFinalDemand(const std::string& aRegionName,
        const Demographic* aDemographics,
        const int aPeriod);

    virtual double calcFinalDemand(const std::string& aRegionName,
        const Demographic* aDemographics,
        const int aPeriod);

    virtual void accept(IVisitor* aVisitor, const int aPeriod) const;

    virtual double getPrice(const std::string& aRegionName,
        const int aPeriod) const;

    virtual double getPricePaid(const std::string& aRegionName,
        const int aPeriod) const;

    // Methods for deriving from EnergyFinalDemand.
    virtual void toDebugXMLDerived(const int period, std::ostream& out, Tabs* tabs) const;


protected:


    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        EnergyFinalDemand,

        //! Coefficient to estimate pass_km demand                      
        DEFINE_VARIABLE(SIMPLE, "coef_trn", mTrnCoef, Value),

        //! Bias Adder to estimate pass_km demand                      
        DEFINE_VARIABLE(ARRAY, "bias-adder", mBiasAdderTrn, objects::PeriodVector<Value>),

        //! Coefficient to adjust trn prices                       
        DEFINE_VARIABLE(SIMPLE, "basePrice", mBasePriceTrn, Value),

        //! Price parameters for debug                      
        DEFINE_VARIABLE(SIMPLE | NOT_PARSABLE, "PriceAdjustParam", mPriceAdjustParam, Value),

        //! Price parameters for debug                      
        DEFINE_VARIABLE(SIMPLE | NOT_PARSABLE, "PriceAdj", mPriceAdj, Value),

        //! Price parameters for debug                      
        DEFINE_VARIABLE(SIMPLE | NOT_PARSABLE, "Price", mPrice, Value),

        //! Price parameters for debug                      
        DEFINE_VARIABLE(SIMPLE | NOT_PARSABLE, "PriceLag", mPriceLag, Value),

        //! Price parameters for debug                      
        DEFINE_VARIABLE(SIMPLE | NOT_PARSABLE, "PriceRatio", mPriceRatio, Value),

        //! Total end-use sector service after technical change is applied.
        DEFINE_VARIABLE(ARRAY | STATE | NOT_PARSABLE, "subregional-population", mSubregionalPopulation, objects::PeriodVector<Value>),

        //! Total end-use sector service after technical change is applied.
        DEFINE_VARIABLE(ARRAY | STATE | NOT_PARSABLE, "subregional-income", mSubregionalIncome, objects::PeriodVector<Value>),

        //! Income share 
        DEFINE_VARIABLE(ARRAY, "subregional-income-share", mSubregIncomeShare, objects::PeriodVector<Value>),

        //! Population share
        DEFINE_VARIABLE(ARRAY, "subregional-population-share", mSubregPopShare, objects::PeriodVector<Value>)

    )

};



#endif // _TRN_FINAL_DEMAND_H_

