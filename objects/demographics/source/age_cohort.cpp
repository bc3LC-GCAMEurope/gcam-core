/*! 
* \file age_cohort.cpp
* \ingroup Objects-SGM
* \brief AgeCohort class source file.
* \author Sonny Kim
* \author Katherine Chung
* \date $Date$
* \version $Revision$
*/

#include <cassert>
#include <string>

// needed for getLowerAgeBound and getUpperAgeBound
#include <sstream>
#include <algorithm>

#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "demographics/include/age_cohort.h"
#include "demographics/include/gender.h"
#include "demographics/include/male.h"
#include "demographics/include/female.h"
#include "util/base/include/xml_helper.h"

using namespace std;
using namespace xercesc;

extern ofstream outputFile;
//static initialize
const string AgeCohort::XML_NAME = "ageCohort";

//! default constructor
AgeCohort::AgeCohort() {
    mLowerAgeBound = -1;
    mUpperAgeBound = -1;
}

//! Default Destructor. Needed because we are using auto_ptr so that we avoid
//! destroying incomplete types.
AgeCohort::~AgeCohort(){
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& AgeCohort::getXMLName() const{
    return XML_NAME;
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& AgeCohort::getXMLNameStatic(){
    return XML_NAME;
}

//! parses AgeCohort xml object
void AgeCohort::XMLParse( const xercesc::DOMNode* node ){
    // make sure we were passed a valid node.
    assert( node );

    ageGroup = XMLHelper<string>::getAttrString( node, "ageGroup" );

    // get all child nodes.
    DOMNodeList* nodeList = node->getChildNodes();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        // Note: Male and female can either be parsed as <gender ="male"> or <male>
        else if( nodeName == "gender" ){
            if( !parseGender( curr ) ){
                cout << "Failed to parse a gender element." << endl;
            }
        }
        else if( parseGender( curr ) ){
            // do nothing but don't print a warning.
        }
        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing ageCohort." << endl;
        }
    }
}

//! Attempt to parse a gender.
bool AgeCohort::parseGender( DOMNode* aNode ) {
    // We could either be parsing out of the "type" attribute
    // or the element name itself.
    string type = XMLHelper<string>::getAttrString( aNode, "type" );
    if( type == "" ){
        type = XMLHelper<string>::safeTranscode( aNode->getNodeName() );
    }

    if( type == Male::getXMLNameStatic() ){
        if( !male.get() ){
            male.reset( new Male() );
        }
        male->XMLParse( aNode );
    }
    else if( type == Female::getXMLNameStatic() ) {
        if( !female.get() ){
            female.reset( new Female() );
        }
        female->XMLParse( aNode );
    }
    else {
        return false;
    }
    return true;
}
//! Write out datamembers to XML output stream.
void AgeCohort::toInputXML( std::ostream& out, Tabs* tabs ) const{
    // write the beginning tag.
    tabs->writeTabs( out );
    out << "<ageCohort ageGroup=\"" << ageGroup << "\">"<< endl;

    // increase the indent.
    tabs->increaseIndent();

    male->toInputXML( out, tabs );
    female->toInputXML( out, tabs );
    XMLWriteClosingTag( "ageCohort", out, tabs );
}

//! Write out XML for debugging purposes.
void AgeCohort::toDebugXML( ostream& out, Tabs* tabs ) const {

    // write the beginning tag.
    tabs->writeTabs( out );
    out << "<ageCohort ageGroup=\"" << ageGroup << "\">"<< endl;

    // increase the indent.
    tabs->increaseIndent();

    male->toDebugXML( out, tabs );
    female->toDebugXML( out, tabs );

    XMLWriteClosingTag( "ageCohort", out, tabs );
}

//! Complete the initialization.
void AgeCohort::completeInit(){
    // TODO: Add better non-crashing error checking to this class.
    assert( female.get() );
    assert( male.get() );
}

//! initialize anything that won't change during the calcuation
void AgeCohort::initCalc(){
    male->calcSurvivingPop();
    female->calcSurvivingPop();
}

//! sets the male population to the param passed in
void AgeCohort::setMalePop( double aMalePopulation ){
    male->setPopulation( aMalePopulation );
}

//! sets the female population to the param passed in
void AgeCohort::setFemalePop(double aFemalePopulation ){
    female->setPopulation( aFemalePopulation );
}

//! returns the male population for this agecohort
double AgeCohort::getMalePop() const {
    return male->getPopulation();
}

//! returns the female population for this agecohort
double AgeCohort::getFemalePop() const {
    return female->getPopulation();
}

// calculate the male births from this age group
// and return the value
// only females give birth
double AgeCohort::calcMaleBirth() {
    return female->calcMaleBirth();
}

// calculate the female births from this age group
// and return the value
// only females give birth
double AgeCohort::calcFemaleBirth() {
    return female->calcFemaleBirth();
}

// calculate the surviving male population of this age group
// and return the value
double AgeCohort::calcSurvMalePop() {
    return male->calcSurvivingPop();
}

// calculate the surviving male population of this age group
// and return the value
double AgeCohort::calcSurvFemalePop() {
    return female->calcSurvivingPop();
}

const string& AgeCohort::getAgeGroup() const {
    return ageGroup;
}

/*! \brief Get the lower limit to the ages contained in this cohort.
* \details This calculates and returns the lower limit of the ages stored
* in this cohort. The function also caches the result locally so that the 
* calculation does not have to be repeated each iteration. If the age limit 
* cannot be parsed the function returns -1.
* \return The lower age limit for this cohort and -1 if there is an error.
* \author Josh Lurz
*/
int AgeCohort::getLowerAgeBound() const {
    // Check if we cached the lower age bound.
    if( mLowerAgeBound != -1 ){
        return mLowerAgeBound;
    }

    // Check if this is the last age cohort, specified as +x
    string lowerRangeString;
    if( ageGroup.find_first_of( '+' ) != ageGroup.npos ){
        lowerRangeString = ageGroup.substr( 1, ageGroup.size() - 1 );
    }
    else {
        // Check and make sure there is one and only one hyphen.
        if( count( ageGroup.begin(), ageGroup.end(), '-' ) != 1 ){
            cout << "Invalid age range " << ageGroup << " found while calculating getLowerAgeBound " << endl;
            return -1;
        }
        // Find the hyphen we now know is there.
        const string::size_type hyphenPos = ageGroup.find_first_of( '-' );
        // Get the lower range string.
        lowerRangeString = ageGroup.substr( 0, hyphenPos );
    }

    // Convert it to an int.
    istringstream converter( lowerRangeString );
    int lowerRange = -1;
    converter >> lowerRange;

    // Cache the value since this function is slow.
    mLowerAgeBound = lowerRange;
    return lowerRange;
}

/*! \brief Get the upper limit to the ages contained in this cohort.
* \details This calculates and returns the upper limit of the ages stored
* in this cohort. The function also caches the result locally so that the 
* calculation does not have to be repeated each iteration. If the age limit 
* cannot be parsed the function returns -1. If the upper age range is unbounded,
* in the case where this is the last cohort, it will return AGE_MAX.
* \return The upper age limit for this cohort, AGE_MAX if it is unbounded and -1 if
* there is an error.
* \author Josh Lurz
*/
int AgeCohort::getUpperAgeBound() const {
    const int AGE_MAX = 500;

    // Check if we cached the upper age bound.
    if( mUpperAgeBound != -1 ){
        return mUpperAgeBound;
    }
    // Check if this is the last age cohort, specified as +x
    if( ageGroup.find_first_of( '+' ) != ageGroup.npos ){
        // This age group has no upper bound.
        return AGE_MAX;
    }

    // Check and make sure there is one and only one hyphen.
    if( count( ageGroup.begin(), ageGroup.end(), '-' ) != 1 ){
        cout << "Invalid age range " << ageGroup << " found while calculating getUpperAgeBound " << endl;
        return -1;
    }
    // Find the hyphen we now know is there.
    const string::size_type hyphenPos = ageGroup.find_first_of( '-' );
    // Get the upper range string.
    const string upperRangeString = ageGroup.substr( hyphenPos + 1, ageGroup.size() -1 );
    // Convert it to an int.
    istringstream converter( upperRangeString );
    int upperRange = -1;
    converter >> upperRange;

    // Cache the value since this function is slow.
    mUpperAgeBound = upperRange;
    return upperRange;
}