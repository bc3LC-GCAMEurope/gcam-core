# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_cement_xml
#'
#' Construct XML data structure for \code{cement.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{cement_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_cement_xml.R} (energy XML).
module_gcameurope_cement_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2321.Supplysector_cement_EUR",
             "L2321.FinalEnergyKeyword_cement_EUR",
             "L2321.SubsectorLogit_cement_EUR",
             "L2321.SubsectorShrwtFllt_cement_EUR",
             "L2321.SubsectorInterp_cement_EUR",
             "L2321.StubTech_cement_EUR",
             "L2321.StubTechProd_cement_EUR",
             "L2321.StubTechCalInput_cement_heat_EUR",
             "L2321.StubTechCoef_cement_EUR",
             "L2321.PerCapitaBased_cement_EUR",
             "L2321.BaseService_cement_EUR",
             "L2321.PriceElasticity_cement_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "cement_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2321.Supplysector_cement_EUR <- get_data(all_data, "L2321.Supplysector_cement_EUR")
    L2321.FinalEnergyKeyword_cement_EUR <- get_data(all_data, "L2321.FinalEnergyKeyword_cement_EUR")
    L2321.SubsectorLogit_cement_EUR <- get_data(all_data, "L2321.SubsectorLogit_cement_EUR")
    #    L2321.SubsectorShrwt_cement <- get_data(all_data, "L2321.SubsectorShrwt_cement")
    L2321.SubsectorShrwtFllt_cement_EUR <- get_data(all_data, "L2321.SubsectorShrwtFllt_cement_EUR")
    L2321.SubsectorInterp_cement_EUR <- get_data(all_data, "L2321.SubsectorInterp_cement_EUR")
    #    L2321.SubsectorInterpTo_cement <- get_data(all_data, "L2321.SubsectorInterpTo_cement")
    L2321.StubTech_cement_EUR <- get_data(all_data, "L2321.StubTech_cement_EUR")
    L2321.StubTechProd_cement_EUR <- get_data(all_data, "L2321.StubTechProd_cement_EUR")
    L2321.StubTechCalInput_cement_heat_EUR <- get_data(all_data, "L2321.StubTechCalInput_cement_heat_EUR")
    L2321.StubTechCoef_cement_EUR <- get_data(all_data, "L2321.StubTechCoef_cement_EUR")
    L2321.PerCapitaBased_cement_EUR <- get_data(all_data, "L2321.PerCapitaBased_cement_EUR")
    L2321.BaseService_cement_EUR <- get_data(all_data, "L2321.BaseService_cement_EUR")
    L2321.PriceElasticity_cement_EUR <- get_data(all_data, "L2321.PriceElasticity_cement_EUR")

    # ===================================================

    # Produce outputs
    create_xml("cement_EUR.xml") %>%
      add_logit_tables_xml(L2321.Supplysector_cement_EUR, "Supplysector") %>%
      add_xml_data(L2321.FinalEnergyKeyword_cement_EUR, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2321.SubsectorLogit_cement_EUR, "SubsectorLogit") %>%
      #      add_xml_data(L2321.SubsectorShrwt_cement, "SubsectorShrwt") %>%
      add_xml_data(L2321.SubsectorShrwtFllt_cement_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L2321.SubsectorInterp_cement_EUR, "SubsectorInterp") %>%
      #      add_xml_data(L2321.SubsectorInterpTo_cement, "SubsectorInterpTo") %>%
      add_xml_data(L2321.StubTech_cement_EUR, "StubTech") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L2321.StubTechProd_cement_EUR, "StubTechProd") %>%
      add_xml_data(L2321.StubTechCalInput_cement_heat_EUR, "StubTechCalInput") %>%
      add_xml_data(L2321.StubTechCoef_cement_EUR, "StubTechCoef") %>%
      add_xml_data(L2321.PerCapitaBased_cement_EUR, "PerCapitaBased") %>%
      add_xml_data(L2321.BaseService_cement_EUR, "BaseService") %>%
      add_xml_data(L2321.PriceElasticity_cement_EUR, "PriceElasticity") %>%
      add_precursors("L2321.Supplysector_cement_EUR", "L2321.FinalEnergyKeyword_cement_EUR", "L2321.SubsectorLogit_cement_EUR",
                     "L2321.SubsectorShrwtFllt_cement_EUR", "L2321.SubsectorInterp_cement_EUR",
                     "L2321.StubTech_cement_EUR", "L2321.StubTechProd_cement_EUR", "L2321.StubTechCalInput_cement_heat_EUR",
                     "L2321.StubTechCoef_cement_EUR", "L2321.PerCapitaBased_cement_EUR", "L2321.BaseService_cement_EUR",
                     "L2321.PriceElasticity_cement_EUR") ->
      cement_EUR.xml

    return_data(cement_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
