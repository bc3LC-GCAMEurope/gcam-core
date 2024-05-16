# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_iron_steel_xml
#'
#' Construct XML data structure for \code{iron_steel_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{iron_steel_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_iron_steel_xml.R} (energy XML).
module_gcameurope_iron_steel_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2323.Supplysector_iron_steel_EUR",
             "L2323.FinalEnergyKeyword_iron_steel_EUR",
             "L2323.SubsectorLogit_iron_steel_EUR",
             "L2323.SubsectorShrwtFllt_iron_steel_EUR",
             "L2323.SubsectorInterp_iron_steel_EUR",
             "L2323.StubTech_iron_steel_EUR",
             "L2323.StubTechCost_iron_steel_EUR",
             "L2323.StubTechProd_iron_steel_EUR",
             "L2323.StubTechCoef_iron_steel_EUR",
             "L2323.PerCapitaBased_iron_steel_EUR",
             "L2323.BaseService_iron_steel_EUR",
             "L2323.PriceElasticity_iron_steel_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "iron_steel_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2323.Supplysector_iron_steel_EUR <- get_data(all_data, "L2323.Supplysector_iron_steel_EUR")
    L2323.FinalEnergyKeyword_iron_steel_EUR <- get_data(all_data, "L2323.FinalEnergyKeyword_iron_steel_EUR")
    L2323.SubsectorLogit_iron_steel_EUR <- get_data(all_data, "L2323.SubsectorLogit_iron_steel_EUR")
    L2323.SubsectorShrwtFllt_iron_steel_EUR <- get_data(all_data, "L2323.SubsectorShrwtFllt_iron_steel_EUR")
    L2323.SubsectorInterp_iron_steel_EUR <- get_data(all_data, "L2323.SubsectorInterp_iron_steel_EUR")
    L2323.StubTech_iron_steel_EUR <- get_data(all_data, "L2323.StubTech_iron_steel_EUR")
    L2323.StubTechCost_iron_steel_EUR <- get_data(all_data, "L2323.StubTechCost_iron_steel_EUR")
    L2323.StubTechProd_iron_steel_EUR <- get_data(all_data, "L2323.StubTechProd_iron_steel_EUR")
    L2323.StubTechCoef_iron_steel_EUR <- get_data(all_data, "L2323.StubTechCoef_iron_steel_EUR")
    L2323.PerCapitaBased_iron_steel_EUR <- get_data(all_data, "L2323.PerCapitaBased_iron_steel_EUR")
    L2323.BaseService_iron_steel_EUR <- get_data(all_data, "L2323.BaseService_iron_steel_EUR")
    L2323.PriceElasticity_iron_steel_EUR <- get_data(all_data, "L2323.PriceElasticity_iron_steel_EUR")

    # ===================================================

    # Produce outputs
    create_xml("iron_steel_EUR.xml") %>%
      add_logit_tables_xml(L2323.Supplysector_iron_steel_EUR, "Supplysector") %>%
      add_xml_data(L2323.FinalEnergyKeyword_iron_steel_EUR, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2323.SubsectorLogit_iron_steel_EUR, "SubsectorLogit") %>%
      add_xml_data(L2323.SubsectorShrwtFllt_iron_steel_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L2323.SubsectorInterp_iron_steel_EUR, "SubsectorInterp") %>%
      add_xml_data(L2323.StubTech_iron_steel_EUR, "StubTech") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L2323.StubTechCost_iron_steel_EUR, "StubTechCost") %>%
      add_xml_data(L2323.StubTechProd_iron_steel_EUR, "StubTechProd") %>%
      add_xml_data(L2323.StubTechCoef_iron_steel_EUR, "StubTechCoef") %>%
      add_xml_data(L2323.PerCapitaBased_iron_steel_EUR, "PerCapitaBased") %>%
      add_xml_data(L2323.BaseService_iron_steel_EUR, "BaseService") %>%
      add_xml_data(L2323.PriceElasticity_iron_steel_EUR, "PriceElasticity") %>%
      add_precursors("L2323.Supplysector_iron_steel_EUR", "L2323.FinalEnergyKeyword_iron_steel_EUR", "L2323.SubsectorLogit_iron_steel_EUR",
                     "L2323.SubsectorShrwtFllt_iron_steel_EUR", "L2323.SubsectorInterp_iron_steel_EUR",
                     "L2323.StubTech_iron_steel_EUR", "L2323.StubTechProd_iron_steel_EUR",
                     "L2323.StubTechCoef_iron_steel_EUR", "L2323.PerCapitaBased_iron_steel_EUR", "L2323.BaseService_iron_steel_EUR",
                     "L2323.PriceElasticity_iron_steel_EUR","L2323.StubTechCost_iron_steel_EUR") ->
      iron_steel_EUR.xml
    return_data(iron_steel_EUR.xml)
  } else {
    stop("Unknown command")
  }
}

