# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_Off_road_xml
#'
#' Construct XML data structure for \code{Off_road.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{Off_road_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_Off_road_xml.R} (energy XML).
module_gcameurope_Off_road_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2324.Supplysector_Off_road_EUR",
             "L2324.FinalEnergyKeyword_Off_road_EUR",
             "L2324.SubsectorLogit_Off_road_EUR",
             "L2324.SubsectorShrwtFllt_Off_road_EUR",
             "L2324.SubsectorInterp_Off_road_EUR",
             "L2324.StubTech_Off_road_EUR",
             "L2324.StubTechProd_Off_road_EUR",
             "L2324.StubTechCalInput_Off_road_EUR",
             "L2324.StubTechCoef_Off_road_EUR",
             "L2324.PerCapitaBased_Off_road_EUR",
             "L2324.BaseService_Off_road_EUR",
             "L2324.PriceElasticity_Off_road_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "Off_road_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2324.Supplysector_Off_road_EUR <- get_data(all_data, "L2324.Supplysector_Off_road_EUR")
    L2324.FinalEnergyKeyword_Off_road_EUR <- get_data(all_data, "L2324.FinalEnergyKeyword_Off_road_EUR")
    L2324.SubsectorLogit_Off_road_EUR <- get_data(all_data, "L2324.SubsectorLogit_Off_road_EUR")
    L2324.SubsectorShrwtFllt_Off_road_EUR <- get_data(all_data, "L2324.SubsectorShrwtFllt_Off_road_EUR")
    L2324.SubsectorInterp_Off_road_EUR <- get_data(all_data, "L2324.SubsectorInterp_Off_road_EUR")
    L2324.StubTech_Off_road_EUR <- get_data(all_data, "L2324.StubTech_Off_road_EUR")
    L2324.StubTechProd_Off_road_EUR <- get_data(all_data, "L2324.StubTechProd_Off_road_EUR")
    L2324.StubTechCalInput_Off_road_EUR <- get_data(all_data, "L2324.StubTechCalInput_Off_road_EUR")
    L2324.StubTechCoef_Off_road_EUR <- get_data(all_data, "L2324.StubTechCoef_Off_road_EUR")
    L2324.PerCapitaBased_Off_road_EUR <- get_data(all_data, "L2324.PerCapitaBased_Off_road_EUR")
    L2324.BaseService_Off_road_EUR <- get_data(all_data, "L2324.BaseService_Off_road_EUR")
    L2324.PriceElasticity_Off_road_EUR <- get_data(all_data, "L2324.PriceElasticity_Off_road_EUR")
    # ===================================================

    # Produce outputs
    create_xml("Off_road_EUR.xml") %>%
      add_logit_tables_xml(L2324.Supplysector_Off_road_EUR, "Supplysector") %>%
      add_xml_data(L2324.FinalEnergyKeyword_Off_road_EUR, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2324.SubsectorLogit_Off_road_EUR, "SubsectorLogit") %>%
      add_xml_data(L2324.SubsectorShrwtFllt_Off_road_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L2324.SubsectorInterp_Off_road_EUR, "SubsectorInterp") %>%
      add_xml_data(L2324.StubTech_Off_road_EUR, "StubTech") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L2324.StubTechProd_Off_road_EUR, "StubTechProd") %>%
      add_xml_data(L2324.StubTechCalInput_Off_road_EUR, "StubTechCalInput") %>%
      add_xml_data(L2324.StubTechCoef_Off_road_EUR, "StubTechCoef") %>%
      add_xml_data(L2324.PerCapitaBased_Off_road_EUR, "PerCapitaBased") %>%
      add_xml_data(L2324.BaseService_Off_road_EUR, "BaseService") %>%
      add_xml_data(L2324.PriceElasticity_Off_road_EUR, "PriceElasticity") %>%
      add_precursors("L2324.Supplysector_Off_road_EUR", "L2324.FinalEnergyKeyword_Off_road_EUR", "L2324.SubsectorLogit_Off_road_EUR",
                     "L2324.SubsectorShrwtFllt_Off_road_EUR","L2324.SubsectorInterp_Off_road_EUR", "L2324.StubTech_Off_road_EUR",
                     "L2324.StubTechCoef_Off_road_EUR", "L2324.StubTechCalInput_Off_road_EUR","L2324.StubTechProd_Off_road_EUR",
                     "L2324.PerCapitaBased_Off_road_EUR", "L2324.BaseService_Off_road_EUR", "L2324.PriceElasticity_Off_road_EUR") ->
      Off_road_EUR.xml

    return_data(Off_road_EUR.xml)
  } else {
    stop("Unknown command")
  }
}

