# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_aluminum_xml
#'
#' Construct XML data structure for \code{aluminum_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{aluminum_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_aluminum_xml.R} (energy XML).
module_gcameurope_aluminum_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L2326.Supplysector_aluminum_EUR",
                     "L2326.FinalEnergyKeyword_aluminum_EUR",
                     "L2326.SubsectorLogit_aluminum_EUR",
                     "L2326.SubsectorShrwtFllt_aluminum_EUR",
                     "L2326.SubsectorInterp_aluminum_EUR",
                     "L2326.StubTech_aluminum_EUR",
                     "L2326.StubTechProd_aluminum_EUR",
                     "L2326.StubTechCalInput_aluminum_EUR",
                     "L2326.StubTechCoef_aluminum_EUR",
                     "L2326.StubTechSecMarket_aluminum_EUR",
                     "L2326.PerCapitaBased_aluminum_EUR",
                     "L2326.BaseService_aluminum_EUR",
                     "L2326.PriceElasticity_aluminum_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "aluminum_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # ===================================================

    # Produce outputs
    create_xml("aluminum_EUR.xml") %>%
      add_logit_tables_xml(L2326.Supplysector_aluminum_EUR, "Supplysector") %>%
      add_xml_data(L2326.FinalEnergyKeyword_aluminum_EUR, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2326.SubsectorLogit_aluminum_EUR, "SubsectorLogit") %>%
      add_xml_data(L2326.SubsectorShrwtFllt_aluminum_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L2326.SubsectorInterp_aluminum_EUR, "SubsectorInterp") %>%
      add_xml_data(L2326.StubTech_aluminum_EUR, "StubTech") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L2326.StubTechProd_aluminum_EUR, "StubTechProd") %>%
      add_xml_data(L2326.StubTechCalInput_aluminum_EUR, "StubTechCalInput") %>%
      add_xml_data(L2326.StubTechCoef_aluminum_EUR, "StubTechCoef") %>%
      add_xml_data(L2326.StubTechSecMarket_aluminum_EUR, "StubTechSecMarket") %>%
      add_xml_data(L2326.PerCapitaBased_aluminum_EUR, "PerCapitaBased") %>%
      add_xml_data(L2326.BaseService_aluminum_EUR, "BaseService") %>%
      add_xml_data(L2326.PriceElasticity_aluminum_EUR, "PriceElasticity") %>%
      add_precursors(MODULE_INPUTS) ->
      aluminum_EUR.xml

    return_data(aluminum_EUR.xml)
  } else {
    stop("Unknown command")
  }
}

