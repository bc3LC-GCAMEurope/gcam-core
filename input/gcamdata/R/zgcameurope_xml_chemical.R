# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_chemical_xml
#'
#' Construct XML data structure for \code{chemical.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{chemical_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_chemical_xml.R} (energy XML).
module_gcameurope_chemical_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L2325.Supplysector_chemical_EUR",
                     "L2325.FinalEnergyKeyword_chemical_EUR",
                     "L2325.SubsectorLogit_chemical_EUR",
                     "L2325.SubsectorShrwtFllt_chemical_EUR",
                     "L2325.SubsectorInterp_chemical_EUR",
                     "L2325.StubTech_chemical_EUR",
                     "L2325.StubTechProd_chemical_EUR",
                     "L2325.StubTechCalInput_chemical_EUR",
                     "L2325.StubTechCoef_chemical_EUR",
                     "L2325.StubTechSecMarket_chemical_EUR",
                     "L2325.PerCapitaBased_chemical_EUR",
                     "L2325.BaseService_chemical_EUR",
                     "L2325.PriceElasticity_chemical_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "chemical_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # ===================================================

    # Produce outputs
    create_xml("chemical_EUR.xml") %>%
      add_logit_tables_xml(L2325.Supplysector_chemical_EUR, "Supplysector") %>%
      add_xml_data(L2325.FinalEnergyKeyword_chemical_EUR, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2325.SubsectorLogit_chemical_EUR, "SubsectorLogit") %>%
      add_xml_data(L2325.SubsectorShrwtFllt_chemical_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L2325.SubsectorInterp_chemical_EUR, "SubsectorInterp") %>%
      add_xml_data(L2325.StubTech_chemical_EUR, "StubTech") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L2325.StubTechProd_chemical_EUR, "StubTechProd") %>%
      add_xml_data(L2325.StubTechCalInput_chemical_EUR, "StubTechCalInput") %>%
      add_xml_data(L2325.StubTechCoef_chemical_EUR, "StubTechCoef") %>%
      add_xml_data(L2325.StubTechSecMarket_chemical_EUR, "StubTechSecMarket") %>%
      add_xml_data(L2325.PerCapitaBased_chemical_EUR, "PerCapitaBased") %>%
      add_xml_data(L2325.BaseService_chemical_EUR, "BaseService") %>%
      add_xml_data(L2325.PriceElasticity_chemical_EUR, "PriceElasticity") %>%
      add_precursors(MODULE_INPUTS) ->
      chemical_EUR.xml

    return_data(chemical_EUR.xml)
  } else {
    stop("Unknown command")
  }
}

