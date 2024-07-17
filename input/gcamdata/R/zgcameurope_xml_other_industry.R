# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_other_industry_xml
#'
#' Construct XML data structure for \code{other_industry_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry.xml}. The corresponding file in the
#' original data system was \code{batch_industry_xml.R} (energy XML).
module_gcameurope_other_industry_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L232.SubsectorLogit_ind_EUR",
                     "L232.FinalEnergyKeyword_ind_EUR",
                     "L232.SubsectorInterp_ind_EUR",
                     "L232.StubTech_ind_EUR",
                     "L232.StubTechInterp_ind_EUR",
                     "L232.StubTechCalInput_indenergy_EUR",
                     "L232.StubTechCalInput_indfeed_EUR",
                     "L232.StubTechProd_industry_EUR",
                     "L232.StubTechCoef_industry_EUR",
                     "L232.FuelPrefElast_indenergy_EUR",
                     "L232.PerCapitaBased_ind_EUR",
                     "L232.PriceElasticity_ind_EUR",
                     "L232.BaseService_ind_EUR",
                     "L232.SubsectorShrwtFllt_ind_EUR",
                     "L232.Supplysector_ind_EUR",
                     "L232.StubTechSecOut_ind_EUR",
                     "L232.StubTechSecMarket_ind_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "other_industry_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    # ===================================================

    # Produce outputs
    create_xml("other_industry_EUR.xml") %>%
      add_logit_tables_xml(L232.Supplysector_ind_EUR, "Supplysector") %>%
      add_logit_tables_xml(L232.SubsectorLogit_ind_EUR, "SubsectorLogit") %>%
      add_xml_data(L232.SubsectorShrwtFllt_ind_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L232.FinalEnergyKeyword_ind_EUR, "FinalEnergyKeyword") %>%
      add_xml_data(L232.SubsectorInterp_ind_EUR, "SubsectorInterp") %>%
      add_xml_data(L232.StubTech_ind_EUR, "StubTech") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L232.StubTechInterp_ind_EUR, "StubTechInterp") %>%
      add_xml_data(L232.StubTechCalInput_indenergy_EUR, "StubTechCalInput") %>%
      add_xml_data(L232.StubTechCalInput_indfeed_EUR, "StubTechCalInput") %>%
      add_xml_data(L232.StubTechProd_industry_EUR, "StubTechProd") %>%
      add_xml_data(L232.StubTechCoef_industry_EUR, "StubTechCoef") %>%
      add_xml_data(L232.StubTechSecOut_ind_EUR, "StubTechSecOutMarket") %>%
      add_xml_data(L232.StubTechSecMarket_ind_EUR, "StubTechSecMarket") %>%
      add_xml_data(L232.FuelPrefElast_indenergy_EUR, "FuelPrefElast") %>%
      add_xml_data(L232.PerCapitaBased_ind_EUR, "PerCapitaBased") %>%
      add_xml_data(L232.PriceElasticity_ind_EUR, "PriceElasticity") %>%
      add_xml_data(L232.BaseService_ind_EUR, "BaseService") %>%
      add_precursors(MODULE_INPUTS) ->
      other_industry_EUR.xml

    return_data(other_industry_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
