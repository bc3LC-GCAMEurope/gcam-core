# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_EFW_irrigation_xml
#'
#' Construct XML data structure for \code{EFW_irrigation_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{EFW_irrigation_EUR.xml}.
module_gcameurope_EFW_irrigation_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L272.Supplysector_irr_EUR",
                     "L272.FinalEnergyKeyword_irr_EUR",
                     "L272.SubsectorLogit_irr_EUR",
                     "L272.SubsectorShrwtFllt_irr_EUR",
                     "L272.StubTech_irr_EUR",
                     "L272.StubTechCoef_irr_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "EFW_irrigation_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Produce outputs
    create_xml("EFW_irrigation_EUR.xml") %>%
      add_logit_tables_xml(L272.Supplysector_irr_EUR, "Supplysector") %>%
      add_logit_tables_xml(L272.SubsectorLogit_irr_EUR, "SubsectorLogit") %>%
      add_xml_data(L272.FinalEnergyKeyword_irr_EUR, "FinalEnergyKeyword") %>%
      add_xml_data(L272.SubsectorShrwtFllt_irr_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L272.StubTech_irr_EUR, "StubTech") %>%
      add_xml_data(L272.StubTechCoef_irr_EUR, "StubTechCoef") %>%
      add_precursors(MODULE_INPUTS) ->
      EFW_irrigation_EUR.xml

    return_data(EFW_irrigation_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
