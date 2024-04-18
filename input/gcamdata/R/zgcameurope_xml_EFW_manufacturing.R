# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_EFW_manufacturing_xml
#'
#' Construct XML data structure for \code{EFW_manufacturing_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{EFW_manufacturing_EUR.xml}.
module_water_EFW_manufacturing_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L273.Supplysector_ind_EUR",
                     "L273.FinalEnergyKeyword_ind_EUR",
                     "L273.SubsectorLogit_ind_EUR",
                     "L273.SubsectorShrwtFllt_ind_EUR",
                     "L273.StubTech_ind_EUR",
                     "L273.StubTechCoef_ind_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "EFW_manufacturing_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Produce outputs
    create_xml("EFW_manufacturing_EUR.xml") %>%
      add_logit_tables_xml(L273.Supplysector_ind_EUR, "Supplysector") %>%
      add_logit_tables_xml(L273.SubsectorLogit_ind_EUR, "SubsectorLogit") %>%
      add_xml_data(L273.FinalEnergyKeyword_ind_EUR, "FinalEnergyKeyword") %>%
      add_xml_data(L273.SubsectorShrwtFllt_ind_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L273.StubTech_ind_EUR, "StubTech") %>%
      add_xml_data(L273.StubTechCoef_ind_EUR, "StubTechCoef") %>%
      add_precursors(MODULE_INPUTS) ->
      EFW_manufacturing_EUR.xml

    return_data(EFW_manufacturing_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
