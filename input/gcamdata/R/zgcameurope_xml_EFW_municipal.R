# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_EFW_municipal_xml
#'
#' Construct XML data structure for \code{EFW_municipal_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{EFW_municipal_EUR.xml}.
module_gcameurope_EFW_municipal_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L274.Supplysector_muni_EUR",
                     "L274.FinalEnergyKeyword_muni_EUR",
                     "L274.SubsectorLogit_muni_EUR",
                     "L274.SubsectorShrwtFllt_muni_EUR",
                     "L274.StubTech_muni_EUR",
                     "L274.StubTechCoef_muni_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "EFW_municipal_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Produce outputs
    create_xml("EFW_municipal_EUR.xml") %>%
      add_logit_tables_xml(L274.Supplysector_muni_EUR, "Supplysector") %>%
      add_logit_tables_xml(L274.SubsectorLogit_muni_EUR, "SubsectorLogit") %>%
      add_xml_data(L274.FinalEnergyKeyword_muni_EUR, "FinalEnergyKeyword") %>%
      add_xml_data(L274.SubsectorShrwtFllt_muni_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L274.StubTech_muni_EUR, "StubTech") %>%
      add_xml_data(L274.StubTechCoef_muni_EUR, "StubTechCoef") %>%
      add_precursors(MODULE_INPUTS) ->
      EFW_municipal_EUR.xml

    return_data(EFW_municipal_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
