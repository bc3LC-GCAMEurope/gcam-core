# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_desalination_xml
#'
#' Construct XML data structure for \code{desalination_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{desalination_EUR.xml}.
module_gcameurope_desalination_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L271.Supplysector_desal_EUR",
                     "L271.FinalEnergyKeyword_desal_EUR",
                     "L271.SubsectorLogit_desal_EUR",
                     "L271.SubsectorShrwtFllt_desal_EUR",
                     "L271.SubsectorInterp_desal_EUR",
                     "L271.SubsectorInterpTo_desal_EUR",
                     "L271.StubTech_desal_EUR",
                     "L271.StubTechProd_desal_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "desalination_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Produce outputs
    create_xml("desalination_EUR.xml") %>%
      add_logit_tables_xml(L271.Supplysector_desal_EUR, "Supplysector") %>%
      add_logit_tables_xml(L271.SubsectorLogit_desal_EUR, "SubsectorLogit") %>%
      add_xml_data(L271.FinalEnergyKeyword_desal_EUR, "FinalEnergyKeyword") %>%
      add_xml_data(L271.SubsectorShrwtFllt_desal_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L271.SubsectorInterp_desal_EUR, "SubsectorInterp") %>%
      add_xml_data(L271.SubsectorInterpTo_desal_EUR, "SubsectorInterpTo") %>%
      add_xml_data(L271.StubTech_desal_EUR, "StubTech") %>%
      add_xml_data(L271.StubTechProd_desal_EUR, "StubTechProd") %>%
      add_precursors(MODULE_INPUTS) ->
      desalination_EUR.xml

    return_data(desalination_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
