# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_heat_xml
#'
#' Construct XML data structure for \code{heat_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{heat_EUR.xml}.
module_gcameurope_heat_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L224.Supplysector_heat_EUR",
                     "L224.SubsectorLogit_heat_EUR",
                     "L224.SubsectorShrwtFllt_heat_EUR",
                     "L224.SubsectorInterp_heat_EUR",
                     "L224.StubTech_heat_EUR",
                     "L224.StubTechCalInput_heat_EUR",
                     "L224.StubTechSecOut_elec_EUR",
                     "L224.StubTechCost_elec_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "heat_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # ===================================================
    # Rename the tibble columns to match the header info.
    L224.StubTechCalInput_heat_EUR <- rename(L224.StubTechCalInput_heat_EUR, tech.share.weight = share.weight, share.weight.year = year.share.weight)

    # Produce outputs
    create_xml("heat_EUR.xml") %>%
      add_logit_tables_xml(L224.Supplysector_heat_EUR, "Supplysector") %>%
      add_logit_tables_xml(L224.SubsectorLogit_heat_EUR, "SubsectorLogit") %>%
      add_xml_data(L224.SubsectorShrwtFllt_heat_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L224.SubsectorInterp_heat_EUR, "SubsectorInterp") %>%
      add_xml_data(L224.StubTech_heat_EUR, "StubTech") %>%
      add_xml_data(L224.StubTechCalInput_heat_EUR, "StubTechCalInput") %>%
      add_xml_data(L224.StubTechSecOut_elec_EUR, "StubTechSecOut") %>%
      add_xml_data(L224.StubTechCost_elec_EUR, "StubTechCost") %>%
      add_precursors("L224.Supplysector_heat_EUR",
                     "L224.SubsectorLogit_heat_EUR",
                     "L224.SubsectorShrwtFllt_heat_EUR",
                     "L224.SubsectorInterp_heat_EUR",
                     "L224.StubTech_heat_EUR",
                     "L224.StubTechCalInput_heat_EUR",
                     "L224.StubTechSecOut_elec_EUR",
                     "L224.StubTechCost_elec_EUR") ->
      heat_EUR.xml

    return_data(heat_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
