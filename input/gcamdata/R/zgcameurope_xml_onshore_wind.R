# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_onshore_wind_xml
#'
#' Construct XML data structure for \code{onshore_wind_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{onshore_wind_EUR.xml}.
module_gcameurope_onshore_wind_xml<- function(command, ...) {
  MODULE_INPUTS <- c("L2231.SmthRenewRsrcCurves_onshore_wind_EUR",
                     "L2231.StubTechCapFactor_onshore_wind_EUR",
                     "L2231.SmthRenewRsrcTechChange_onshore_wind_EUR",
                     "L2231.StubTechCost_onshore_wind_EUR",
                     "L2231.StubTechCapFactor_onshore_wind_elecS_EUR",
                     "L2231.StubTechCost_onshore_wind_elecS_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "onshore_wind_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)


    # ===================================================

    # Produce outputs
    create_xml("onshore_wind_EUR.xml") %>%
      add_xml_data(L2231.SmthRenewRsrcCurves_onshore_wind_EUR, "SmthRenewRsrcCurves") %>%
      add_xml_data(L2231.SmthRenewRsrcTechChange_onshore_wind_EUR, "SmthRenewRsrcTechChange") %>%
      add_xml_data_generate_levels(L2231.StubTechCapFactor_onshore_wind_elecS_EUR,
                                   "StubTechCapFactor","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2231.StubTechCost_onshore_wind_elecS_EUR,
                                   "StubTechCost","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data(L2231.StubTechCapFactor_onshore_wind_EUR, "StubTechCapFactor") %>%
      add_xml_data(L2231.StubTechCost_onshore_wind_EUR, "StubTechCost") %>%
      add_precursors(MODULE_INPUTS) ->
      onshore_wind_EUR.xml

    return_data(onshore_wind_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
