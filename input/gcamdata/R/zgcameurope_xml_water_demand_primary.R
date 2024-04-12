# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_water_demand_primary_xml
#'
#' Construct XML data structure for \code{water_demand_primary_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_demand_primary_EUR.xml}.
module_gcameurope_water_demand_primary_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.TechCoef_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_demand_primary_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.TechCoef_EUR <- get_data(all_data, "L210.TechCoef_EUR")

    # ===================================================

    # Produce outputs
    create_xml("water_demand_primary_EUR.xml") %>%
      add_xml_data(L210.TechCoef_EUR, "TechCoef") %>%
      add_precursors("L210.TechCoef_EUR") ->
      water_demand_primary_EUR.xml

    return_data(water_demand_primary_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
