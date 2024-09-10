# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_elecS_ghg_emissions_water_xml
#'
#' Construct XML data structure for \code{elecS_ghg_emissions_water_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs
module_gcameurope_elecS_ghg_emissions_water_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2236.elecS_cool_ghg_tech_coeff_EUR",
             "L2236.elecS_cool_ghg_emissions_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "elecS_ghg_emissions_water_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2236.elecS_cool_ghg_tech_coeff_EUR <- get_data(all_data, 'L2236.elecS_cool_ghg_tech_coeff_EUR')
    L2236.elecS_cool_ghg_emissions_EUR  <- get_data(all_data, 'L2236.elecS_cool_ghg_emissions_EUR')

    # Produce outputs
    create_xml("elecS_ghg_emissions_water_EUR.xml") %>%
      add_xml_data_generate_levels(L2236.elecS_cool_ghg_emissions_EUR,
                                   "StbTechOutputEmissions","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2236.elecS_cool_ghg_tech_coeff_EUR,
                                   "OutputEmissCoeff","subsector","nesting-subsector",1,FALSE) %>%
      add_precursors("L2236.elecS_cool_ghg_tech_coeff_EUR",
                     "L2236.elecS_cool_ghg_emissions_EUR") ->
      elecS_ghg_emissions_water_EUR.xml

    return_data(elecS_ghg_emissions_water_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
