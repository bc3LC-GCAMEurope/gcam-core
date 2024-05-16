# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_all_fgas_emissions_xml
#'
#' Construct XML data structure for \code{all_fgas_emissions_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_fgas_emissions_EUR.xml}, \code{all_fgas_emissions_MAC_EUR.xml}.
#' The corresponding file in the
#' original data system was \code{batch_all_fgas_emissions.xml} (emissions XML).
module_gcameurope_all_fgas_emissions_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L241.hfc_all_EUR",
             "L241.pfc_all_EUR",
             "L241.hfc_future_EUR",
             "L241.fgas_all_units_EUR",
             "L252.MAC_higwp_EUR",
             "L252.MAC_higwp_phaseInTime_EUR",
             "L252.MAC_higwp_tc_average_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_fgas_emissions_EUR.xml",
             XML = "all_fgas_emissions_MAC_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    tech.change <- tech.change.year <- NULL #Silence package check

    # Load required inputs
    L241.hfc_all_EUR <- get_data(all_data, "L241.hfc_all_EUR")
    L241.pfc_all_EUR <- get_data(all_data, "L241.pfc_all_EUR")
    L241.hfc_future_EUR <- get_data(all_data, "L241.hfc_future_EUR")
    L241.fgas_all_units_EUR <- get_data(all_data, "L241.fgas_all_units_EUR")
    L252.MAC_higwp_EUR <- get_data(all_data, "L252.MAC_higwp_EUR")
    L252.MAC_higwp_phaseInTime_EUR <- get_data(all_data, "L252.MAC_higwp_phaseInTime_EUR")
    L252.MAC_higwp_tc_average_EUR <- get_data(all_data, "L252.MAC_higwp_tc_average_EUR")

    # ===================================================

    # Produce outputs
    create_xml("all_fgas_emissions_EUR.xml") %>%
      add_xml_data(L241.hfc_all_EUR, "StbTechOutputEmissions") %>%
      add_xml_data(L241.pfc_all_EUR, "StbTechOutputEmissions") %>%
      add_xml_data(L241.hfc_future_EUR, "OutputEmissCoeff") %>%
      add_xml_data(L241.fgas_all_units_EUR, "StubTechEmissUnits") %>%
      add_precursors("L241.hfc_all_EUR", "L241.pfc_all_EUR",
                     "L241.hfc_future_EUR", "L241.fgas_all_units_EUR") ->
      all_fgas_emissions_EUR.xml

    create_xml("all_fgas_emissions_MAC_EUR.xml") %>%
      add_xml_data(L252.MAC_higwp_EUR, "MAC") %>%
      add_xml_data(L252.MAC_higwp_tc_average_EUR, "MACTC") %>%
      add_xml_data(L252.MAC_higwp_phaseInTime_EUR, "MACPhaseIn") %>%
      add_precursors("L252.MAC_higwp_EUR", "L252.MAC_higwp_tc_average_EUR", "L252.MAC_higwp_phaseInTime_EUR") ->
      all_fgas_emissions_MAC_EUR.xml

    return_data(all_fgas_emissions_EUR.xml,
                all_fgas_emissions_MAC_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
