# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_ssp2_emissions_factors_xml
#'
#' Construct XML data structure for \code{ssp2_emissions_factors_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ssp2_emissions_factors_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_ssp2_emissions_factors_xml.R} (emissions XML).
module_gcameurope_ssp2_emissions_factors_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L251.ssp2_ef_EUR",
             "L251.ssp2_ef_elec_EUR",
             "L251.ssp2_ef_vin_EUR",
             "L251.ssp2_ef_residTradBio_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ssp2_emissions_factors_EUR.xml",
             XML = "ssp2_emissions_factors_tradBio_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L251.ssp2_ef_EUR <- get_data(all_data, "L251.ssp2_ef_EUR")
    L251.ssp2_ef_residTradBio_EUR <- get_data(all_data, "L251.ssp2_ef_residTradBio_EUR")
    L251.ssp2_ef_elec_EUR <- get_data(all_data, "L251.ssp2_ef_elec_EUR")
    L251.ssp2_ef_vin_EUR <- get_data(all_data, "L251.ssp2_ef_vin_EUR")

    # Silence package checks
    emiss.coeff <- NULL

    # ===================================================

    # Rename L251.ssp2_ef_EUR column to match the expected column
    # names in the add_xml_data header.
    L251.ssp2_ef_EUR <- rename(L251.ssp2_ef_EUR, `emiss.coef` = `emiss.coeff`)
    L251.ssp2_ef_residTradBio_EUR <- rename(L251.ssp2_ef_residTradBio_EUR, `emiss.coef` = `emiss.coeff`)

    # Produce outputs
    create_xml("ssp2_emissions_factors_EUR.xml") %>%
      add_xml_data(L251.ssp2_ef_EUR, "InputEmissCoeff") %>%
      add_xml_data(L251.ssp2_ef_elec_EUR, "OutputEmissCoeff") %>%
      add_xml_data(L251.ssp2_ef_vin_EUR, "ReadInControl") %>%
      add_precursors("L251.ssp2_ef_EUR", "L251.ssp2_ef_elec_EUR", "L251.ssp2_ef_vin_EUR") ->
      ssp2_emissions_factors_EUR.xml

    create_xml("ssp2_emissions_factors_tradBio_EUR.xml") %>%
      add_xml_data(L251.ssp2_ef_residTradBio_EUR, "InputEmissCoeff") %>%
      add_precursors("L251.ssp2_ef_residTradBio_EUR") ->
      ssp2_emissions_factors_tradBio_EUR.xml

    return_data(ssp2_emissions_factors_EUR.xml, ssp2_emissions_factors_tradBio_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
