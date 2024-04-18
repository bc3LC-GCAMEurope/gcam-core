# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_EFW_input_coefs_xml
#'
#' Construct XML data structure for \code{EFW_input_coefs_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{EFW_input_coefs_EUR.xml}.
module_gcameurope_EFW_input_coefs_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L270.TechCoef_EFW_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "EFW_input_coefs_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L270.TechCoef_EFW_EUR <- get_data(all_data, "L270.TechCoef_EFW_EUR")

    # ===================================================

    # Produce outputs
    create_xml("EFW_input_coefs_EUR.xml") %>%
      add_xml_data(L270.TechCoef_EFW_EUR, "TechCoef") %>%
      add_precursors("L270.TechCoef_EFW_EUR") ->
      EFW_input_coefs_EUR.xml

    return_data(EFW_input_coefs_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
