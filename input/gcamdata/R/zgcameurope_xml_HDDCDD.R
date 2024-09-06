# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_HDDCDD_xml
#'
#' Construct XML data structure for all the \code{HDDCDD.xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{HDDCDD_A2_CCSM3x_EUR.xml}, \code{HDDCDD_A2_HadCM3_EUR.xml}, \code{HDDCDD_B1_CCSM3x_EUR.xml},
#' \code{HDDCDD_B1_HadCM3_EUR.xml}, and \code{HDDCDD_constdd_no_GCM_EUR.xml}.
module_gcameurope_HDDCDD_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.HDDCDD_A2_CCSM3x_EUR",
             "L244.HDDCDD_A2_HadCM3_EUR",
             "L244.HDDCDD_B1_CCSM3x_EUR",
             "L244.HDDCDD_B1_HadCM3_EUR",
             "L244.HDDCDD_constdd_no_GCM_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "HDDCDD_A2_CCSM3x_EUR.xml",
             XML = "HDDCDD_A2_HadCM3_EUR.xml",
             XML = "HDDCDD_B1_CCSM3x_EUR.xml",
             XML = "HDDCDD_B1_HadCM3_EUR.xml",
             XML = "HDDCDD_constdd_no_GCM_EUR.xml"))
  } else if(command == driver.MAKE) {

    # silence package checks
    HDDCDD_A2_CCSM3x_EUR.xml <- HDDCDD_A2_HadCM3_EUR.xml <- HDDCDD_B1_CCSM3x_EUR.xml <- HDDCDD_B1_HadCM3_EUR.xml <-
      HDDCDD_constdd_no_GCM_EUR.xml <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.HDDCDD_A2_CCSM3x_EUR <- get_data(all_data, "L244.HDDCDD_A2_CCSM3x_EUR")
    L244.HDDCDD_A2_HadCM3_EUR <- get_data(all_data, "L244.HDDCDD_A2_HadCM3_EUR")
    L244.HDDCDD_B1_CCSM3x_EUR <- get_data(all_data, "L244.HDDCDD_B1_CCSM3x_EUR")
    L244.HDDCDD_B1_HadCM3_EUR <- get_data(all_data, "L244.HDDCDD_B1_HadCM3_EUR")
    L244.HDDCDD_constdd_no_GCM_EUR <- get_data(all_data, "L244.HDDCDD_constdd_no_GCM_EUR")

    # Produce outputs
    create_xml("HDDCDD_A2_CCSM3x_EUR.xml") %>%
      add_xml_data(L244.HDDCDD_A2_CCSM3x_EUR, "HDDCDD") %>%
      add_precursors("L244.HDDCDD_A2_CCSM3x_EUR") ->
      HDDCDD_A2_CCSM3x_EUR.xml

    create_xml("HDDCDD_A2_HadCM3_EUR.xml") %>%
      add_xml_data(L244.HDDCDD_A2_HadCM3_EUR, "HDDCDD") %>%
      add_precursors("L244.HDDCDD_A2_HadCM3_EUR") ->
      HDDCDD_A2_HadCM3_EUR.xml

    create_xml("HDDCDD_B1_CCSM3x_EUR.xml") %>%
      add_xml_data(L244.HDDCDD_B1_CCSM3x_EUR, "HDDCDD") %>%
      add_precursors("L244.HDDCDD_B1_CCSM3x_EUR") ->
      HDDCDD_B1_CCSM3x_EUR.xml

    create_xml("HDDCDD_B1_HadCM3_EUR.xml") %>%
      add_xml_data(L244.HDDCDD_B1_HadCM3_EUR, "HDDCDD") %>%
      add_precursors("L244.HDDCDD_B1_HadCM3_EUR") ->
      HDDCDD_B1_HadCM3_EUR.xml

    create_xml("HDDCDD_constdd_no_GCM_EUR.xml") %>%
      add_xml_data(L244.HDDCDD_constdd_no_GCM_EUR, "HDDCDD") %>%
      add_precursors("L244.HDDCDD_constdd_no_GCM_EUR") ->
      HDDCDD_constdd_no_GCM_EUR.xml

    return_data(HDDCDD_A2_CCSM3x_EUR.xml, HDDCDD_A2_HadCM3_EUR.xml, HDDCDD_B1_CCSM3x_EUR.xml, HDDCDD_B1_HadCM3_EUR.xml,
                HDDCDD_constdd_no_GCM_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
