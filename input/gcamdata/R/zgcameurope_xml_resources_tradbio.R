# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_resources_tradbio_xml
#'
#' Construct XML data structure for \code{resources_tradbio_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{resources_tradbio_EUR.xml}.
module_gcameurope_resources_tradbio_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L210.RenewRsrc_tradbio_EUR",
                      "L210.RenewRsrcPrice_tradbio_EUR",
                      "L210.GrdRenewRsrcCurves_tradbio_EUR",
                      "L210.GrdRenewRsrcMax_tradbio_EUR",
                      "L210.ResTechShrwt_tradbio_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resources_tradbio_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # ===================================================
    # Produce outputs
    create_xml("resources_tradbio_EUR.xml") %>%
      add_xml_data(L210.RenewRsrc_tradbio_EUR, "RenewRsrc") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L210.RenewRsrcPrice_tradbio_EUR, "RenewRsrcPrice") %>%
      add_xml_data(L210.GrdRenewRsrcCurves_tradbio_EUR, "GrdRenewRsrcCurves") %>%
      add_xml_data(L210.GrdRenewRsrcMax_tradbio_EUR, "GrdRenewRsrcMax") %>%
      add_xml_data(L210.ResTechShrwt_tradbio_EUR, "ResTechShrwt") %>%
      add_precursors(MODULE_INPUTS) ->
      resources_tradbio_EUR.xml

    return_data(resources_tradbio_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
