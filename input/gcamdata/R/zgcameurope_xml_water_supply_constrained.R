# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_water_supply_constrained_xml
#'
#' Construct XML data structure for \code{water_supply_constrained_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_supply_constrained_EUR.xml}.
module_gcameurope_water_supply_constrained_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L201.DeleteUnlimitRsrc_EUR",
                     "L201.Rsrc_EUR",
                     "L201.RsrcPrice_EUR",
                     "L201.RenewRsrcCurves_calib_EUR",
                     "L201.GrdRenewRsrcMax_runoff_EUR",
                     "L201.DepRsrcCurves_ground_EUR",
                     "L201.RenewRsrcTechShrwt_EUR",
                     "L201.RsrcTechShrwt_EUR",
                     "L201.RsrcTechCoef_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_supply_constrained_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # ===================================================

    # Produce outputs
    create_xml("water_supply_constrained_EUR.xml") %>%
      add_node_equiv_xml("resource") %>%
      add_xml_data(L201.DeleteUnlimitRsrc_EUR, "DeleteUnlimitRsrc") %>%
      add_xml_data(L201.Rsrc_EUR, "Rsrc") %>%
      add_xml_data(L201.RsrcPrice_EUR, "RsrcPrice") %>%
      # Note we are going to use the RenewRsrcCurves header to avoid having to create
      # duplicate headers.  The resource type will remain "resource" because we set
      # the "resource" node_equiv_xml above.  However we still need to set the
      # column names appropriately if we want the column re-ordering to work.
      add_xml_data(L201.RenewRsrcCurves_calib_EUR %>% rename(renewresource = resource), "RenewRsrcCurves") %>%
      add_xml_data(L201.GrdRenewRsrcMax_runoff_EUR, "GrdRenewRsrcMaxNoFillOut") %>%
      add_xml_data(L201.DepRsrcCurves_ground_EUR, "RsrcCurves") %>%
      add_node_equiv_xml("subresource") %>%
      add_xml_data(L201.RenewRsrcTechShrwt_EUR, "ResTechShrwt") %>%
      add_xml_data(L201.RsrcTechShrwt_EUR, "ResTechShrwt") %>%
      add_xml_data(L201.RsrcTechCoef_EUR, "ResTechCoef") %>%
      add_precursors(MODULE_INPUTS) ->
      water_supply_constrained_EUR.xml
    return_data(water_supply_constrained_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
