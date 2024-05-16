# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_water_td_xml
#'
#' Construct XML data structure for \code{water_td_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_td_EUR.xml}. The corresponding file in the
#' original data system was \code{batch_water_mapping.xml.R} (water XML).
module_gcameurope_water_td_xml <- function(command, ...) {
  MODULE_INPUTS <- c("L203.Supplysector_watertd_EUR",
                     "L203.SubsectorLogit_watertd_EUR",
                     "L203.SubsectorShrwtFllt_watertd_EUR",
                     "L203.SubsectorInterp_watertd_EUR",
                     "L203.TechShrwt_watertd_EUR",
                     "L203.TechInterp_watertd_EUR",
                     "L203.TechCoef_watertd_EUR",
                     "L203.TechPmult_watertd_EUR",
                     "L203.Production_watertd_EUR",
                     "L203.Supplysector_desal_basin_EUR",
                     "L203.SubsectorLogit_desal_basin_EUR",
                     "L203.SubsectorShrwtFllt_desal_basin_EUR",
                     "L203.TechShrwt_desal_basin_EUR",
                     "L203.TechCoef_desal_basin_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_td_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # ===================================================

    # Produce outputs
    create_xml("water_td_EUR.xml") %>%
      add_logit_tables_xml(L203.Supplysector_watertd_EUR, "Supplysector") %>%
      add_logit_tables_xml(L203.SubsectorLogit_watertd_EUR, "SubsectorLogit") %>%
      add_xml_data(L203.SubsectorShrwtFllt_watertd_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L203.SubsectorInterp_watertd_EUR, "SubsectorInterp") %>%
      add_xml_data(L203.TechShrwt_watertd_EUR, "TechShrwt") %>%
      add_xml_data(L203.TechInterp_watertd_EUR, "TechInterp") %>%
      add_xml_data(L203.TechCoef_watertd_EUR, "TechCoef") %>%
      add_xml_data(L203.TechPmult_watertd_EUR, "TechPmult") %>%
      add_xml_data(L203.Production_watertd_EUR, "Production") %>%
      add_logit_tables_xml(L203.Supplysector_desal_basin_EUR, "Supplysector") %>%
      add_logit_tables_xml(L203.SubsectorLogit_desal_basin_EUR, "SubsectorLogit") %>%
      add_xml_data(L203.SubsectorShrwtFllt_desal_basin_EUR, "SubsectorShrwtFllt") %>%
      add_xml_data(L203.TechShrwt_desal_basin_EUR, "TechShrwt") %>%
      add_xml_data(L203.TechCoef_desal_basin_EUR, "TechCoef") %>%
      add_precursors(MODULE_INPUTS) ->
      water_td_EUR.xml

    return_data(water_td_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
