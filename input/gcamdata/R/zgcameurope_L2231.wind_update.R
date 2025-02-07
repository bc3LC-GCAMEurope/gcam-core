# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L2231.wind_update
#'
#' Updates region-specific onshore wind supply curves for europe with the new electricity segments
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2231.SmthRenewRsrcCurves_onshore_wind_EUR}, \code{L2231.StubTechCapFactor_onshore_wind_EUR}, \code{L2231.SmthRenewRsrcTechChange_onshore_wind_EUR},
#' \code{L2231.StubTechCost_onshore_wind_EUR}. The corresponding file in the
#' original data system was \code{L2231.wind_update.R} (energy level2).
#' @details Updates region-specific onshore wind supply curves using improved global wind resource estimate from Eurek et al. (2016).
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select group_by summarise distinct arrange bind_rows rename
#' @importFrom tidyr gather
#' @importFrom stats optimize
#' @author MB GI AJS March 2019
module_gcameurope_L2231.wind_update <- function(command, ...) {
  ZENERGY_OUTPUTS <- c("L2231.SmthRenewRsrcCurves_onshore_wind",
                     "L2231.StubTechCapFactor_onshore_wind",
                     "L2231.SmthRenewRsrcTechChange_onshore_wind",
                     "L2231.StubTechCost_onshore_wind")
  MODULE_INPUTS <- c(FILE = "gcam-europe/mappings/grid_regions",
                     "L2235.StubTech_elecS_cool_EUR",
                     ZENERGY_OUTPUTS)
  MODULE_OUTPUTS <- c(paste0(ZENERGY_OUTPUTS, "_EUR"),
                      "L2231.StubTechCapFactor_onshore_wind_elecS_EUR",
                      "L2231.StubTechCost_onshore_wind_elecS_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]


    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Change EUR sector names ===================================================
    L2231.StubTechCapFactor_onshore_wind_elecS_EUR <- L2231.StubTechCapFactor_onshore_wind %>%
      semi_join(L2235.StubTech_elecS_cool_EUR, by = "region") %>%
      select(-supplysector, subsector0 = subsector) %>%
      left_join(L2235.StubTech_elecS_cool_EUR, by = c("region", "subsector0", "stub.technology"))

    # any gcam-europe countries not in segments
    L2231.StubTechCapFactor_onshore_wind_EUR <- L2231.StubTechCapFactor_onshore_wind %>%
      filter(region %in% gcameurope.EUROSTAT_COUNTRIES & !region %in% L2231.StubTechCapFactor_onshore_wind_elecS_EUR$region)

    L2231.StubTechCost_onshore_wind_elecS_EUR <- L2231.StubTechCost_onshore_wind %>%
      semi_join(L2235.StubTech_elecS_cool_EUR, by = "region") %>%
      select(-supplysector, subsector0 = subsector) %>%
      left_join(L2235.StubTech_elecS_cool_EUR, by = c("region", "subsector0", "stub.technology"))

    # any gcam-europe countries not in segments
    L2231.StubTechCost_onshore_wind_EUR <- L2231.StubTechCost_onshore_wind %>%
      filter(region %in% gcameurope.EUROSTAT_COUNTRIES & !region %in% L2231.StubTechCost_onshore_wind_elecS_EUR$region)

    # Simple copy of renew resources ===========================================
    L2231.SmthRenewRsrcCurves_onshore_wind_EUR <- L2231.SmthRenewRsrcCurves_onshore_wind %>%
      filter_regions_europe(regions_to_keep_name = unique(c(L2231.StubTechCapFactor_onshore_wind_elecS_EUR$region,
                                                            L2231.StubTechCapFactor_onshore_wind_EUR$region)))

    L2231.SmthRenewRsrcTechChange_onshore_wind_EUR <- L2231.SmthRenewRsrcTechChange_onshore_wind %>%
      filter_regions_europe(regions_to_keep_name = unique(c(L2231.StubTechCapFactor_onshore_wind_elecS_EUR$region,
                                                            L2231.StubTechCapFactor_onshore_wind_EUR$region)))
    # Produce outputs ===================================================
    L2231.SmthRenewRsrcCurves_onshore_wind_EUR %>%
      add_title("Smooth Renewable Resource Curve Onshore Wind") %>%
      add_units("Unitless") %>%
      add_comments("Resource curve and prices for onshore wind by region") %>%
      add_precursors("L2231.SmthRenewRsrcCurves_onshore_wind", "L2235.StubTech_elecS_cool_EUR", "gcam-europe/mappings/grid_regions") ->
      L2231.SmthRenewRsrcCurves_onshore_wind_EUR

    L2231.StubTechCapFactor_onshore_wind_EUR %>%
      add_title("Capacity factor Onshore Wind") %>%
      add_units("Unitless") %>%
      add_comments("Capital and O&M Capacity factor for onshore wind by region") %>%
      add_precursors("L2231.StubTechCapFactor_onshore_wind", "L2235.StubTech_elecS_cool_EUR", "gcam-europe/mappings/grid_regions") ->
      L2231.StubTechCapFactor_onshore_wind_EUR

    L2231.SmthRenewRsrcTechChange_onshore_wind_EUR %>%
      add_title("Smooth Renewable Resource Technological Change Onshore Wind") %>%
      add_units("Unitless") %>%
      add_comments("Technological change associated with onshore wind by region and period") %>%
      add_precursors("L2231.SmthRenewRsrcTechChange_onshore_wind", "L2235.StubTech_elecS_cool_EUR", "gcam-europe/mappings/grid_regions") ->
      L2231.SmthRenewRsrcTechChange_onshore_wind_EUR

    L2231.StubTechCost_onshore_wind_EUR %>%
      add_title("Cost of onshore wind") %>%
      add_units("Unitless") %>%
      add_comments("Regional price adjustment of input cost by region") %>%
      add_precursors("L2231.StubTechCost_onshore_wind", "L2235.StubTech_elecS_cool_EUR", "gcam-europe/mappings/grid_regions") ->
      L2231.StubTechCost_onshore_wind_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
