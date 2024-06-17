# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L126.distribution_grid_regions
#'
#' Adjust distribution losses for electricity for regions in grid regions. These losses do not need to balance because of trade
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by left_join mutate select summarise
#' @importFrom tidyr replace_na
#' @author RH February 2024
module_gcameurope_L126.distribution_grid_regions <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     FILE = "gcam-europe/mappings/grid_regions",
                     "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
                     "L1012.en_bal_EJ_R_Si_Fi_Yh")
  MODULE_OUTPUTS <- c("L126.in_EJ_R_electd_F_Yh_EUR_grid",
                      "L126.out_EJ_R_electd_F_Yh_EUR_grid",
                      "L126.IO_R_electd_F_Yh_EUR_grid")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Add in switzerland to eurostat electricity data ---------------------
    L1012.en_bal_EJ_R_Si_Fi_Yh_EUR <- replace_with_eurostat(L1012.en_bal_EJ_R_Si_Fi_Yh, L1012.en_bal_EJ_R_Si_Fi_Yh_EUR) %>%
      filter_regions_europe(regions_to_keep_name = grid_regions$region, region_ID_mapping = GCAM_region_names)

    # 2. ELECTRICITY TRANSMISSION AND DISTRIBUTION =========================================
    # electd_out is the FE consumption of electricity, without losses and ownuse
    L126.out_EJ_R_electd_F_Yh_EUR_grid <- L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(fuel == "electricity",
             grepl("^in|net", sector),
             !(grepl("ownuse|distribution", sector))) %>%
      mutate(sector = "electricity distribution") %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup

    # electd_in is the FE consumption of electricity and distribution losses
    L126.in_EJ_R_electd_F_Yh_EUR_grid <- L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(fuel == "electricity",
             grepl("^in|net", sector),
             !(grepl("ownuse", sector))) %>%
      mutate(sector = "electricity distribution") %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup

    # generate IO factors
    L126.IO_R_electd_F_Yh_EUR_grid <- L126.in_EJ_R_electd_F_Yh_EUR_grid %>%
      left_join_error_no_match(L126.out_EJ_R_electd_F_Yh_EUR_grid, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(IO = value.x / value.y) %>%
      select(-value.x, -value.y)

    # OUTPUTS ===================================================
    L126.in_EJ_R_electd_F_Yh_EUR_grid %>%
      add_title("Electricity transmission and distribution energy input") %>%
      add_units("EJ") %>%
      add_comments("Sum of electricty generation adjusted for onsite losses") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh_EUR") ->
      L126.in_EJ_R_electd_F_Yh_EUR_grid

    L126.out_EJ_R_electd_F_Yh_EUR_grid %>%
      add_title("Electricity transmission and distribution energy output") %>%
      add_units("EJ") %>%
      add_comments("Sum of electricty generation adjusted for onsite, transmission, and distribution losses") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh_EUR") ->
      L126.out_EJ_R_electd_F_Yh_EUR_grid

    L126.IO_R_electd_F_Yh_EUR_grid %>%
      add_title("Electricity transmission and distribution energy input/output ratio") %>%
      add_units("EJ") %>%
      add_comments("Energy input divided by ouput") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh_EUR") ->
      L126.IO_R_electd_F_Yh_EUR_grid

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
