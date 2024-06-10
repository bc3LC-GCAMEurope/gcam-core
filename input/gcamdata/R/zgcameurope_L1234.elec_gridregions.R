# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L1234.elec_gridregions
#'
#' Calculate electricity fuel consumption and electricity generation by grid region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the listed outputs
#' @details By grid region, calculates electricity fuel consumption and electricity generation.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @author MTB August 2018
module_gcameurope_L1234.elec_gridregions <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "gcam-europe/mappings/grid_regions",
                     FILE = "common/GCAM_region_names",
                     "L123.out_EJ_R_elec_F_Yh_EUR",
                     "L123.in_EJ_R_elec_F_Yh_EUR",
                     "L123.out_EJ_R_elec_F_Yh",
                     "L123.in_EJ_R_elec_F_Yh")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1234.in_EJ_grid_elec_F_EUR",
             "L1234.out_EJ_grid_elec_F_EUR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    grid_region <- sector <- fuel <- year <- fuel.input <- generation <- state <-
      value <- NULL  # silence package check notes

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # combine EUR data with non-EUR regions
    # to ensure switzerland, etc added to grids
    L123.out_EJ_R_elec_F_Yh_EUR <- L123.out_EJ_R_elec_F_Yh %>%
      anti_join(L123.out_EJ_R_elec_F_Yh_EUR, by = "GCAM_region_ID") %>%
      bind_rows(L123.out_EJ_R_elec_F_Yh_EUR)

    L123.in_EJ_R_elec_F_Yh_EUR <- L123.in_EJ_R_elec_F_Yh %>%
      anti_join(L123.in_EJ_R_elec_F_Yh_EUR, by = "GCAM_region_ID") %>%
      bind_rows(L123.in_EJ_R_elec_F_Yh_EUR)

    # ===================================================
    # Data Processing

    # Take in country-level data on electric power sector fuel consumption, aggregate to grid regions
    L1234.in_EJ_grid_elec_F_EUR <- L123.in_EJ_R_elec_F_Yh_EUR %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      inner_join(grid_regions,  by ="region") %>%
      group_by(grid_region, sector, fuel, year) %>%
      summarise(fuel.input = sum(value)) %>%
      ungroup()

    # Take in state-level data on electricity generation by fuel, aggregate to grid regions
    L1234.out_EJ_grid_elec_F_EUR <- L123.out_EJ_R_elec_F_Yh_EUR %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      inner_join(grid_regions,  by ="region") %>%
      group_by(grid_region, sector, fuel, year) %>%
      summarise(generation = sum(value)) %>%
      ungroup()

    # ===================================================
    # Produce outputs

    L1234.in_EJ_grid_elec_F_EUR %>%
      add_title("Fuel input into electricity by fuel and grid region") %>%
      add_units("EJ") %>%
      add_comments("Fuel input values from L123.in_EJ_state_elec_F aggregated to grid region level") %>%
      add_precursors("L123.in_EJ_R_elec_F_Yh_EUR",
                     "gcam-europe/mappings/grid_regions") ->
      L1234.in_EJ_grid_elec_F_EUR

    L1234.out_EJ_grid_elec_F_EUR %>%
      add_title("Electricity generation by fuel and grid region") %>%
      add_units("EJ") %>%
      add_comments("Electricity generation (output) values from L123.out_EJ_state_elec_F aggregated to grid region level") %>%
      add_precursors("L123.out_EJ_R_elec_F_Yh_EUR",
                     "gcam-europe/mappings/grid_regions") ->
      L1234.out_EJ_grid_elec_F_EUR

    return_data(L1234.in_EJ_grid_elec_F_EUR, L1234.out_EJ_grid_elec_F_EUR)
  } else {
    stop("Unknown command")
  }
}
