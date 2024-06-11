# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L1239.elec_ctry_fractions
#'
#' Map electricity generation by fuel | grid region | horizontal segment to generation by fuel | country | segment.
#' The fraction of generation by fuel by horizontal segment is assumed to be equal for all countries within a grid region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1239.R_elec_supply}.
#'
#' The corresponding file in the original data system was \code{LB1239.elec_state_fractions.R} (gcam-usa level1).
#' @details Calculates the fraction of electricity generation by fuel, by horizontal load segment, by state.
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter left_join mutate select semi_join
#' @author MTB August 2018
module_gcameurope_L1239.elec_ctry_fractions <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     FILE = "gcam-europe/mappings/grid_regions",
                     "L123.out_EJ_R_elec_F_Yh_EUR",
                     "L123.out_EJ_R_elec_F_Yh",
                     "L1236.grid_elec_supply_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1239.R_elec_supply"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    grid_region <- segment <- fuel <- year <- tot_generation <- fraction <- generation <-
      state <- sector <- value <- NULL # silence package check notes

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # combine EUR data with non-EUR regions
    # to ensure switzerland, etc added to grids
    L123.out_EJ_R_elec_F_Yh_EUR <- L123.out_EJ_R_elec_F_Yh %>%
      anti_join(L123.out_EJ_R_elec_F_Yh_EUR, by = "GCAM_region_ID") %>%
      bind_rows(L123.out_EJ_R_elec_F_Yh_EUR)

    # ===================================================
    # Data Processing
    # Create table of electricity generation by load segment | fuel | state
    # L123.out_EJ_R_elec_F_Yh_EUR contains electricity generation by fuel & state
    L1239.R_elec_supply <- L123.out_EJ_R_elec_F_Yh_EUR %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      semi_join(grid_regions, by = c("region")) %>%
      # The PV / CSP distinction does not matter for allocating electricity
      # generation by fuel across load segments, so assign both to "solar"
      mutate(fuel = sub("solar CSP", "solar", fuel),
             fuel = sub("solar PV", "solar", fuel)) %>%
      group_by(GCAM_region_ID, region, sector, fuel, year) %>%
      summarise(tot_generation = sum(value)) %>%
      ungroup() %>%
      # filter out years which are not present in the electricity load segments calibration data table
      semi_join(L1236.grid_elec_supply_EUR, by = "year") %>%
      left_join_error_no_match(grid_regions, by = "region") %>%
      # map fuel shares by horizontal load segment and grid to the states
      # joining L1236.grid_elec_supply_EUR is intended to duplicate rows,
      # creating four rows for every state | fuel | year (one row per load segment)
      # left_join_error_no_match throws error when number of rows changes, so left_join is used
      left_join(L1236.grid_elec_supply_EUR %>%
                  select(grid_region, segment, fuel, year, fraction),
                by = c("grid_region", "fuel", "year")) %>%
      mutate(segment = if_else(is.na(segment) & tot_generation == 0, "base load generation", segment),
             fraction = if_else(is.na(fraction) & tot_generation == 0, 0, fraction),
             generation = tot_generation * fraction) %>%
      select(GCAM_region_ID, grid_region, segment, fuel, year, tot_generation, fraction, generation)

    # ===================================================

    # Produce outputs

    L1239.R_elec_supply %>%
      add_title("Electricity supply by fuel by horizontal load segment in each state.") %>%
      add_units("EJ; unitless (fraction)") %>%
      add_comments("Electricity generation by fuel & state (from L123.out_EJ_state_elec_F) allocated across horizontal load segments.") %>%
      add_comments("This allocation is based on the fraction of fuel in the horizontal load segments by grid region (from L1236.grid_elec_supply_EUR).") %>%
      add_legacy_name("L1239.state_elec_supply") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L123.out_EJ_state_elec_F",
                     "L1236.grid_elec_supply_EUR") ->
      L1239.R_elec_supply

    return_data(L1239.R_elec_supply)

  } else {
    stop("Unknown command")
  }
}
