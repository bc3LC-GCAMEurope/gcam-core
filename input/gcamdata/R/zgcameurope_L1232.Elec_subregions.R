# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L1232.Elec_subregions
#'
#' Aggregates European country electricity generation to electricity subregions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1232.out_EJ_sR_elec}. The corresponding file in the
#' original data system was \code{LB1232.Elec_subregions.R} (gcam-usa level1).
#' @details Aggregates USA state electricity generation to electricity subregions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr group_by left_join select summarise
#' @author RLH September 2017
module_gcameurope_L1232.Elec_subregions <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-europe/mappings/grid_regions",
             FILE = "common/GCAM_region_names",
             "L1231.out_EJ_R_elec_F_tech_Yh_EUR",
             "L1231.out_EJ_R_elec_F_tech_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1232.out_EJ_sR_elec_EUR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    state <- grid_region <- year <- value <- sector <- NULL

    # Load required inputs
    grid_regions <- get_data(all_data, "gcam-europe/mappings/grid_regions")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    L1231.out_EJ_R_elec_F_tech_Yh_EUR <- get_data(all_data, "L1231.out_EJ_R_elec_F_tech_Yh_EUR")
    L1231.out_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.out_EJ_R_elec_F_tech_Yh")

    # combine EUR data with non-EUR regions
    # to ensure switzerland, etc added to grids
    L1231.out_EJ_R_elec_F_tech_Yh <- replace_with_eurostat(L1231.out_EJ_R_elec_F_tech_Yh, L1231.out_EJ_R_elec_F_tech_Yh_EUR)

    # ===================================================
    # Aggregating states to electricity subregions
    L1232.out_EJ_sR_elec_EUR <- L1231.out_EJ_R_elec_F_tech_Yh %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # Want to drop Belarus, Ukraine, Turkey, Moldova, Iceland
      inner_join(grid_regions, by = "region") %>%
      group_by(grid_region, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      # ===================================================
    # Produce outputs
    add_title("Electricity generation by grid region/fuel/technology") %>%
      add_units("EJ") %>%
      add_comments("L1231.out_EJ_R_elec_F_tech_Yh_EUR aggregated to grid region") %>%
      add_precursors("gcam-europe/mappings/grid_regions",
                     "L1231.out_EJ_R_elec_F_tech_Yh_EUR")

    return_data(L1232.out_EJ_sR_elec_EUR)
  } else {
    stop("Unknown command")
  }
}
