# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L1233.elec_water_grid
#'
#' Compute water withdrawals/consumption by state, fuel, technology, and cooling system type.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1233.out_EJ_ctry_elec_F_tech_cool_EUR}, \code{L1233.share_sR_elec_F_tech_cool_EUR}. The corresponding file in the
#' original data system was \code{LB1233.Elec_water.R} (gcam-usa level1).
#' @details Compute water withdrawals/consumption by state, fuel, technology, and cooling system type.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ST October 2017, NTG May 2020, RLH June 2024
module_gcameurope_L1233.elec_water_grid <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "gcam-europe/mappings/grid_regions",
                     FILE = "common/GCAM_region_names",
                     FILE = "water/elec_tech_water_map",
                     FILE = "gcam-europe/A23.elecS_naming",
                     "L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR",
                     "L1233.out_EJ_R_elec_F_tech_Yh_cool",
                     "L1239.R_elec_supply")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1233.out_EJ_R_elecS_F_tech_cool_EUR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # ===================================================
    # Add in solar techs
    L1239.R_elec_supply <- L1239.R_elec_supply %>%
      left_join(distinct(L1233.out_EJ_R_elec_F_tech_Yh_cool, fuel_new = fuel) %>% filter(grepl("solar", fuel_new)) %>%
                  mutate(fuel = "solar"), by = "fuel") %>%
      mutate(fuel = if_else(!is.na(fuel_new), fuel_new, fuel)) %>%
      select(GCAM_region_ID, segment, fuel, year, fraction) %>%
      # ensure all segment fuel combinations present
      complete(nesting(GCAM_region_ID, segment, year), fuel) %>%
      tidyr::replace_na(list(fraction = 0))

    # fix solar names here too
    elec_tech_water_map <- elec_tech_water_map %>%
      mutate(from.subsector = if_else(from.subsector == "solar", fuel, from.subsector))

    # combine EUR data with non-EUR regions
    # to ensure switzerland, etc added to grids
    L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR <- replace_with_eurostat(L1233.out_EJ_R_elec_F_tech_Yh_cool, L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR) %>%
      filter_regions_europe(regions_to_keep_name = grid_regions$region, region_ID_mapping = GCAM_region_names)

    # add in segment options to cooling techs
    L1233.out_EJ_R_elecS_F_tech_cool_EUR <- L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR %>%
      left_join_error_no_match(elec_tech_water_map %>% select(from.subsector, from.technology, cooling_system, water_type, to.technology),
                by = c("fuel" = "from.subsector", "technology" = "from.technology", "cooling_system", "water_type")) %>%
      # filter to years and IDs from L1239.R_elec_supply, while adding in segments
      inner_join(L1239.R_elec_supply, by = c("GCAM_region_ID", "fuel", "year")) %>%
      left_join_error_no_match(A23.elecS_naming, by = c("segment" = "supplysector")) %>%
      mutate(value = value * fraction,
             subsector = paste(technology, name_adder, sep = "_")) %>%
      select(GCAM_region_ID, fuel, supplysector = segment, subsector, technology = to.technology, year, value)

    # Produce outputs ===================================================
    L1233.out_EJ_R_elecS_F_tech_cool_EUR %>%
      add_title("Electricity output by state / fuel / technology / cooling system / water type") %>%
      add_units("EJ") %>%
      add_comments("Electricity output by state / fuel / technology / cooling system / water type") %>%
      add_precursors(MODULE_INPUTS) ->
      L1233.out_EJ_R_elecS_F_tech_cool_EUR

    return_data(L1233.out_EJ_R_elecS_F_tech_cool_EUR)
  } else {
    stop("Unknown command")
  }
}
