# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L144.building_det_heatpumps
#'
#' Calculates EUR detailed heatpumps energy data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.in_EJ_R_bld_serv_complete_F_Yh_EUR} (energy level1.5).
#' @details Calculates building energy consumption, non-energy costs, energy output by service, internal gains, and end-use technology and shell efficiency
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join lag mutate pull select summarise
#' @importFrom tidyr complete replace_na
#' @author CRB January 2025
module_gcameurope_L144.building_det_heatpumps <- function(command, ...) {
  MODULE_INPUTS <- c(
    FILE = "common/GCAM32_to_EU",
    FILE = "gcam-europe/calibrated_techs_bld_det_EUR",
    FILE = "gcam-europe/estat_nrg_ind_ahbtc_filtered_en",
    FILE = "gcam-europe/mappings/geo_to_climate_map",
    FILE = "gcam-europe/mappings/geo_to_iso_map",
    FILE = "gcam-europe/mappings/heatpump_to_tech_map",
    FILE = "gcam-europe/A44.cost_efficiency_EUR",
    "L144.in_EJ_R_bld_serv_F_Yh_EUR")
  MODULE_OUTPUTS <- c(
    "L144.in_EJ_R_bld_serv_complete_F_Yh_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # Silence package checks
    building.service.input <- NULL


    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # Compute ctry specific ambient heat given by Eurostat - not considering 'solar thermal'
    L144.ambient_heat <- estat_nrg_ind_ahbtc_filtered_en %>%
      select(-OBS_FLAG) %>%
      left_join_error_no_match(heatpump_to_tech_map, by = c('hp_tech' = 'nrg_bal')) %>%
      filter(nchar(geo) == 2) %>%
      left_join_error_no_match(geo_to_climate_map, by = c('geo')) %>%
      # delete Georgia (non EUR region)
      filter(geo != 'GE') %>%
      mutate(technology = if_else(tech != 'geo-water pump', paste(tech, climate_group), tech)) %>%
      group_by(unit, geo, year = TIME_PERIOD, value = OBS_VALUE, subsector, technology) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Compute energy used by tech: en_used * efficiency = ambient_heat
    L144.en_used <- L144.ambient_heat %>%
      left_join(A44.cost_efficiency_EUR, by = c('subsector','technology'), relationship = "many-to-many") %>%
      # from GWH to EJ/yr
      mutate(en = (value / efficiency)  * 3.6e-6) %>%
      left_join_error_no_match(geo_to_iso_map, by = 'geo') %>%
      left_join_error_no_match(GCAM32_to_EU %>%
                                 filter(GCAMEU_region != GCAM32_region),
                               by = 'iso') %>%
      select(supplysector, subsector, technology, value = en, GCAM_region_ID, year)

    # Assuming we divide equally the energy consumption among all technologies (non heat pumps),
    # reduce the heating energy consumption, and if necessary, the cooking energy consumption
    L144.in_EJ_R_bld_serv_elec_F_Yh_EUR <- L144.in_EJ_R_bld_serv_F_Yh_EUR %>%
      filter(fuel == 'electricity') %>%
      left_join(L144.en_used %>%
                  group_by(GCAM_region_ID, year, service = supplysector) %>%
                  summarise(value = sum(value)) %>%
                  ungroup(),
                by = c('GCAM_region_ID','year','service')) %>%
      # divide equally the heat pump energy among the technologies
      mutate(value.y = if_else(is.na(value.y), 0, value.y)) %>%
      mutate(value = value.x - value.y) %>%
      # if heat pumps energy is > than consumed heat, we reduce the remaining energy from cooking
      mutate(adj_cooking = if_else(value < 0, value, 0),
             value = if_else(value < 0, 0, value)) %>%
      select(-value.x, -value.y)

    L144.in_EJ_R_bld_serv_complete_F_Yh_EUR <-
      bind_rows(L144.in_EJ_R_bld_serv_F_Yh_EUR %>%
                  filter(fuel != 'electricity') %>%
                  mutate(adj_cooking = 0),
                L144.in_EJ_R_bld_serv_elec_F_Yh_EUR) %>%
      mutate(value = if_else(service == 'resid cooking modern EUR',
                             value + adj_cooking, value)) %>% # adj_cooking is already negative
      select(-adj_cooking)


    # OUTPUTS ===================================================

    L144.in_EJ_R_bld_serv_complete_F_Yh_EUR %>%
      add_title("Building energy consumption by GCAM region ID / sector / fuel / service / historical year adjusted by heat pumps ambient heat/eff/en") %>%
      add_units("EJ/yr") %>%
      add_comments("Energy consumption by service is calculated by allocating energy consumption across services using calculated service shares") %>%
      add_legacy_name("L144.in_EJ_R_bld_serv_complete_F_Yh_EUR") %>%
      add_precursors("common/GCAM32_to_EU", "gcam-europe/calibrated_techs_bld_det_EUR", "gcam-europe/estat_nrg_ind_ahbtc_filtered_en",
                     "gcam-europe/mappings/geo_to_climate_map", "gcam-europe/mappings/geo_to_iso_map", "gcam-europe/mappings/heatpump_to_tech_map",
                     "gcam-europe/A44.cost_efficiency_EUR","L144.in_EJ_R_bld_serv_F_Yh_EUR") ->
      L144.in_EJ_R_bld_serv_complete_F_Yh_EUR


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
