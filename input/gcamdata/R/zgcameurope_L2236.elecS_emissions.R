# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L2236.elecS_emissions
#'
#' European electricity non CO2 emissions (GHG and air pollutants) and future emission coefficients by sector, fuel and cooling techs
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs
#' @details Write electricity emission coefficients to multiple load segments and cooling technology
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KD August 2018 / YO March 2022 / RH Sept 2024
module_gcameurope_L2236.elecS_emissions <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "energy/A23.globaltech_input_driver",
                     "L223.elec_tech_water_map_EUR",
                     FILE = "gcam-europe/A23.elecS_naming",
                     FILE = "common/GCAM_region_names",
                     'L1231.in_EJ_R_elec_F_tech_Yh_EUR',
                     'L201.OutputEmissions_elec',
                     'L201.OutputEmissions_elec_EUR',
                     'L241.OutputEmissCoeff_elec',
                     'L241.OutputEmissCoeff_elec_EUR',
                     'L201.nonghg_max_reduction',
                     'L201.nonghg_max_reduction_EUR',
                     'L201.nonghg_steepness',
                     'L201.nonghg_steepness_EUR',
                     'L241.nonco2_max_reduction',
                     'L241.nonco2_max_reduction_EUR',
                     'L241.nonco2_steepness',
                     'L241.nonco2_steepness_EUR',
                     'L253.EF_NSPS_new_vintage',
                     'L2234.StubTechCost_offshore_wind_elecS_EUR',
                     'L2234.GlobalTechOMfixed_elecS_EUR',
                     'L2235.StubTech_elecS_cool_EUR',
                     'L2235.StubTechCalInput_elecS_cool_EUR')
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c('L2236.elecS_cool_ghg_tech_coeff_EUR',
             'L2236.elecS_cool_ghg_emissions_EUR',
             'L2236.elecS_cool_ctrl_max_EUR',
             'L2236.elecS_cool_ctrl_steep_EUR',
             'L2236.elecS_cool_EF_NSPS_EUR'))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Electricity emissions (greenhouse gases and air pollutants) are taken from the GCAM-Europe tables when the region
    # is there, and from the core tables otherwise (e.g. Switzerland, which is segmented but is not a Eurostat country)
    L201.OutputEmissions_elec_all <- bind_rows(
      L201.OutputEmissions_elec_EUR,
      L201.OutputEmissions_elec %>% anti_join(distinct(L201.OutputEmissions_elec_EUR, region), by = "region"))
    L241.OutputEmissCoeff_elec_all <- bind_rows(
      L241.OutputEmissCoeff_elec_EUR,
      L241.OutputEmissCoeff_elec %>% anti_join(distinct(L241.OutputEmissCoeff_elec_EUR, region), by = "region"))

    # Electricity technology to water type mapping with the offshore wind technologies of GCAM-Europe
    elec_tech_water_map <- L223.elec_tech_water_map_EUR

    battery_mapping <- L2234.GlobalTechOMfixed_elecS_EUR %>%  filter(grepl("battery", technology)) %>%
      distinct(subsector.name, technology) %>%
      mutate(to.technology = technology)

    elec_cool_expansion <- distinct(elec_tech_water_map, subsector.name = from.subsector,
                                    technology = to.subsector, to.technology, from.technology) %>%
      repeat_add_columns(distinct(A23.elecS_naming, name_adder)) %>%
      # storage, wind , and solar tech names need to be taken from from.technology, other techs from.technology col
      mutate(technology = if_else(grepl("storage|solar|wind", technology),
                                  paste(from.technology, name_adder, sep = "_"),
                                  # don't want to change rooftop
                                  if_else(subsector.name == "rooftop_pv", technology,
                                          # All other cases
                                          paste(technology, name_adder, sep = "_")))) %>%
      select(subsector.name, technology, to.technology) %>%
      # add in battery
      bind_rows(battery_mapping) %>%
      distinct()

    # 1. Define functions ===========================================================================================
    add_global_cooling_techs <- function(data){
      subsector_name = names(data)[which(grepl("subsector", names(data)))]
      tech_name = names(data)[which(grepl("technology", names(data)))]
      subsector0_name = paste0(subsector_name, "0")
      data %>%
        left_join(elec_cool_expansion,
                  by = structure(names = c(subsector_name, tech_name), .Data = c("subsector.name", "technology"))) %>%
        mutate(!!subsector0_name := get(subsector_name),
               !!subsector_name := get(tech_name),
               !!tech_name := to.technology) %>%
        select(-to.technology)
    }

    # Keep only technologies that really exist in the segmented structure (e.g. seawater cooling
    # does not exist in non-coastal countries): avoids 'nocreate' warnings when parsing the XMLs
    keep_existing_techs <- function(data){
      data %>%
        semi_join(L2235.StubTech_elecS_cool_EUR,
                  by = c("region", "supplysector", "subsector", "stub.technology"))
    }

    # 2a. Emission coefficients ===========================================================================================
    # Write electricity emission coefficients to multiple load segments
    L2234.load_segments <- distinct(A23.elecS_naming, supplysector)

    L2236.elecS_cool_ghg_tech_coeff_EUR <- L241.OutputEmissCoeff_elec_all %>%
      semi_join(L2235.StubTech_elecS_cool_EUR, by = "region") %>%
      filter(supplysector == "electricity") %>%
      expand_to_segments(group_by_cols = c("subsector", "stub.technology"),
                         segments = L2234.load_segments$supplysector) %>%
      tech_name_expansion(tech = "stub.technology", mapping = A23.elecS_naming) %>%
      add_global_cooling_techs() %>%
      keep_existing_techs()

    # 2b. Input Emissions ===========================================================================
    # Calibrated input emissions of N2O and CH4 by country
    # Filter the emissions data for Europe & electricity sector

    # Define countries that have access to offshore wind as allowing for seawater cooling
    seawater_countries <- unique(L2234.StubTechCost_offshore_wind_elecS_EUR$region)

    # Expand emissions to cooling techs and segments
    L2236.elec_ghg_emissions_EUR <- L201.OutputEmissions_elec_all %>%
      semi_join(L2235.StubTech_elecS_cool_EUR, by = "region") %>%
      filter(grepl("electricity", supplysector)) %>%
      mutate(old.tech = stub.technology) %>%
      expand_to_segments(group_by_cols = c("subsector", "stub.technology"),
                         segments = L2234.load_segments$supplysector) %>%
      tech_name_expansion(tech = "stub.technology", mapping = A23.elecS_naming) %>%
      add_global_cooling_techs() %>%
      filter(!(grepl("seawater|wind_offshore", stub.technology) & !region %in% seawater_countries)) %>%
      keep_existing_techs()

    # Share out CH4 and N2O emissions by region based on the fuel input shares
    L2236.elecS_cool_ghg_emissions_EUR <- L2236.elec_ghg_emissions_EUR %>%
      left_join_error_no_match(L2235.StubTechCalInput_elecS_cool_EUR %>%  select(LEVEL2_DATA_NAMES[["StubTechYr"]], subsector0, calibrated.value),
                               by = c("region", "supplysector", "subsector", "stub.technology", "year", "subsector0")) %>%
      # Get shares of input by old.tech
      group_by(region, old.tech, Non.CO2, year) %>%
      mutate(input.emissions = round((calibrated.value / sum(calibrated.value)) * input.emissions, emissions.DIGITS_EMISSIONS)) %>%
      ungroup %>%
      replace_na(list(input.emissions = 0))

    # Cap implausible implied emission factors. In the base year, GCAM derives the future emission factor of each
    # technology as emissions / output. When a country reports emissions for a fuel that has (almost) no generation
    # (e.g. biomass in Serbia and Montenegro, ~1e-9 EJ), the implied factor explodes (hundreds of Tg per EJ) as soon as
    # that technology grows. For each region, fuel technology, gas and year, if the implied factor exceeds
    # `ef_cap_multiple` times the European median (computed only on regions with a meaningful fuel input), the emissions
    # are scaled down proportionally across segments and cooling technologies (their shares are kept). Only the last
    # calibration year is capped, since it is the one that defines the future emission factors.
    # Air pollutants vary much more between countries (abatement equipment), so their limit is wider
    ef_cap_multiple_ghg <- 10
    ef_cap_multiple_pol <- 100
    ef_min_input <- 1e-3  # EJ of fuel input needed to enter the European median

    L2236.ef_median <- L2236.elecS_cool_ghg_emissions_EUR %>%
      group_by(region, old.tech, Non.CO2, year) %>%
      summarise(emiss = sum(input.emissions), input = sum(calibrated.value), .groups = "drop") %>%
      filter(input > ef_min_input, emiss > 0) %>%
      mutate(ef = emiss / input) %>%
      group_by(old.tech, Non.CO2, year) %>%
      summarise(ef_median = median(ef), .groups = "drop")

    L2236.elecS_cool_ghg_emissions_EUR <- L2236.elecS_cool_ghg_emissions_EUR %>%
      group_by(region, old.tech, Non.CO2, year) %>%
      mutate(reg_input = sum(calibrated.value), reg_emiss = sum(input.emissions)) %>%
      ungroup %>%
      left_join(L2236.ef_median, by = c("old.tech", "Non.CO2", "year")) %>%
      mutate(ef_cap_multiple = if_else(Non.CO2 %in% emissions.GHG_NAMES, ef_cap_multiple_ghg, ef_cap_multiple_pol),
             max_emiss = if_else(is.na(ef_median), Inf, ef_cap_multiple * ef_median * reg_input),
             emiss_scale = if_else(year == max(year) & reg_emiss > max_emiss, max_emiss / reg_emiss, 1),
             input.emissions = round(input.emissions * emiss_scale, emissions.DIGITS_EMISSIONS)) %>%
      select(-old.tech, -calibrated.value, -reg_input, -reg_emiss, -ef_median, -ef_cap_multiple, -max_emiss, -emiss_scale)

    # Confirm that emissions match
    new_sum <- L2236.elecS_cool_ghg_emissions_EUR %>%
      group_by(region, subsector = subsector0, Non.CO2, year) %>%
      summarise(value_new = round(sum(input.emissions), 6)) %>%
      ungroup

    # Not going to throw error, as there are less than 10 differences, all quite small
    check <- L201.OutputEmissions_elec_all %>%
      filter(grepl("electricity", supplysector)) %>%
      semi_join(L2235.StubTech_elecS_cool_EUR, by = "region") %>%
      group_by(region, subsector, Non.CO2, year) %>%
      summarise(value = round(sum(input.emissions), 6)) %>%
      ungroup %>%
      left_join_error_no_match(new_sum, by = c("region", "subsector", "Non.CO2", "year")) %>%
      filter(abs(value - value_new) > 1e-6)

    # 3. Emission controls ===========================================================================================
    # GDP control parameters (existing technologies: L201, new technologies: L241) and new source performance
    # standards (L253) are expanded to load segments and cooling technologies like the emissions and the
    # coefficients. GCAM-Europe tables are used where available, core tables for the other segmented regions.
    bind_eur_core <- function(eur, core) {
      bind_rows(eur, core %>% anti_join(distinct(eur, region), by = "region"))
    }

    expand_ctrl <- function(df) {
      df %>%
        select(-any_of(c("fuel", "nesting.subsector"))) %>%
        filter(supplysector == "electricity") %>%
        semi_join(L2235.StubTech_elecS_cool_EUR, by = "region") %>%
        expand_to_segments(group_by_cols = c("subsector", "stub.technology"),
                           segments = L2234.load_segments$supplysector) %>%
        tech_name_expansion(tech = "stub.technology", mapping = A23.elecS_naming) %>%
        add_global_cooling_techs() %>%
        keep_existing_techs()
    }

    L2236.elecS_cool_ctrl_max_EUR <- bind_rows(
      bind_eur_core(L201.nonghg_max_reduction_EUR, L201.nonghg_max_reduction),
      bind_eur_core(L241.nonco2_max_reduction_EUR, L241.nonco2_max_reduction)) %>%
      expand_ctrl()

    L2236.elecS_cool_ctrl_steep_EUR <- bind_rows(
      bind_eur_core(L201.nonghg_steepness_EUR, L201.nonghg_steepness),
      bind_eur_core(L241.nonco2_steepness_EUR, L241.nonco2_steepness)) %>%
      expand_ctrl()

    L2236.elecS_cool_EF_NSPS_EUR <- expand_ctrl(L253.EF_NSPS_new_vintage)

    # Check for missing values
    stopifnot(!any(is.na(L2236.elecS_cool_ctrl_max_EUR)))
    stopifnot(!any(is.na(L2236.elecS_cool_ctrl_steep_EUR)))
    stopifnot(!any(is.na(L2236.elecS_cool_EF_NSPS_EUR)))
    stopifnot(!any(is.na(L2236.elecS_cool_ghg_tech_coeff_EUR)))
    stopifnot(!any(is.na(L2236.elecS_cool_ghg_emissions_EUR)))

    # Produce outputs ===========================================================================================

    L2236.elecS_cool_ghg_tech_coeff_EUR %>%
      add_title("Europe electricity GHG emission coefficients by technology sector") %>%
      add_units("NA") %>%
      add_comments("Write electricity emission coefficients to multiple load segments then") %>%
      add_comments("compute shares for each category in the fuel input table") %>%
      add_precursors(MODULE_INPUTS) ->
      L2236.elecS_cool_ghg_tech_coeff_EUR

    L2236.elecS_cool_ghg_emissions_EUR %>%
      add_title("Europe electricity non CO2 emission coefficients by technology sector") %>%
      add_units("NA") %>%
      add_comments("Write electricity emission coefficients to multiple load segments then") %>%
      add_comments("compute state shares for each category in the fuel input table") %>%
      add_precursors(MODULE_INPUTS) ->
      L2236.elecS_cool_ghg_emissions_EUR

    L2236.elecS_cool_ctrl_max_EUR %>%
      add_title("Europe electricity GDP control (maximum reduction) by technology sector") %>%
      add_units("percent") %>%
      add_comments("GDP control parameters of existing and new technologies expanded to load segments and cooling technologies") %>%
      add_precursors(MODULE_INPUTS) ->
      L2236.elecS_cool_ctrl_max_EUR

    L2236.elecS_cool_ctrl_steep_EUR %>%
      add_title("Europe electricity GDP control (steepness) by technology sector") %>%
      add_units("NA") %>%
      add_comments("GDP control parameters of existing and new technologies expanded to load segments and cooling technologies") %>%
      add_precursors(MODULE_INPUTS) ->
      L2236.elecS_cool_ctrl_steep_EUR

    L2236.elecS_cool_EF_NSPS_EUR %>%
      add_title("Europe electricity new source performance standards by technology sector") %>%
      add_units("Tg/EJ") %>%
      add_comments("Emission coefficients of new vintages expanded to load segments and cooling technologies") %>%
      add_precursors(MODULE_INPUTS) ->
      L2236.elecS_cool_EF_NSPS_EUR

    return_data(L2236.elecS_cool_ghg_tech_coeff_EUR,
                L2236.elecS_cool_ghg_emissions_EUR,
                L2236.elecS_cool_ctrl_max_EUR,
                L2236.elecS_cool_ctrl_steep_EUR,
                L2236.elecS_cool_EF_NSPS_EUR)

  } else {
    stop("Unknown command")
  }
}
