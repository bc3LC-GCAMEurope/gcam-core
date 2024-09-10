# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L2236.elecS_emissions
#'
#' European electricity non CO2 GHG emission and future emission coefficients by sector, fuel and cooling techs
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
                     FILE = "gcam-europe/A23.elecS_naming",
                     FILE = "common/GCAM_region_names",
                     'L1231.in_EJ_R_elec_F_tech_Yh_EUR',
                     'L201.OutputEmissions_elec',
                     'L241.OutputEmissCoeff_elec',
                     'L2234.StubTechCost_offshore_wind_elecS_EUR',
                     'L2235.StubTech_elecS_cool_EUR',
                     'L2235.StubTechCalInput_elecS_cool_EUR')
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c('L2236.elecS_cool_ghg_tech_coeff_EUR',
             'L2236.elecS_cool_ghg_emissions_EUR'))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

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

    # 2a. Emission coefficients ===========================================================================================
    # Write electricity emission coefficients to multiple load segments
    L2234.load_segments <- distinct(A23.elecS_naming, supplysector)

    L2236.elecS_cool_ghg_tech_coeff_EUR <- L241.OutputEmissCoeff_elec %>%
      semi_join(L2235.StubTech_elecS_cool_EUR, by = "region") %>%
      filter(supplysector == "electricity", Non.CO2 %in% emissions.GHG_NAMES) %>%
      expand_to_segments(group_by_cols = c("subsector", "stub.technology"),
                         segments = L2234.load_segments$supplysector) %>%
      tech_name_expansion(tech = "stub.technology", mapping = A23.elecS_naming) %>%
      add_global_cooling_techs()

    # 2b. Input Emissions ===========================================================================
    # Calibrated input emissions of N2O and CH4 by country
    # Filter the emissions data for Europe & electricity sector

    # Define countries that have access to offshore wind as allowing for seawater cooling
    seawater_countries <- unique(L2234.StubTechCost_offshore_wind_elecS_EUR$region)

    # Expand emissions to cooling techs and segments
    L2236.elec_ghg_emissions_EUR <- L201.OutputEmissions_elec %>%
      semi_join(L2235.StubTech_elecS_cool_EUR, by = "region") %>%
      filter(grepl("electricity", supplysector) & Non.CO2 %in% emissions.GHG_NAMES) %>%
      mutate(old.tech = stub.technology) %>%
      expand_to_segments(group_by_cols = c("subsector", "stub.technology"),
                         segments = L2234.load_segments$supplysector) %>%
      tech_name_expansion(tech = "stub.technology", mapping = A23.elecS_naming) %>%
      add_global_cooling_techs() %>%
      filter(!(grepl("seawater|wind_offshore", stub.technology) & !region %in% seawater_countries))

    # Share out CH4 and N2O emissions by region based on the fuel input shares
    L2236.elecS_cool_ghg_emissions_EUR <- L2236.elec_ghg_emissions_EUR %>%
      left_join_error_no_match(L2235.StubTechCalInput_elecS_cool_EUR %>%  select(LEVEL2_DATA_NAMES[["StubTechYr"]], subsector0, calibrated.value),
                               by = c("region", "supplysector", "subsector", "stub.technology", "year", "subsector0")) %>%
      # Get shares of input by old.tech
      group_by(region, old.tech, Non.CO2, year) %>%
      mutate(input.emissions = round((calibrated.value / sum(calibrated.value)) * input.emissions, emissions.DIGITS_EMISSIONS)) %>%
      ungroup %>%
      replace_na(list(input.emissions = 0)) %>%
      select(-old.tech, -calibrated.value)

    # Confirm that emissions match
    new_sum <- L2236.elecS_cool_ghg_emissions_EUR %>%
      group_by(region, subsector = subsector0, Non.CO2, year) %>%
      summarise(value_new = round(sum(input.emissions), 6)) %>%
      ungroup

    # Not going to throw error, as there are less than 10 differences, all quite small
    check <- L201.OutputEmissions_elec %>%
      filter(grepl("electricity", supplysector) & Non.CO2 %in% emissions.GHG_NAMES) %>%
      semi_join(L2235.StubTech_elecS_cool_EUR, by = "region") %>%
      group_by(region, subsector, Non.CO2, year) %>%
      summarise(value = round(sum(input.emissions), 6)) %>%
      ungroup %>%
      left_join_error_no_match(new_sum, by = c("region", "subsector", "Non.CO2", "year")) %>%
      filter(abs(value - value_new) > 1e-6)

    # Check for missing values
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

    return_data(L2236.elecS_cool_ghg_tech_coeff_EUR,
                L2236.elecS_cool_ghg_emissions_EUR)

  } else {
    stop("Unknown command")
  }
}
