# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L123.electricity
#'
#' This script creates electricity generation and inputs by fuel, region and historical year. Estimates are adjusted by efficiency factors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L123.out_EJ_R_elec_F_Yh_EUR}, \code{L123.in_EJ_R_elec_F_Yh_EUR}, \code{L123.eff_R_elec_F_Yh_EUR}, \code{L123.out_EJ_R_indchp_F_Yh_EUR}, \code{L123.in_EJ_R_indchp_F_Yh_EUR}. The corresponding file in the
#' original data system was \code{LA123.electricity.R} (energy level1).
#' @details This script creates electricity generation and inputs by fuel, region and historical year. Estimates are adjusted by efficiency factors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter full_join if_else group_by left_join mutate select semi_join summarise summarise_all
#' @author RH February 2024
module_gcameurope_L123.electricity <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/mappings/enduse_fuel_aggregation",
             FILE = "energy/A23.chp_elecratio",
             "L101.en_bal_EJ_R_Si_Fi_Yh_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L123.out_EJ_R_elec_F_Yh_EUR",
             "L123.in_EJ_R_elec_F_Yh_EUR",
             "L123.eff_R_elec_F_Yh_EUR",
             "L123.out_EJ_R_indchp_F_Yh_EUR",
             "L123.in_EJ_R_indchp_F_Yh_EUR"))
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- fuel <- sector <- electricity <-
      outputs <- input <- outputs_new <- industry <- elec_ratio <-
      outputs_ratio <- NULL           # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")
    A23.chp_elecratio <- get_data(all_data, "energy/A23.chp_elecratio")
    L101.en_bal_EJ_R_Si_Fi_Yh_EUR <- get_data(all_data, "L101.en_bal_EJ_R_Si_Fi_Yh_EUR")


    # 1. Electricity ===================================================

    # Creates end use fuel for electricity to be used to create L123.in_EJ_R_elec_F_Yh_EUR
    enduse_fuel_aggregation_electricity <- enduse_fuel_aggregation %>%
      select(fuel,electricity) %>%
      filter(!is.na(electricity))

    # Creates L123.in_EJ_R_elec_F_Yh_EUR based on L101.en_bal_EJ_R_Si_Fi_Yh_EUR and enduse_fuel_aggregation_electricity
    # Calculates the inputs by fuel (based on electricity input fuels), region ID and sector (electricity)
    L123.in_EJ_R_elec_F_Yh_EUR <- L101.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "in_electricity generation",
             !fuel %in% c("heat", "electricity")) %>%
      mutate(sector = "electricity generation") %>%
      left_join_error_no_match(enduse_fuel_aggregation_electricity, by = "fuel") %>%
      select(GCAM_region_ID, sector, fuel = electricity, year, input = value) %>%
      filter(fuel %in% energy.ELECTRICITY_INPUT_FUELS) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise_all(sum) %>%
      ungroup()

    # Creates L123.out_EJ_R_elec_F_Yh_EUR based on L101.en_bal_EJ_R_Si_Fi_Yh_EUR and enduse_fuel_aggregation_electricity
    # Calculates the electricity outputs by fuel, region ID and sector (electricity generation)
    L123.out_EJ_R_elec_F_Yh_preadj_EUR <- L101.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "out_electricity generation",
             !fuel %in% c("heat", "electricity")) %>%
      mutate(sector = "electricity generation") %>%
      left_join_error_no_match(enduse_fuel_aggregation_electricity, by = "fuel") %>%
      select(GCAM_region_ID, sector, fuel = electricity, year, outputs = value) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise_all(sum) %>%
      ungroup()

    # Long form of L123.in_EJ_R_elec_F_Yh_EUR (previously created) to be used to calculate efficiencies
    # based on electricity inputs and outputs by fuel and year.

    # Calculates electricity generation efficiencies (L123.eff_R_elec_F_Yh_EUR) by region, fuel and year
    L123.eff_R_elec_F_Yh_EUR <- L123.out_EJ_R_elec_F_Yh_preadj_EUR %>%
      semi_join(L123.in_EJ_R_elec_F_Yh_EUR, by = c("GCAM_region_ID", "fuel", "year")) %>%
      left_join(L123.in_EJ_R_elec_F_Yh_EUR, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(value = outputs / input) %>%
      select(-outputs, -input) %>%
      # Taking care of NA, 0, and INF values generatted in previous step (efficiency calculations) and updating
      # the efficiency output L123.eff_R_elec_F_Yh_EUR
      mutate(value = if_else(!is.na(value), value, energy.DEFAULT_ELECTRIC_EFFICIENCY),
             value = if_else(value == 0, energy.DEFAULT_ELECTRIC_EFFICIENCY, value),
             value = if_else(is.infinite(value), energy.DEFAULT_ELECTRIC_EFFICIENCY, value))

    # Tibble (Output_efficiency_based) created to adjust electricity outputs (L123.out_EJ_R_elec_F_Yh_EUR)
    # based on above modified efficiency calculations
    Output_efficiency_based <- L123.in_EJ_R_elec_F_Yh_EUR %>%
      left_join(L123.eff_R_elec_F_Yh_EUR, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(outputs_new = input * value) %>%
      select(-input, -value)

    # Adjusting electricity outputs based on new efficiencies
    L123.out_EJ_R_elec_F_Yh_EUR <- L123.out_EJ_R_elec_F_Yh_preadj_EUR %>%
      left_join(Output_efficiency_based, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(outputs = if_else(!is.na(outputs_new), outputs_new, outputs)) %>%
      select(GCAM_region_ID, sector, fuel, year, value = outputs)

    #2: CHP calculations --------------------
    # filter end use fuel in the industry sector and remove any NA generated.
    enduse_fuel_aggregation_industry <- enduse_fuel_aggregation %>%
      select(fuel, industry) %>%
      filter(!is.na(industry))

    # Create Industry CHP estimates by fuel, region and industry sector.
    L123.out_EJ_R_indchp_F_Yh_EUR <- L101.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "out_chp_elec",
             !fuel %in% c("heat", "electricity")) %>%
      mutate(sector = "chp_elec") %>%
      left_join(enduse_fuel_aggregation_industry, by = "fuel") %>%
      mutate(fuel = industry, ) %>%
      select(-industry) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise_all(sum) %>%
      ungroup()

    # Estimates inputs for CHP in the industry sector based on outputs and the electricity ration by fuel (output/elec_ratio)
    L123.in_EJ_R_indchp_F_Yh_EUR <- L123.out_EJ_R_indchp_F_Yh_EUR %>%
      full_join(filter(A23.chp_elecratio, fuel != "hydrogen"), by = "fuel") %>%
      mutate(value = value / elec_ratio) %>%
      select(-value, -elec_ratio)

    # Save results -------------
    L123.out_EJ_R_elec_F_Yh_EUR %>%
      add_title("Outputs of electricity sector by GCAM-Europe region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_precursors("L101.en_bal_EJ_R_Si_Fi_Yh_EUR",
                     "energy/mappings/enduse_fuel_aggregation", "energy/A23.chp_elecratio") ->
      L123.out_EJ_R_elec_F_Yh_EUR

    L123.in_EJ_R_elec_F_Yh_EUR %>%
      rename(value = input) %>%
      add_title("Inputs to electricity sector by GCAM-Europe region / fuel / historical year") %>%
      add_units("EJ") %>%
      same_precursors_as(L123.out_EJ_R_elec_F_Yh_EUR) ->
      L123.in_EJ_R_elec_F_Yh_EUR

    L123.eff_R_elec_F_Yh_EUR %>%
      add_title("Electric sector efficiencies by GCAM-Europe region / fuel / historical year") %>%
      add_units("Unitless") %>%
      same_precursors_as(L123.out_EJ_R_elec_F_Yh_EUR) ->
      L123.eff_R_elec_F_Yh_EUR

    L123.out_EJ_R_indchp_F_Yh_EUR %>%
      add_title("Industrial CHP electricity generation by GCAM-Europe region / fuel / historical year") %>%
      add_units("EJ") %>%
      same_precursors_as(L123.out_EJ_R_elec_F_Yh_EUR) ->
      L123.out_EJ_R_indchp_F_Yh_EUR

    L123.in_EJ_R_indchp_F_Yh_EUR %>%
      add_title("Inputs to industrial CHP by GCAM-Europe region / fuel / historical year") %>%
      add_units("EJ") %>%
      same_precursors_as(L123.out_EJ_R_elec_F_Yh_EUR) ->
      L123.in_EJ_R_indchp_F_Yh_EUR

    return_data(L123.out_EJ_R_elec_F_Yh_EUR, L123.in_EJ_R_elec_F_Yh_EUR, L123.eff_R_elec_F_Yh_EUR, L123.out_EJ_R_indchp_F_Yh_EUR, L123.in_EJ_R_indchp_F_Yh_EUR)
  } else {
    stop("Unknown command")
  }
}
