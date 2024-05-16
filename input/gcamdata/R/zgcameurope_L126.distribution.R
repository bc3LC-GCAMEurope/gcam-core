# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L126.distribution
#'
#' Adjust for onsite, transmission, and distribution losses for electricity and gas. Energy input, output, and input/output ratio are calculated for the following three processes:
#' \itemize{
#'  \item{Electricity net ownuse, i.e., electricity consumed onsite prior to any transmission and distribution losses}
#'  \item{Electricity transmission and distribution}
#'  \item{Gas pipeline}
#' }
#' Nine tables are created in total, 3x3=9. Energy is reported by GCAM_region, sector, fuel, and from 1971-2010.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L126.in_EJ_R_elecownuse_F_Yh_EUR}, \code{L126.out_EJ_R_elecownuse_F_Yh_EUR}, \code{L126.IO_R_elecownuse_F_Yh_EUR}, \code{L126.in_EJ_R_electd_F_Yh_EUR}, \code{L126.out_EJ_R_electd_F_Yh_EUR}, \code{L126.IO_R_electd_F_Yh_EUR}, \code{L126.in_EJ_R_gaspipe_F_Yh_EUR}, \code{L126.out_EJ_R_gaspipe_F_Yh_EUR}, \code{L126.IO_R_gaspipe_F_Yh_EUR}. The corresponding file in the
#' original data system was \code{LA126.distribution.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by left_join mutate select summarise
#' @importFrom tidyr replace_na
#' @author RH February 2024
module_gcameurope_L126.distribution <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
             "L122.out_EJ_R_gasproc_F_Yh_EUR",
             "L123.out_EJ_R_elec_F_Yh_EUR",
             "L123.out_EJ_R_indchp_F_Yh_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L126.in_EJ_R_elecownuse_F_Yh_EUR",
             "L126.out_EJ_R_elecownuse_F_Yh_EUR",
             "L126.IO_R_elecownuse_F_Yh_EUR",
             "L126.in_EJ_R_electd_F_Yh_EUR",
             "L126.out_EJ_R_electd_F_Yh_EUR",
             "L126.IO_R_electd_F_Yh_EUR",
             "L126.in_EJ_R_gaspipe_F_Yh_EUR",
             "L126.out_EJ_R_gaspipe_F_Yh_EUR",
             "L126.IO_R_gaspipe_F_Yh_EUR"))
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- sector <- fuel <-
      value_electricity_generation <- value_industryCHP <-
      value_electricity_ownuse_in <- value_electricity_ownuse <-
      value_electricity_ownuse_out <- value_electricity_ownuse_IO <-
      value_electd <- value_electd_out <- value_electd_IO <-
      value_gaspipe_in <- value_gaspipe <- value_gaspipe_out <-
      value_gaspipe_IO <- NULL       # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    L1012.en_bal_EJ_R_Si_Fi_Yh_EUR <- get_data(all_data, "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR")
    L122.out_EJ_R_gasproc_F_Yh_EUR <- get_data(all_data, "L122.out_EJ_R_gasproc_F_Yh_EUR")
    L123.out_EJ_R_elec_F_Yh_EUR <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh_EUR")
    L123.out_EJ_R_indchp_F_Yh_EUR <- get_data(all_data, "L123.out_EJ_R_indchp_F_Yh_EUR")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    # 1. ELECTRICITY OWNUSE ===================================================
    # i.e., electricity consumed onsite prior to any transmission and distribution losses
    # Summing industrial CHP electricity generation and electricity generation by GCAM region ID and year
    # Summing electricity generation by GCAM region ID and year
    L123.out_EJ_R_elec_F_Yh_EUR %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value_electricity_generation = sum(value)) %>%
      ungroup() ->
      Electricity_generation

    # Summing industrial CHP electricity generation and then joining it with electricity generation
    L123.out_EJ_R_indchp_F_Yh_EUR %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value_industryCHP = sum(value)) %>%
      ungroup() %>%
      left_join_error_no_match(Electricity_generation, by = c("GCAM_region_ID", "year")) %>%
      mutate(value_electricity_ownuse_in = value_electricity_generation + value_industryCHP) %>% # Summing electricity generation and industrial CHP electricity generation. This creates values for table, L126.in_EJ_R_elecownuse_F_Yh_EUR (i.e., input).
      select(-value_electricity_generation, -value_industryCHP) ->
      Electricity_total

    # Filtering energy balance by to electricity ownuse sectors
    # Pumped hydro has input and output, so we need to calculate the difference
    L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(grepl("electricity ownuse", sector),
             fuel == "electricity") %>%
      mutate(value = if_else(sector == "out_electricity ownuse", -1 * value, value),
             sector = "electricity ownuse") %>% # Renaming sector as "electricity ownuse"
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value_electricity_ownuse = sum(value)) %>%
      ungroup() %>%
      left_join_error_no_match(Electricity_total, by = c("GCAM_region_ID", "year")) %>% # Joining previous table (electricity sector generation, which includes industrial CHP electricity generation)
      mutate(value_electricity_ownuse_out = value_electricity_ownuse_in - value_electricity_ownuse, # Creating values for table, L126.out_EJ_R_elecownuse_F_Yh_EUR (i.e., output), by subtracting electricity generation by elecricity ownuse
             value_electricity_ownuse_IO = if_else(value_electricity_ownuse_out == 0, 0, value_electricity_ownuse_in / value_electricity_ownuse_out)) -> # Creating values for table, L126.IO_R_elecownuse_F_Yh_EUR,  by dividing input by output
      Electricity_ownuse_all

    # Table Electricity_ownuse_all is separated to create the final tables
    Electricity_ownuse_all %>%
      select(GCAM_region_ID, sector, fuel, year, value = value_electricity_ownuse_in) ->
      L126.in_EJ_R_elecownuse_F_Yh_EUR
    Electricity_ownuse_all %>%
      select(GCAM_region_ID, sector, fuel, year, value = value_electricity_ownuse_out) ->
      L126.out_EJ_R_elecownuse_F_Yh_EUR
    Electricity_ownuse_all %>%
      select(GCAM_region_ID, sector, fuel, year, value = value_electricity_ownuse_IO) ->
      L126.IO_R_elecownuse_F_Yh_EUR

    # 2. ELECTRICITY TRANSMISSION AND DISTRIBUTION =========================================
    # Preparing electricity generation output (i.e., L126.out_EJ_R_elecownuse_F_Yh_EUR) to be joined later with energy balance
    L126.out_EJ_R_elecownuse_F_Yh_EUR %>%
      ungroup() %>% # Need to ungroup to deselect sector and fuel
      select(-sector, -fuel) ->
      Electricity_ownuse_out

    # Filtering energy balance by "net_electricity distribution" sector and joining with electricity generation output
    L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "net_electricity distribution") %>%
      mutate(sector = replace(sector, sector == "net_electricity distribution", "electricity distribution")) %>% # Renaming sector as "electricity distribution"
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value_electd = sum(value)) %>%
      ungroup() %>%
      left_join_error_no_match(Electricity_ownuse_out, by = c("GCAM_region_ID", "year")) %>% # Joining electricity generation output (L126.out_EJ_R_elecownuse_F_Yh_EUR, wherein electricity ownuse was subtracted from electricity generation)
      mutate(value_electd_out = value - value_electd, # Creating values for table, L126.out_EJ_R_electd_F_Yh_EUR (i.e. ouput), by subtracting electricity generation (without ownuse) by transmission and distribution consumption
             value_electd_IO = value / value_electd_out) -> # Creating values for table, L126.IO_R_electd_F_Yh_EUR, by dividing input by output
      Electricity_distribution_all

    # Need to adjust Moldova because the majority of electricity is imported
    # so production - losses results in missing electricity for demand
    # Set maximum IO to 1.5
    MOLDOVA_ID <- GCAM_region_names %>% filter(region == "Moldova") %>%  pull(GCAM_region_ID)
    Electricity_distribution_all <- Electricity_distribution_all %>%
      mutate(value_electd_IO = if_else(value_electd_IO > 1.5 & GCAM_region_ID == MOLDOVA_ID, 1.5, value_electd_IO),
             value_electd_out = value / value_electd_IO)


    # Table Electricity_distribution_all is separated to create the final tables
    Electricity_distribution_all %>%
      select(GCAM_region_ID, sector, fuel, year, value) ->
      L126.in_EJ_R_electd_F_Yh_EUR
    Electricity_distribution_all %>%
      select(GCAM_region_ID, sector, fuel, year, value = value_electd_out) ->
      L126.out_EJ_R_electd_F_Yh_EUR
    Electricity_distribution_all %>%
      select(GCAM_region_ID, sector, fuel, year, value = value_electd_IO) %>%
      # Correct Nan in Srb&Mne in 1975 by extrapolating the next available coef (1990)
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = if_else(!is.nan(value), value, approx_fun(year, value, 2))) %>%
      ungroup()  -> L126.IO_R_electd_F_Yh_EUR

    # 3. GAS PIPELINE  =========================================
    # Preparing to be joined later - summing by GCAM region ID and year
    L122.out_EJ_R_gasproc_F_Yh_EUR %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value_gaspipe_in = sum(value)) %>% # Creating values for table, L126.in_EJ_R_gaspipe_F_Yh_EUR (i.e., input)
      ungroup() ->
      Gasproc_out

    # Filtering energy balance by "net_gas pipeline" sector and joining with gas output
    L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "net_gas pipeline") %>%
      mutate(sector = replace(sector, sector == "net_gas pipeline", "gas pipeline")) %>% # Renaming sector as "gas pipeline"
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value_gaspipe = sum(value)) %>%
      ungroup() %>%
      left_join(Gasproc_out, by = c("GCAM_region_ID", "year")) %>% # Joining gas output
      mutate(value_gaspipe_out = value_gaspipe_in - value_gaspipe, # Creating values for table, L126.out_EJ_R_gaspipe_F_Yh_EUR (i.e., output), by subtracting gas pipeline input by gas pipeline consumption
             value_gaspipe_IO = value_gaspipe_in / value_gaspipe_out) %>% # Creating values for table, L126.IO_R_gaspipe_F_Yh_EUR, by dividing input by output. Note that some regions have zero gas in some of the base years. Reset their IO coefs to 1 in the next step.
      replace_na(list(value_gaspipe_IO = 1)) -> # Reset NaN IO coefs to 1, since some regions have no gas in some base years.
      Gas_pipeline_all

    # Table Gas_pipeline_all is separated to create the final tables
    Gas_pipeline_all %>%
      select(GCAM_region_ID, sector, fuel, year, value = value_gaspipe_in) ->
      L126.in_EJ_R_gaspipe_F_Yh_EUR
    Gas_pipeline_all %>%
      select(GCAM_region_ID, sector, fuel, year, value = value_gaspipe_out) ->
      L126.out_EJ_R_gaspipe_F_Yh_EUR
    Gas_pipeline_all %>%
      select(GCAM_region_ID, sector, fuel, year, value = value_gaspipe_IO) ->
      L126.IO_R_gaspipe_F_Yh_EUR

    # OUTPUTS ===================================================
    L126.in_EJ_R_elecownuse_F_Yh_EUR %>%
      add_title("Electricity onsite energy input") %>%
      add_units("EJ") %>%
      add_comments("Sum of electricty generation, including industrial CHP secondary input") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh_EUR", "L123.out_EJ_R_elec_F_Yh_EUR", "L123.out_EJ_R_indchp_F_Yh_EUR") ->
      L126.in_EJ_R_elecownuse_F_Yh_EUR

    L126.out_EJ_R_elecownuse_F_Yh_EUR %>%
      add_title("Electricity onsite energy output") %>%
      add_units("EJ") %>%
      add_comments("Sum of electricty generation adjusted for onsite losses") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh_EUR", "L123.out_EJ_R_elec_F_Yh_EUR", "L123.out_EJ_R_indchp_F_Yh_EUR") ->
      L126.out_EJ_R_elecownuse_F_Yh_EUR

    L126.IO_R_elecownuse_F_Yh_EUR %>%
      add_title("Electricity onsite energy input/output ratio") %>%
      add_units("EJ") %>%
      add_comments("Energy input divided by ouput") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh_EUR", "L123.out_EJ_R_elec_F_Yh_EUR", "L123.out_EJ_R_indchp_F_Yh_EUR") ->
      L126.IO_R_elecownuse_F_Yh_EUR

    L126.in_EJ_R_electd_F_Yh_EUR %>%
      add_title("Electricity transmission and distribution energy input") %>%
      add_units("EJ") %>%
      add_comments("Sum of electricty generation adjusted for onsite losses") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh_EUR", "L123.out_EJ_R_elec_F_Yh_EUR", "L123.out_EJ_R_indchp_F_Yh_EUR") ->
      L126.in_EJ_R_electd_F_Yh_EUR

    L126.out_EJ_R_electd_F_Yh_EUR %>%
      add_title("Electricity transmission and distribution energy output") %>%
      add_units("EJ") %>%
      add_comments("Sum of electricty generation adjusted for onsite, transmission, and distribution losses") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh_EUR", "L123.out_EJ_R_elec_F_Yh_EUR", "L123.out_EJ_R_indchp_F_Yh_EUR") ->
      L126.out_EJ_R_electd_F_Yh_EUR

    L126.IO_R_electd_F_Yh_EUR %>%
      add_title("Electricity transmission and distribution energy input/output ratio") %>%
      add_units("EJ") %>%
      add_comments("Energy input divided by ouput") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh_EUR", "L123.out_EJ_R_elec_F_Yh_EUR", "L123.out_EJ_R_indchp_F_Yh_EUR") ->
      L126.IO_R_electd_F_Yh_EUR

    L126.in_EJ_R_gaspipe_F_Yh_EUR %>%
      add_title("Gas pipeline energy input") %>%
      add_units("EJ") %>%
      add_comments("Energy input to gas pipeline") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh_EUR", "L122.out_EJ_R_gasproc_F_Yh_EUR") ->
      L126.in_EJ_R_gaspipe_F_Yh_EUR

    L126.out_EJ_R_gaspipe_F_Yh_EUR %>%
      add_title("Gas pipeline energy output") %>%
      add_units("EJ") %>%
      add_comments("Energy output of gas pipeline, accounting for pipeline losses") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh_EUR", "L122.out_EJ_R_gasproc_F_Yh_EUR") ->
      L126.out_EJ_R_gaspipe_F_Yh_EUR

    L126.IO_R_gaspipe_F_Yh_EUR %>%
      add_title("Gas pipeline energy input/output ratio") %>%
      add_units("EJ") %>%
      add_comments("Energy input divided by ouput") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh_EUR", "L122.out_EJ_R_gasproc_F_Yh_EUR") ->
      L126.IO_R_gaspipe_F_Yh_EUR

    return_data(L126.in_EJ_R_elecownuse_F_Yh_EUR, L126.out_EJ_R_elecownuse_F_Yh_EUR, L126.IO_R_elecownuse_F_Yh_EUR,
                L126.in_EJ_R_electd_F_Yh_EUR, L126.out_EJ_R_electd_F_Yh_EUR, L126.IO_R_electd_F_Yh_EUR,
                L126.in_EJ_R_gaspipe_F_Yh_EUR, L126.out_EJ_R_gaspipe_F_Yh_EUR, L126.IO_R_gaspipe_F_Yh_EUR)
  } else {
    stop("Unknown command")
  }
}
