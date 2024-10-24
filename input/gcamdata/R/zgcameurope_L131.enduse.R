# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L131.enduse
#'
#' Generate the following two tables:
#' \itemize{
#'  \item{Final scaled energy input by GCAM region / end-use sector, incl CHP / fuel / historical year; and}
#'  \item{Share of heat consumption by end-use sector within GCAM region / historical year}
#' }
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L131.in_EJ_R_Senduse_F_Yh_EUR}, \code{L131.share_R_Senduse_heat_Yh_EUR}. The corresponding file in the
#' original data system was \code{LA131.enduse.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by mutate select summarise
#' @importFrom tidyr replace_na
#' @author RH February 2024
module_gcameurope_L131.enduse <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A_regions",
             FILE = "gcam-europe/mappings/enduse_sector_aggregation",
             FILE = "water/EFW_mapping",
             "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
             "L122.in_EJ_R_refining_F_Yh_EUR",
             "L124.in_EJ_R_heat_F_Yh_EUR",
             "L124.out_EJ_R_heat_F_Yh_EUR",
             "L124.out_EJ_R_heatfromelec_F_Yh_EUR",
             "L126.out_EJ_R_electd_F_Yh_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L131.in_EJ_R_Senduse_F_Yh_EUR",
             "L131.share_R_Senduse_heat_Yh_EUR"))
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- sector <- fuel <- value.y <- value.x <-
      has_district_heat <- . <- NULL  # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    A_regions <- get_data(all_data, "energy/A_regions")
    enduse_sector_aggregation <- get_data(all_data, "gcam-europe/mappings/enduse_sector_aggregation")
    EFW_mapping <- get_data(all_data, "water/EFW_mapping")
    L1012.en_bal_EJ_R_Si_Fi_Yh_EUR <- get_data(all_data, "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR", strip_attributes = TRUE)
    L122.in_EJ_R_refining_F_Yh_EUR <- get_data(all_data, "L122.in_EJ_R_refining_F_Yh_EUR")
    L124.out_EJ_R_heat_F_Yh_EUR <- get_data(all_data, "L124.out_EJ_R_heat_F_Yh_EUR")
    L124.out_EJ_R_heatfromelec_F_Yh_EUR <- get_data(all_data, "L124.out_EJ_R_heatfromelec_F_Yh_EUR")
    L126.out_EJ_R_electd_F_Yh_EUR <- get_data(all_data, "L126.out_EJ_R_electd_F_Yh_EUR")
    L124.in_EJ_R_heat_F_Yh_EUR  <- get_data(all_data, "L124.in_EJ_R_heat_F_Yh_EUR")

    # 1. ELECTRICITY SCALING  ===================================================
    # First, subset and aggregate the "upstream" electricity demands by the energy system that are not being scaled
    L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector %in% EFW_mapping$agg_sector,
             fuel == "electricity") ->
      L121.in_EJ_R_EFW_elec_Yh_EUR

    L122.in_EJ_R_refining_F_Yh_EUR %>%
      filter(fuel == "electricity", year %in% HISTORICAL_YEARS) %>%
      bind_rows(L121.in_EJ_R_EFW_elec_Yh_EUR) %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup ->
      Refin_EFW_elect

    # Subtract this from total delivered electricity (output of t&d sector). This is the amount that is available for scaling to end uses.
    Refin_EFW_elect %>%
      left_join_error_no_match(L126.out_EJ_R_electd_F_Yh_EUR, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(value = value.y - value.x) %>%
      select(-sector, -value.x, -value.y) ->
      Enduse_elect

    # Subset the end use sectors and aggregate by fuel
    L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector %in% enduse_sector_aggregation$sector, year %in% HISTORICAL_YEARS) %>%
      filter(fuel == "electricity") %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) ->
      Enduse_elect_unscaled

    # Calculate the scalers required to balance electricity within each region
    Enduse_elect %>%
      left_join_error_no_match(Enduse_elect_unscaled, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(value = if_else(value.y == 0, 0,  value.x / value.y)) %>%
      ungroup() %>%
      select(GCAM_region_ID, year, value) ->
      Enduse_elect_scaler

    # Multiply the electricity scalers by the original estimates of electricity consumption by end use sectors
    L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector %in% enduse_sector_aggregation$sector) %>%
      filter(fuel == "electricity") %>%
      left_join_error_no_match(Enduse_elect_scaler, by = c("GCAM_region_ID", "year")) %>%
      mutate(value = value.x * value.y) %>%
      select(-value.x, -value.y) ->
      Enduse_elect_scaled

    # Replace unscaled estimates of end use sector electricity consumption in full table
    L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector %in% enduse_sector_aggregation$sector) %>%
      filter(fuel != "electricity") %>%
      bind_rows(Enduse_elect_scaled) ->
      Enduse_elect_scaled_heat_unscaled # still need to scale heat

    # 2. HEAT SCALING ===================================================
    # Total delivered heat = output of district heat sector + secondary (heat) output of electric sector
    L124.out_EJ_R_heatfromelec_F_Yh_EUR %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) ->
      Heatfromelect

    L124.out_EJ_R_heat_F_Yh_EUR %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) %>%
      left_join_error_no_match(Heatfromelect, by = c("GCAM_region_ID", "year")) %>%
      mutate(value = value.x + value.y) %>%
      select(-value.x, -value.y) ->
      Enduse_heat

    # Remove heat consumption from regions with no production
    Enduse_elect_scaled_heat_unscaled <- Enduse_elect_scaled_heat_unscaled %>%
      filter(fuel == "heat") %>%
      left_join(Enduse_heat %>% ungroup,
                by = c("GCAM_region_ID", "year")) %>%
      tidyr::replace_na(list(value.y = 0)) %>%
      mutate(value = if_else(value.y == 0, 0, value.x)) %>%
      select(-value.x, -value.y) %>%
      bind_rows(Enduse_elect_scaled_heat_unscaled %>% filter(fuel != "heat"))

    # Subset the end use sectors and aggregate by fuel. Only in regions where heat is modeled as a separate fuel.
    A_regions %>%
      filter(has_district_heat == 1) %>% # Filtering for regions where heat is modeled as a separate fuel
      select(GCAM_region_ID) %>%
      .$GCAM_region_ID ->
      GCAM_region_ID_heat

    Enduse_elect_scaled_heat_unscaled %>%
      filter(fuel == "heat", GCAM_region_ID %in% GCAM_region_ID_heat) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) ->
      Enduse_heat_unscaled

    # Calculate the scalers required to balance district heat production and consumption within each region
    Enduse_heat %>%
      left_join_error_no_match(Enduse_heat_unscaled, by = c("GCAM_region_ID", "year")) %>%
      # If value.y is zero, there is no consumption of heat, so scaler should be zero
      mutate(value = if_else(value.y == 0, 0, value.x / value.y)) %>%
      select(-value.x, -value.y) ->
      Enduse_heat_scaler

    # Multiply the district heat scalers by the original estimates of district heat consumption by end use sectors
    Enduse_elect_scaled_heat_unscaled %>%
      filter(fuel == "heat", GCAM_region_ID %in% GCAM_region_ID_heat) %>%
      left_join_error_no_match(Enduse_heat_scaler, by = c("GCAM_region_ID", "year")) %>%
      mutate(value = value.x * value.y) %>%
      select(-value.x, -value.y) ->
      Enduse_heat_scaled

    # Replace unscaled estimates of end use sector heat consumption in full table
    Enduse_elect_scaled_heat_unscaled %>%
      mutate(fuel = replace(fuel, GCAM_region_ID %in% GCAM_region_ID_heat & fuel == "heat", "heat_scaled")) %>%
      filter(fuel != "heat_scaled") %>%
      bind_rows(Enduse_heat_scaled) %>%
      arrange(GCAM_region_ID, sector, fuel, year) ->
      L131.in_EJ_R_Senduse_F_Yh_EUR_tmp # Output table 1

    # There are some regions that have heat production that is not included in the energy balance
    # this was done because for regions with heat from elec but no direct production of district heat
    # Subtract the fuel consumption used for this derived heat production here
    ExtraHeatProduction <- L124.in_EJ_R_heat_F_Yh_EUR %>%
      left_join_error_no_match(L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
                                 filter(sector == "in_heat") %>%
                                 mutate(sector = "heat"), by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(deduction = value.x - value.y,
             sector = "in_industry_general") %>%
      select(GCAM_region_ID, sector, fuel, year, deduction)

    L131.in_EJ_R_Senduse_F_Yh_EUR <- L131.in_EJ_R_Senduse_F_Yh_EUR_tmp %>%
      left_join(ExtraHeatProduction, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      tidyr::replace_na(list(deduction = 0)) %>%
      mutate(value = value - deduction) %>%
      select(-deduction)


    # Heat in some regions is not modeled separately from the fuels used to produce it
    A_regions %>%
      filter(has_district_heat == 0) %>% # Filtering for regions where heat is not modeled as a separate fuel
      select(GCAM_region_ID) %>%
      .$GCAM_region_ID ->
      GCAM_region_ID_no_heat

    # In these regions, calculate the share of regional heat demand by each sector
    L131.in_EJ_R_Senduse_F_Yh_EUR %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) ->
      Enduse_total

    L131.in_EJ_R_Senduse_F_Yh_EUR %>%
      filter(fuel == "heat") %>%
      filter(GCAM_region_ID %in% GCAM_region_ID_no_heat) %>%
      left_join_error_no_match(Enduse_total, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(value = value.x / value.y) %>%
      select(-value.x, -value.y) ->
      Enduse_heat_scaled_share

    # Regions may have zero heat consumption by demand sectors while nevertheless having heat production. Assign this to industry
    Enduse_heat_scaled_share %>%
      filter(sector == "in_industry_general") %>%
      replace_na(list(value = 1)) ->
      Enduse_heat_scaled_share_indust

    Enduse_heat_scaled_share %>%
      filter(sector != "in_industry_general") %>%
      replace_na(list(value = 0)) %>%
      bind_rows(Enduse_heat_scaled_share_indust) %>%
      arrange(GCAM_region_ID, sector, year) ->
      L131.share_R_Senduse_heat_Yh_EUR # Output table 2

    # Outputs ===================================================
    L131.in_EJ_R_Senduse_F_Yh_EUR %>%
      add_title("Final scaled energy input by GCAM Europe region / end-use sector (incl CHP) / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Scalers were used to balance electricity and district heat production and consumption within each region") %>%
      add_legacy_name("L131.in_EJ_R_Senduse_F_Yh_EUR") %>%
      add_precursors("gcam-europe/mappings/enduse_sector_aggregation", "water/EFW_mapping", "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
                     "L121.in_EJ_R_unoil_F_Yh", "L122.in_EJ_R_refining_F_Yh_EUR", "L126.out_EJ_R_electd_F_Yh_EUR") ->
      L131.in_EJ_R_Senduse_F_Yh_EUR

    L131.share_R_Senduse_heat_Yh_EUR %>%
      add_title("Share of heat consumption by end-use sector within GCAM Europe region / historical year") %>%
      add_units("Unitless") %>%
      add_comments("Share of regional heat demand by each sector was calculated for regions where heat is not modeled separately from the fuels used to produce it. Moreoever, regions having zero heat consumption by demand sectors while nevertheless also having heat production, this was assigned to industry") %>%
      add_legacy_name("L131.share_R_Senduse_heat_Yh_EUR") %>%
      add_precursors("energy/A_regions", "gcam-europe/mappings/enduse_sector_aggregation",
                     "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
                     "L121.in_EJ_R_unoil_F_Yh",
                     "L122.in_EJ_R_refining_F_Yh_EUR",
                     "L124.out_EJ_R_heat_F_Yh_EUR", "L124.out_EJ_R_heatfromelec_F_Yh_EUR",
                     "L126.out_EJ_R_electd_F_Yh_EUR") ->
      L131.share_R_Senduse_heat_Yh_EUR

    return_data(L131.in_EJ_R_Senduse_F_Yh_EUR, L131.share_R_Senduse_heat_Yh_EUR)
  } else {
    stop("Unknown command")
  }
}
