# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L1233.Elec_water
#'
#' Water consumption and withdrawals for electricity.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR}, \code{L1233.in_EJ_R_elec_F_tech_Yh_cool_EUR}, \code{L1233.wdraw_km3_R_elec_EUR}, \code{L1233.wcons_km3_R_elec_EUR}, \code{L1233.shrwt_R_elec_cool_EUR_Yf}. The corresponding file in the
#' original data system was \code{L1233.Elec_water.R} (water level1).
#' @details Categorizes electricity generating technologies by cooling water type, and computes water withdrawals and consumption.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter if_else group_by left_join mutate right_join select semi_join summarise
#' @importFrom tidyr complete fill nesting replace_na
#' @author RH Feb 2024
module_gcameurope_L1233.Elec_water <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "common/iso_GCAM_regID",
                     FILE = "energy/calibrated_techs",
                     FILE = "energy/mappings/enduse_fuel_aggregation",
                     "L101.en_bal_EJ_R_Si_Fi_Yh_EUR",
                     "L103.water_mapping_R_B_W_Ws_share",
                     "L1231.in_EJ_R_elec_F_tech_Yh_EUR",
                     "L1231.out_EJ_R_elec_F_tech_Yh_EUR",
                     FILE = "water/A23.CoolingSystemShares_RG3",
                     FILE = "water/elec_tech_water_map",
                     FILE = "water/Macknick_elec_water_m3MWh")
  MODULE_OUTPUTS <- c("L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR",
                      "L1233.in_EJ_R_elec_F_tech_Yh_cool_EUR",
                      "L1233.wdraw_km3_R_elec_EUR",
                      "L1233.wdraw_km3_R_B_elec_EUR",
                      "L1233.wcons_km3_R_elec_EUR",
                      "L1233.wcons_km3_R_B_elec_EUR",
                      "L1233.shrwt_R_elec_cool_Yf_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    EcYield_kgm2_hi <- EcYield_kgm2_lo <- GCAM_commodity <- GCAM_region_ID <- GLU <-
      Irr_Rfd <- LC_bm2_hi <- LC_bm2_lo <- cooling_system <- country_name <- electricity <-
      fuel <- gen <- iso <- landshare_hi <- landshare_lo <- level <- output <- output_cool_EUR <-
      plant_type <- region_GCAM3 <- sector <- share <- technology <- value <-
      water_consumption <- water_type <- water_withdrawals <- year <- yield <-
      yieldmult_hi <- yieldmult_lo <- water_sector <- GCAM_basin_ID <- NULL  # silence package check notes

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    ## STEP 1. DOWNSCALE WATER COOLING SYSTEM SHARES FROM GCAM3 REGIONS to GCAM_EUROPE COUNTRIES ----
    # RENAME INTERMEDIATE FUELS AND AGGREGATE BY UPDATED FUEL TYPE
    L101.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "out_electricity generation") %>%
      left_join(select(enduse_fuel_aggregation, fuel, electricity), by = "fuel") %>%
      # ^^ non-restrictive join required, since electricity variable has NA for certain fuels
      filter(!is.na(electricity)) %>%
      select(-fuel) %>%
      rename(fuel = electricity) -> L1233.out_EJ_R_elec_Fi_Yh_EUR
    L1233.out_EJ_R_elec_Fi_Yh_EUR %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID , year, fuel) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(sector = "electricity generation") ->  # << rename sector entries for consistency with other tables
      L1233.out_EJ_R_elec_F_Yh_EUR

    # INTERPOLATE A23.CoolingSystemShares_RG3 FOR HISTORICAL YEARS AND HISTORICAL + FUTURE YEARS
    A23.CoolingSystemShares_RG3 %>%
      gather_years %>%
      complete(nesting(region_GCAM3, plant_type, cooling_system, water_type),
               year = c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      group_by(region_GCAM3, plant_type, cooling_system, water_type) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() ->
      A23.CoolingSystemShares_RG3_LF  # << A23.CoolingSystemShares_RG3 in long form and interpolated for all yrs
    A23.CoolingSystemShares_RG3_LF %>%
      filter(year %in% HISTORICAL_YEARS) ->
      A23.CoolingSystemShares_RG3_LF_hist  # << ...and interpolated for only historical years

    # WRITE OUT REGIONAL LEVEL COOLING INFORMATION TO ALL COUNTRIES...
    # ... AND MATCH GCAM region_GCAM3 INTO TIBBLE
    elec_tech_water_map %>%
      semi_join(calibrated_techs, by = c("sector", "fuel", "technology")) %>%
      # ^^ semi_join used to remove rows in elec_tech_water_map that have no matches in calibrated_techs ^^
      select(sector, fuel, technology, cooling_system, water_type, plant_type) -> elec_tech_water_map_cal

    elec_tech_water_map_cal %>%
      repeat_add_columns(distinct(L1233.out_EJ_R_elec_F_Yh_EUR, GCAM_region_ID)) %>%
      left_join_error_no_match(distinct(iso_GCAM_regID, GCAM_region_ID, region_GCAM3) %>% # Now need to remove non-european regions, ie British/French islands
                                 filter(grepl("Europe|Soviet", region_GCAM3)), by = "GCAM_region_ID") %>%
      left_join(A23.CoolingSystemShares_RG3_LF_hist,
                by = c("cooling_system", "water_type", "plant_type", "region_GCAM3")) %>%
      # ^^ non-restrictive join required as A23 lacks data for "no cooling" plant type
      complete(nesting(sector, fuel, technology, cooling_system, water_type, plant_type, GCAM_region_ID, region_GCAM3),
               year = HISTORICAL_YEARS) %>%  # << Fill out all years for "no cooling" plant type
      filter(year %in% HISTORICAL_YEARS) %>%
      mutate(value = if_else(plant_type == "no cooling", 1, value)) %>%
      # ^^ Set cooling system share to 1 for technologies with no cooling
      rename(share = value) -> L1233.weights_R_elec_F_Yh_cool_EUR

    ## STEP 2. MULTIPLY SHARES BY COUNTRY LEVEL GENERATION TO GET ELECTRICITY -------------
    ## ... OUTPUT BY GENERATING AND COOLING TECHNOLOGIES
    L1233.weights_R_elec_F_Yh_cool_EUR %>%
      left_join(L1233.out_EJ_R_elec_F_Yh_EUR, by = c("GCAM_region_ID", "year", "fuel", "sector")) %>%
      # ^^ non-restrictive join required for additional combinations of grouping variables in initial tibble
      rename(gen = value) %>%
      mutate(output = share * gen) %>%
      select(-share, -gen) %>%
      replace_na(list(output = 0)) %>%
      select(GCAM_region_ID, sector, fuel, technology, cooling_system, water_type, year, output) -> L1233.output_R_elec_F_Yh_cool_EUR

    # (b) ...WITHOUT COOLING TECHNOLOGIES
    L1233.output_R_elec_F_Yh_cool_EUR %>%
      group_by(GCAM_region_ID, sector, fuel, technology, year) %>%
      summarise(output = sum(output)) %>%
      ungroup() -> L1233.weights_R_elec_F_Yh

    ## STEP 3: MATCH REGIONAL-LEVEL SHARES OF COOLING TECHNOLOGY WITH GENERATION TECHNOLOGY -----------------
    elec_tech_water_map_cal %>%
      repeat_add_columns(distinct(L101.en_bal_EJ_R_Si_Fi_Yh_EUR, GCAM_region_ID)) %>%
      select(GCAM_region_ID, sector, fuel, technology, cooling_system, water_type, plant_type) %>%
      left_join(L1233.output_R_elec_F_Yh_cool_EUR,
                by = c("GCAM_region_ID", "sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      rename(output_cool = output) %>%
      left_join(L1233.weights_R_elec_F_Yh,
                by = c("GCAM_region_ID", "sector", "fuel", "technology", "year")) %>%
      mutate(share = if_else(output + output_cool == 0, NA_real_, output_cool / output)) %>%
      # ^^ above used to catch possible NaN and set as NA instead so that tidyr::fill works below
      select(-output_cool, -output) %>%
      arrange(GCAM_region_ID, sector, fuel, technology, cooling_system, water_type, plant_type, year) %>%
      group_by(GCAM_region_ID, sector, fuel, technology, cooling_system, water_type, plant_type) %>%
      fill(share, .direction = "up") %>% ungroup() %>%
      replace_na(list(share = -1)) ->
      L1233.shares_R_elec_F_tech_Yh_cool_EUR

    ## STEP 4: MULTIPLY SHARES BY TECHNOLOGY-LEVEL OUTPUT TO COMPUTE NEW CALIBRATION DATASET ------------------
    L1233.shares_R_elec_F_tech_Yh_cool_EUR %>%
      left_join(L1231.out_EJ_R_elec_F_tech_Yh_EUR,
                by = c("GCAM_region_ID", "sector", "fuel", "technology", "year")) %>%
      mutate(value = share * value) %>% select(-share) -> L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR

    ## STEP 5: PARTITION FUEL INPUTS TO ELECTRICITY GENERATION ---------------
    ## ...  USING COOLING-WITHIN-GENERATION TECHNOLOGY SHARES
    ## NOTE: ASSUMES UNIFORM EFFICIENCY ACROSS ALL COOLING SYSTEM TYPES
    L1233.shares_R_elec_F_tech_Yh_cool_EUR %>%
      semi_join(L1231.in_EJ_R_elec_F_tech_Yh_EUR, by = c("fuel", "technology")) %>%
      left_join(L1231.in_EJ_R_elec_F_tech_Yh_EUR,
                by = c("GCAM_region_ID", "sector", "fuel", "technology", "year")) %>%
      mutate(value = share * value) %>% select(-share) -> L1233.in_EJ_R_elec_F_tech_Yh_cool_EUR

    ## STEP 6: MULTIPLY ELECTRICITY GENERATION BY WITHDRAWAL AND CONSUMPTION -----
    # ... COEFFICIENTS, AND AGGREGATE BY REGION
    # WITHDRAWALS
    L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR %>%
      left_join_error_no_match(Macknick_elec_water_m3MWh,
                               by = c("sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      mutate(value = value * water_withdrawals / CONV_MWH_GJ) %>%
      select(-water_withdrawals, - water_consumption) -> L1233.wdraw_km3_R_elec_EUR_F_tech_Yh_cool_EUR
    L1233.wdraw_km3_R_elec_EUR_F_tech_Yh_cool_EUR %>%
      group_by(GCAM_region_ID, sector, water_type, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(water_type != "none") -> L1233.wdraw_km3_R_elec_EUR

    # CONSUMPTION
    L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR %>%
      left_join_error_no_match(Macknick_elec_water_m3MWh,
                               by = c("sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      mutate(value = value * water_consumption / CONV_MWH_GJ) %>%
      select(-water_withdrawals, - water_consumption) -> L1233.wcons_km3_R_elec_EUR_F_tech_Yh_cool_EUR
    L1233.wcons_km3_R_elec_EUR_F_tech_Yh_cool_EUR %>%
      group_by(GCAM_region_ID, sector, water_type, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(water_type != "none") -> L1233.wcons_km3_R_elec_EUR

    ## STEP 7: COMPUTE REGIONAL-LEVEL FUTURE SHARE WEIGHTS -----------
    ## ... BASED ON GCAM 3.0, 14-REGION ASSUMPTIONS. NOTE: USES ...
    ## ... REPRESENTATIVE COUNTRIES RATHER THAN WEIGHTED AVERAGES ...
    ## ... (COUNTRY WITH MOST ELEC AS THE REPRESENTATIVE)
    L1233.out_EJ_R_elec_Fi_Yh_EUR %>%
      filter(year == dplyr::last(HISTORICAL_YEARS)) %>%
      group_by(GCAM_region_ID) %>%
      summarise(value = sum(value)) %>%
      arrange(GCAM_region_ID) %>% ungroup() -> L1233.out_EJ_R_elec_Yfby

    ## STEP 8: FILL OUT TABLE OF ALL TECHNOLOGIES TO MATCH IN SHARES ----------------
    elec_tech_water_map %>%
      select(sector, fuel, technology, cooling_system, water_type, plant_type) %>%
      na.omit() %>%
      repeat_add_columns(distinct(L101.en_bal_EJ_R_Si_Fi_Yh_EUR, GCAM_region_ID)) %>%
      left_join_error_no_match(distinct(iso_GCAM_regID, GCAM_region_ID, region_GCAM3) %>% # Now need to remove non-european regions, ie British/French islands
                                 filter(grepl("Europe|Soviet", region_GCAM3)), by = "GCAM_region_ID") %>%
      mutate(plant_type = sub("\\ \\(CCS\\)", "", plant_type)) %>%
      # ^^ sub out CCS parentheses to allow clean join with A.23, which doesn't classify tech. at this detail
      # filter regions in case there are GCAM3 regions that aren't assigned to any regions (e.g., running with a small # of regions)
      right_join(filter(A23.CoolingSystemShares_RG3_LF, grepl("Europe|Soviet", region_GCAM3)) ,
                 by = c("cooling_system", "water_type", "plant_type", "region_GCAM3")) %>%
      select(-region_GCAM3, -plant_type) ->
      L1233.shrwt_R_elec_cool_Yf_EUR

    # Addendum for basin-level calibration: write out the demands by basin
    L1233.wdraw_km3_R_B_elec_EUR <- L1233.wdraw_km3_R_elec_EUR %>%
      filter(water_type == "fresh") %>%
      select(-water_type) %>%
      left_join(filter(L103.water_mapping_R_B_W_Ws_share, water_type == "water withdrawals" & water_sector == "Electricity"),
                by = "GCAM_region_ID") %>%
      mutate(value = value * share) %>%
      select(GCAM_region_ID, GCAM_basin_ID, sector, water_type, year, value)

    L1233.wcons_km3_R_B_elec_EUR <- L1233.wcons_km3_R_elec_EUR %>%
      filter(water_type == "fresh") %>%
      select(-water_type) %>%
      left_join(filter(L103.water_mapping_R_B_W_Ws_share, water_type == "water consumption" & water_sector == "Electricity"),
                by = "GCAM_region_ID") %>%
      mutate(value = value * share) %>%
      select(GCAM_region_ID, GCAM_basin_ID, sector, water_type, year, value)

    ## OUTPUTS ------------------
    L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR %>%
      add_title("Electricity output by region, fuel, technology, cooling system, and water type") %>%
      add_units("EJ") %>%
      add_comments("Computed by multiplying shares by technology-level output") %>%
      add_legacy_name("L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR") %>%
      add_precursors("common/iso_GCAM_regID",
                     "energy/calibrated_techs",
                     "energy/mappings/enduse_fuel_aggregation",
                     "L101.en_bal_EJ_R_Si_Fi_Yh_EUR",
                     "L1231.in_EJ_R_elec_F_tech_Yh_EUR",
                     "L1231.out_EJ_R_elec_F_tech_Yh_EUR",
                     "water/A23.CoolingSystemShares_RG3",
                     "water/elec_tech_water_map") ->
      L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR

    L1233.in_EJ_R_elec_F_tech_Yh_cool_EUR %>%
      add_title("Fuel inputs to electricity generation by region, fuel, technology, cooling system, and water type") %>%
      add_units("EJ") %>%
      add_comments("Computed by multiplying shares by technology-level input") %>%
      add_legacy_name("L1233.in_EJ_R_elec_F_tech_Yh_cool_EUR") %>%
      add_precursors("common/iso_GCAM_regID",
                     "energy/calibrated_techs",
                     "energy/mappings/enduse_fuel_aggregation",
                     "L101.en_bal_EJ_R_Si_Fi_Yh_EUR",
                     "L1231.in_EJ_R_elec_F_tech_Yh_EUR",
                     "L1231.out_EJ_R_elec_F_tech_Yh_EUR",
                     "water/A23.CoolingSystemShares_RG3",
                     "water/elec_tech_water_map") ->
      L1233.in_EJ_R_elec_F_tech_Yh_cool_EUR

    L1233.wdraw_km3_R_elec_EUR %>%
      add_title("Water withdrawals for electricity generation by region and water type") %>%
      add_units("km^3") %>%
      add_comments("Computed by multiplying generation by water withdrawal coefficients") %>%
      add_legacy_name("L1233.wdraw_km3_R_elec_EUR") %>%
      add_precursors("common/iso_GCAM_regID",
                     "energy/calibrated_techs",
                     "energy/mappings/enduse_fuel_aggregation",
                     "L101.en_bal_EJ_R_Si_Fi_Yh_EUR",
                     "L1231.out_EJ_R_elec_F_tech_Yh_EUR",
                     "water/A23.CoolingSystemShares_RG3",
                     "water/elec_tech_water_map",
                     "water/Macknick_elec_water_m3MWh") ->
      L1233.wdraw_km3_R_elec_EUR

    L1233.wcons_km3_R_elec_EUR %>%
      add_title("Water consumption for electricity generation by region and water type") %>%
      add_units("km^3") %>%
      add_comments("Computed by multiplying generation by water consumption coefficients") %>%
      add_legacy_name("L1233.wcons_km3_R_elec_EUR") %>%
      add_precursors("common/iso_GCAM_regID",
                     "energy/calibrated_techs",
                     "energy/mappings/enduse_fuel_aggregation",
                     "L101.en_bal_EJ_R_Si_Fi_Yh_EUR",
                     "L1231.out_EJ_R_elec_F_tech_Yh_EUR",
                     "water/A23.CoolingSystemShares_RG3",
                     "water/elec_tech_water_map",
                     "water/Macknick_elec_water_m3MWh") ->
      L1233.wcons_km3_R_elec_EUR

    L1233.shrwt_R_elec_cool_Yf_EUR %>%
      add_title("Future cooling system shareweights by region, electric sector, and technology ") %>%
      add_units("Unitless") %>%
      add_comments("Filled out using cooling shares table") %>%
      add_precursors("common/iso_GCAM_regID",
                     "water/A23.CoolingSystemShares_RG3",
                     "water/elec_tech_water_map") ->
      L1233.shrwt_R_elec_cool_Yf_EUR

    L1233.wdraw_km3_R_B_elec_EUR %>%
      add_title("Water withdrawals for electricity generation by region, basin, and water type") %>%
      add_units("km^3") %>%
      add_comments("Computed by multiplying regional withdrawal volumes by basin-level shares") %>%
      same_precursors_as(L1233.wdraw_km3_R_elec_EUR) %>%
      add_precursors("L103.water_mapping_R_B_W_Ws_share") ->
      L1233.wdraw_km3_R_B_elec_EUR

    L1233.wcons_km3_R_B_elec_EUR %>%
      add_title("Water consumption for electricity generation by region, basin, and water type") %>%
      add_units("km^3") %>%
      add_comments("Computed by multiplying regional consumption volumes by basin-level shares") %>%
      same_precursors_as(L1233.wdraw_km3_R_elec_EUR) %>%
      add_precursors("L103.water_mapping_R_B_W_Ws_share") ->
      L1233.wcons_km3_R_B_elec_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
