# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L121.liquids
#'
#' Downscale ethanol and biodiesel consumption to feedstock. Sets consumption of unconventional oil using GCAM3 assumptions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L121.in_EJ_R_unoil_F_Yh},
#'   \code{L121.in_EJ_R_TPES_crude_Yh}, \code{L121.in_EJ_R_TPES_unoil_Yh}, \code{L121.share_R_TPES_biofuel_tech}.
#' @details This chunk uses data from IIASA to downscale ethanol and biodiesel consumption to modeled technologies and feedstocks. Also adjusts
#' unconventional oil and crude oil consumption, but assumes zero unconventional oil production in all of EU (could add for Estonia in future).
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange distinct filter group_by if_else inner_join left_join mutate select summarise
#' @importFrom tidyr complete nesting
#' @author RH January 2024
module_gcameurope_L121.liquids <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/IIASA_biofuel_production",
             FILE = "aglu/IIASA_biofuel_tech_mapping",
             FILE = "aglu/IIASA_biofuel_region_mapping",
             FILE = "aglu/A_OilSeed_SecOut",
             "L101.en_bal_EJ_R_Si_Fi_Yh_EUR",
             "L121.in_EJ_R_TPES_unoil_Yh",
             "L121.BiomassOilRatios_kgGJ_R_C"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L121.in_EJ_R_TPES_crude_Yh_EUR",
             "L121.in_EJ_R_TPES_unoil_Yh_EUR",
             "L121.share_R_TPES_biofuel_tech_EUR",
             "L121.BiomassOilRatios_kgGJ_R_C_EUR"))
  } else if(command == driver.MAKE) {

    ## silence package check.
    year <- value <- iso <- FLOW <- PRODUCT <- fuel <- sector <-
      share_ctry_RG3 <- value_unoil <- GCAM_region_ID <- calibration <-
      secondary.output <- supplysector <- region_GCAM3 <- value_RG3 <-
      share <- share_RG3_world <- subsector <- technology <- minicam.energy.input <-
      value_coef <- fuel.y <- value_coef_gas <- resource <- Production_ML <-
      Biofuel <- GCAM_commodity <- SecOutRatio <- IOcoef <- Weighted_IOcoef <-
      Weighted_SecOutRatio <- Weight <- region <- gas_coef <- val_unoil <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID", strip_attributes = TRUE)
    IIASA_biofuel_production <- get_data(all_data, "aglu/IIASA_biofuel_production", strip_attributes = TRUE)
    IIASA_biofuel_tech_mapping <- get_data(all_data, "aglu/IIASA_biofuel_tech_mapping", strip_attributes = TRUE)
    IIASA_biofuel_region_mapping <- get_data(all_data, "aglu/IIASA_biofuel_region_mapping", strip_attributes = TRUE)
    A_OilSeed_SecOut <- get_data(all_data, "aglu/A_OilSeed_SecOut", strip_attributes = TRUE)
    L101.en_bal_EJ_R_Si_Fi_Yh_EUR <- get_data(all_data, "L101.en_bal_EJ_R_Si_Fi_Yh_EUR", strip_attributes = TRUE)
    L121.in_EJ_R_TPES_unoil_Yh  <- get_data(all_data, "L121.in_EJ_R_TPES_unoil_Yh", strip_attributes = TRUE)
    L121.BiomassOilRatios_kgGJ_R_C  <- get_data(all_data, "L121.BiomassOilRatios_kgGJ_R_C", strip_attributes = TRUE)


    # 1. Setting unconventional and crude oil consumption  ===================================================
    # Directly copying unconventional oil consumption calculated in module_energy_L121.liquids
    L121.in_EJ_R_TPES_unoil_Yh_EUR <- L121.in_EJ_R_TPES_unoil_Yh %>%
      semi_join(L101.en_bal_EJ_R_Si_Fi_Yh_EUR, by = "GCAM_region_ID")

    # Now subtract from refined liquids TPES and set remainder to crude oil
    L121.in_EJ_R_TPES_crude_Yh_EUR <- L101.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "TPES", fuel == "refined liquids") %>%
      left_join(L121.in_EJ_R_TPES_unoil_Yh_EUR, by = c("GCAM_region_ID", "sector", "year")) %>%
      replace_na(list(value.y = 0)) %>%
      mutate(fuel = "crude oil",
             value = value.x - value.y) %>%
      select(GCAM_region_ID, sector, fuel, year, value)

    # 2. Downscale biofuel consumption to specific technologies, per data from IIASA ----------
    # Shouldn't be any different than version from main GCAM calculation
    # can change in future if we want to use country specific data
    L121.share_R_biofuel_tech <- left_join(IIASA_biofuel_production,
                                              IIASA_biofuel_tech_mapping,
                                              by = c("Biofuel", "Crop")) %>%
      left_join(IIASA_biofuel_region_mapping, by = "Region") %>%
      filter(!is.na(technology),
             Production_ML > 0) %>%
      group_by(iso, Biofuel, technology, GCAM_commodity) %>%
      summarise(Production_ML = sum(Production_ML)) %>%
      ungroup() %>%
      group_by(iso, Biofuel) %>%
      mutate(share = Production_ML / sum(Production_ML)) %>%
      ungroup() %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      select(GCAM_region_ID, Biofuel, technology, GCAM_commodity, share)

    L121.share_R_TPES_biofuel_tech_EUR <- L101.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "TPES",
             grepl("refined biofuels", fuel),
             year == max(HISTORICAL_YEARS),
             value > 0) %>%
      # filter to regions in L121.share_R_biofuel_tech
      semi_join(L121.share_R_biofuel_tech, by = "GCAM_region_ID") %>%
      mutate(Biofuel = if_else(fuel == "refined biofuels_ethanol", "ethanol", "biodiesel")) %>%
      inner_join(L121.share_R_biofuel_tech, by = c("GCAM_region_ID", "Biofuel")) %>%
      mutate(value = value * share) %>%
      group_by(GCAM_region_ID, Biofuel) %>%
      mutate(share = value / sum(value)) %>%
      ungroup() %>%
      select(GCAM_region_ID, Biofuel, technology, GCAM_commodity, share)

    # Direct copy of L121.BiomassOilRatios_kgGJ_R_C
    L121.BiomassOilRatios_kgGJ_R_C_EUR <- L121.BiomassOilRatios_kgGJ_R_C %>%
      semi_join(L101.en_bal_EJ_R_Si_Fi_Yh_EUR, by = "GCAM_region_ID")

    # ===================================================
    # Produce outputs
    L121.in_EJ_R_TPES_crude_Yh_EUR %>%
      add_title("Crude oil total primary energy supply by GCAM region / historical year", overwrite = TRUE) %>%
      add_units("EJ") %>%
      add_comments("Unconventional oil subtracted from total primary energy supply of liquids") %>%
      add_comments("to determine crude oil supply") %>%
      add_precursors("L121.in_EJ_R_TPES_unoil_Yh", "L101.en_bal_EJ_R_Si_Fi_Yh_EUR") ->
      L121.in_EJ_R_TPES_crude_Yh_EUR

    L121.in_EJ_R_TPES_unoil_Yh_EUR %>%
      add_title("Unconventional oil total primary energy supply by GCAM region / historical year", overwrite = TRUE) %>%
      add_units("EJ") %>%
      add_comments("Direct copy of GCAM-Europe regions in L121.in_EJ_R_TPES_unoil_Yh") %>%
      add_comments("Unconventional oil production shared out to GCAM regions") %>%
      add_precursors("L121.in_EJ_R_TPES_unoil_Yh", "L101.en_bal_EJ_R_Si_Fi_Yh_EUR") ->
      L121.in_EJ_R_TPES_unoil_Yh_EUR

    L121.share_R_TPES_biofuel_tech_EUR %>%
      add_title("Share of biofuel consumption by region / technology / feedstock", overwrite = TRUE) %>%
      add_units("unitless") %>%
      add_comments("Ethanol and biodiesel consumption assigned to feedstock shares") %>%
      add_precursors("aglu/IIASA_biofuel_production", "aglu/IIASA_biofuel_region_mapping",
                     "aglu/IIASA_biofuel_tech_mapping", "L101.en_bal_EJ_R_Si_Fi_Yh_EUR", "common/iso_GCAM_regID") ->
      L121.share_R_TPES_biofuel_tech_EUR

    L121.BiomassOilRatios_kgGJ_R_C_EUR %>%
      add_title("BiomassOil input-output coefficient (kg crop / GJ oil) and secondary output ratio (kg feedcake / GJ oil) by region / feedstock", overwrite = TRUE) %>%
      add_units("kg / GJ") %>%
      add_comments("Direct copy of GCAM-Europe regions in L121.BiomassOilRatios_kgGJ_R_C") %>%
      add_comments("Calculated from weighted average OilCrop oil contents and assumptions about losses") %>%
      add_precursors( "L121.BiomassOilRatios_kgGJ_R_C", "L101.en_bal_EJ_R_Si_Fi_Yh_EUR") ->
      L121.BiomassOilRatios_kgGJ_R_C_EUR


    return_data(L121.in_EJ_R_TPES_crude_Yh_EUR,
                L121.in_EJ_R_TPES_unoil_Yh_EUR,
                L121.share_R_TPES_biofuel_tech_EUR,
                L121.BiomassOilRatios_kgGJ_R_C_EUR)
  } else {
    stop("Unknown command")
  }
}
