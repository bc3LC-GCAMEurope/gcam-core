# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L111.rsrc_fos_Prod
#'
#' Calculate historical fossil energy production and fossil resource supply curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L111.Prod_EJ_R_F_Yh_EUR}.
#' @details For historical fossil energy production, determine regional shares of production for each primary fuel,
#' interpolate unconventional oil production to all historical years, deduct unconventional oil from total oil,
#' and include it in calibrated production table.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr complete replace_na
#' @author RH January 2024
module_gcameurope_L111.rsrc_fos_Prod <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "common/GCAM32_to_EU",
                     FILE = "energy/mappings/IEA_product_rsrc",
                     FILE = "energy/rsrc_unconv_oil_prod_bbld",
                     FILE = "energy/A11.fos_curves",
                     "L100.IEA_en_bal_ctry_hist",
                     "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
                     "L100.IEA_en_bal_ctry_hist",
                     "L1012.en_bal_EJ_R_Si_Fi_Yh",
                     "L111.Prod_EJ_R_F_Yh")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L111.Prod_EJ_R_F_Yh_EUR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    sector <- fuel <- year <- value <- share <- iso <- GCAM_region_ID <- unconventionals <- value.x <-
      value.y <- FLOW <- PRODUCT <- resource <- region_GCAM3 <- CumulSum <- subresource <- grade <-
      available <- available_region_GCAM3 <- extractioncost <- technology <-  . <- NULL  # silence package check notes

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # 1a. Fossil fuel production to be distributed to europe regions -------
    # Keep non-Europe production the same and distribute europe production based on remainder
    # NOTE: Regional production is derived for each fuel as TPES times regional share of production

    # calculate production that must remain the same
    L111.Prod_EJ_R_F_Yh_FIXED <- L111.Prod_EJ_R_F_Yh %>%
      anti_join(L1012.en_bal_EJ_R_Si_Fi_Yh_EUR, by = "GCAM_region_ID") %>%
      group_by(sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      mutate(fuel = case_when(
        fuel == "coal" ~ "coal",
        fuel == "crude oil" ~ "refined liquids",
        fuel == "natural gas" ~ "gas"))

    # remaining consumption that needs to be distributed
    L111.TPES_EJ_F_Yh_REMAINDER <- L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      anti_join(L1012.en_bal_EJ_R_Si_Fi_Yh_EUR, by = c("GCAM_region_ID")) %>%
      bind_rows(L1012.en_bal_EJ_R_Si_Fi_Yh_EUR) %>%
      filter(sector == "TPES", fuel %in% energy.RSRC_FUELS, year %in% HISTORICAL_YEARS) %>%
      group_by(sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      left_join_error_no_match(L111.Prod_EJ_R_F_Yh_FIXED, by = c("fuel", "year")) %>%
      mutate(value = value.x - value.y) %>%
      select(year, fuel, value)

    # 1b. Regional shares of remaining production-------
    # Determine regional shares of production for each primary fuel
    L111.Prod_EJ_R_F_Yh_EUR_unadj <- L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "out_resources", fuel %in% energy.RSRC_FUELS, year %in% HISTORICAL_YEARS)

    L111.Prod_share_R_F_Yh_EUR <- L111.Prod_EJ_R_F_Yh_EUR_unadj %>%
      group_by(sector, fuel, year) %>%
      mutate(share = value / sum(value)) %>%
      ungroup %>%
      select(-value)

    # 1c. Production by fuel -----------------------
    # Multiply through to calculate production by fuel
    L111.Prod_EJ_R_F_Yh_EUR <- L111.Prod_EJ_R_F_Yh_EUR_unadj %>%
      select(-value) %>%
      left_join_error_no_match(L111.TPES_EJ_F_Yh_REMAINDER, by = c("fuel", "year")) %>%
      left_join_error_no_match(L111.Prod_share_R_F_Yh_EUR, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(value = value * share) %>%
      select(-share) %>%
      mutate(fuel = case_when(
        fuel == "coal" ~ "coal",
        fuel == "refined liquids" ~ "crude oil",
        fuel == "gas" ~ "natural gas"),
        technology = fuel)

    # Produce outputs
    L111.Prod_EJ_R_F_Yh_EUR %>%
      add_title("Historical fossil energy production") %>%
      add_units("EJ") %>%
      add_precursors( "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR") ->
      L111.Prod_EJ_R_F_Yh_EUR

    return_data(L111.Prod_EJ_R_F_Yh_EUR)
  } else {
    stop("Unknown command")
  }
}
