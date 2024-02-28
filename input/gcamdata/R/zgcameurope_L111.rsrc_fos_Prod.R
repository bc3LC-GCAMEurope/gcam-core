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
  if(command == driver.DECLARE_INPUTS) {
    return(c("L1012.en_bal_EJ_R_Si_Fi_Yh_EUR"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L111.Prod_EJ_R_F_Yh_EUR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    sector <- fuel <- year <- value <- share <- iso <- GCAM_region_ID <- unconventionals <- value.x <-
      value.y <- FLOW <- PRODUCT <- resource <- region_GCAM3 <- CumulSum <- subresource <- grade <-
      available <- available_region_GCAM3 <- extractioncost <- technology <-  . <- NULL  # silence package check notes

    # Load required inputs
    L1012.en_bal_EJ_R_Si_Fi_Yh_EUR <- get_data(all_data, "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR", strip_attributes = TRUE)

    # ------- HISTORICAL FOSSIL ENERGY PRODUCTION -------
    # Determine production for each primary fuel by country
    L111.Prod_EJ_R_F_Yh_EUR <- L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(sector == "out_resources", fuel %in% energy.RSRC_FUELS, year %in% HISTORICAL_YEARS) %>%
      mutate(technology = fuel)

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
