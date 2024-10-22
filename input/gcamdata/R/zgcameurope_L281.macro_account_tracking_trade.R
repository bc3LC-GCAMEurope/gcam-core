# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L281.macro_account_tracking
#'
#' Generate tibbles to add "hooks" into GCAM to be able to track certain accounts such as
#' final energy service and net energy trade.  In addition generate the trial value market/resources
#' for those accounts as well as the capital investment tracking markets.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:
#' There is no corresponding file in the original data system.
#' @details National accounts data and GDP macro function parameters for GCAM regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter lag mutate mutate_at select rename
#' @author SHK October 2020
#'
module_gcameurope_L281.macro_account_tracking <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     "L281.TechAccountOutput_entrade",
                     "L281.TechAccountInput_entrade",
                     "L281.TechAccountInput_NG_entrade",
                     "L281.GlobalTechAccountInput_entrade",
                     "L239.TechCoef_tra_EUR",
                     "L239.TechCoef_reg_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L281.TechAccountOutput_entrade_EUR"))
             # "L281.TechAccountInput_entrade_EUR"))
             # "L281.TechAccountInput_NG_entrade",
             # "L281.GlobalTechAccountInput_entrade_EUR"))
  } else if(command == driver.MAKE) {

    # 1. Read data --------------------------------------------------

    all_data <- list(...)[[1]]

    get_data_list(all_data, MODULE_INPUTS)

    # 2. Adjust markets for europe single market --------------------------------------------------
    L281.TechAccountOutput_entrade_EUR <- L281.TechAccountOutput_entrade %>%
      left_join(L239.TechCoef_tra_EUR, by = c("supplysector", "subsector", "technology", "year", "market.name")) %>%
      mutate(region = if_else(is.na(region.y), region.x, region.y)) %>%
      select(names(L281.TechAccountOutput_entrade))

    lng_trade_regions <- L281.TechAccountOutput_entrade_EUR %>%
      filter(supplysector %in% L239.TechCoef_tra_EUR$supplysector) %>%
      distinct(region_lng = region, market.name)

    L281.TechAccountOutput_entrade_EUR  <- L281.TechAccountOutput_entrade_EUR %>%
      left_join(lng_trade_regions, by = c("market.name")) %>%
      mutate(region = if_else(supplysector == "traded LNG", region_lng, region)) %>%
      select(-region_lng)

    L281.TechAccountOutput_entrade_EUR %>%  filter(year == 2015, market.name == "Spain")
    # ===================================================

    # Produce outputs
    L281.TechAccountOutput_entrade_EUR %>%
      add_title("Net energy trade / exports accounting in supplysectors") %>%
      add_units("Unitless") %>%
      add_comments("Sets up accounting secondary outputs to track regional exports of primary energy") %>%
      add_precursors("L239.TechCoef_tra", "L243.TechCoef_TradedBio", "L2392.TechCoef_tra_NG") ->
      L281.TechAccountOutput_entrade_EUR


    return_data(L281.TechAccountOutput_entrade_EUR)
  } else {
    stop("Unknown command")
  }
}
