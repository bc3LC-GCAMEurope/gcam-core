# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L3222.CTax
#'
#' Produce custom carbon cap targets and markets
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L3222.CTax}, \code{L3222.CTax_Link}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L3222.CTax <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "policy/A_CTax",
                     FILE = "policy/mappings/ghg_link",
                     FILE = "policy/mappings/market_region_mappings")
  MODULE_OUTPUTS <- c("L3222.CTax",
                      "L3222.CTax_GHG_Link")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    A_CTax <- A_CTax %>% mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))


    # Write tax to all regions (no reason not to and necessary sometimes)
    L3222.CTax <- A_CTax %>%
      gather_years(value_col = "fixedTax") %>%
      filter(!is.na(fixedTax)) %>%
      select(-link.type) %>%
      left_join(market_region_mappings, by = "market") %>%
      tidyr::replace_na(list(market.overwrite = 0)) %>%
      mutate(region = if_else(!is.na(region), region, market),
             market = if_else(market.overwrite == 1, region, market)) %>%
      select(-market.overwrite)

    # Get ghg link for each
    L3222.CTax_GHG_Link <- A_CTax %>%
      left_join(market_region_mappings, by = "market") %>%
      tidyr::replace_na(list(market.overwrite = 0)) %>%
      mutate(region = if_else(!is.na(region), region, market),
             market = if_else(market.overwrite == 1, region, market)) %>%
      distinct(xml, link.type, market, region, ghgpolicy) %>%
      filter(!is.na(link.type)) %>%
      left_join(ghg_link, by = "link.type") %>%
      rename(linked.policy = ghgpolicy) %>%
      select(-link.type)

    # Produce outputs
    L3222.CTax %>%
      add_title("Carbon taxes", overwrite = T) %>%
      add_units("$1975/MTC") %>%
      add_precursors("policy/A_CTax") ->
      L3222.CTax

    L3222.CTax_GHG_Link %>%
      add_title("GHG links for carbon taxes", overwrite = T) %>%
      add_units("Various") %>%
      add_precursors("policy/A_CTax",
                     "policy/mappings/ghg_link") ->
      L3222.CTax_GHG_Link

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}

