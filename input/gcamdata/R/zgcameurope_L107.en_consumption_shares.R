# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L107.en_consumption_shares
#'
#' Read in raw income distribution microdata fuel usage and transform into inputs other chunks can use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L107.en_consumption_shares_EUR} (energy level1).
#' @details Calculates fuel shares by income group
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join lag mutate pull select summarise rowwise
#' @importFrom tidyr complete replace_na
#' @author BC3 2026
module_gcameurope_L107.en_consumption_shares <- function(command, ...) {

  MODULE_INPUTS <- c(
    FILE = "common/GCAM32_to_EU",
    FILE  = "gcam-europe/A107.hh_DIAMOND",
    FILE  = "gcam-europe/A107.hh_DIAMOND_varList")
  MODULE_OUTPUTS <- c("L107.en_consumption_shares_EUR")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    A107.hh_DIAMOND <- A107.hh_DIAMOND %>%
      # compute per capita income & total population by decile
      mutate(income = (HY010 * Households) / (Population * Households),
             total_pop = Households * Population) %>%
      # # rename deciles column
      mutate(Decile = as.character(Decile),
             Decile = paste0('d',as.character(Decile))) %>%
      # select relevant coicop items
      select(ISO3, Decile, Households, Population, HY010, total_pop, income,
             EUR_HE0451, EUR_HE0452, EUR_HE0453, EUR_HE0454, EUR_HE0455) %>%
      pivot_longer(cols = c('EUR_HE0451', 'EUR_HE0452', 'EUR_HE0453', 'EUR_HE0454', 'EUR_HE0455'), names_to = 'COICOP', values_to = 'value') %>%
      left_join(A107.hh_DIAMOND_varList, by = 'COICOP') %>%
      # compute shares by country & technology among Deciles
      group_by(ISO3, Item) %>%
      mutate(total_tech_expenditure = sum(value * total_pop),
             share = value * total_pop / total_tech_expenditure) %>%
      ungroup() %>%
      # fix NAs (due to total_tech_expenditure)
      mutate(share = if_else(is.na(share), 0, share)) %>%
      # use full ctry spellling
      mutate(ISO3 = tolower(ISO3),
             Item = tolower(Item)) %>%
      left_join(GCAM32_to_EU %>%
                  select(ISO3 = iso, region = GCAMEU_region, GCAM_region_ID), by = c('ISO3')) %>%
      # select relevant columns
      select(region, decile = Decile, consumption.category = Item, share, GCAM_region_ID)

    # add "other" category with uniform shares
    L107.en_consumption_shares_EUR <- A107.hh_DIAMOND %>%
      select(region, decile, GCAM_region_ID) %>%
      distinct() %>%
      mutate(consumption.category = 'other', share = 0.1) %>%
      bind_rows(A107.hh_DIAMOND)


    #===================================================
    # Produce outputs
    L107.en_consumption_shares_EUR %>%
      add_title("Technology consumption shares by country and hh group") %>%
      add_units("Share") %>%
      add_comments("Only EU-12 & EU-15 countries") %>%
      add_legacy_name("L107.en_consumption_shares_EUR") %>%
      add_precursors("common/GCAM32_to_EU","gcam-europe/A107.hh_DIAMOND",
                     "gcam-europe/A107.hh_DIAMOND_varList") ->
      L107.en_consumption_shares_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
