# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L106.income_distributions
#'
#' Read in raw income distribution data and transform into inputs other chunks can use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}.
#' @author EL August 2022
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select
#
module_socio_L106.income_distributions <- function(command, ...) {
  if(command == driver.DECLARE_OUTPUTS) {
    return(c("L106.income_distributions"))
  } else if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "socioeconomics/income_shares",
             FILE = "socioeconomics/Rao_multimodel_income_deciles"))
  } else if(command == driver.MAKE) {

    # Set SSP and Model type that we want to use
    ssp <- "SSP2"
    model_type <- "PCA algorithm (Two Components)"

    all_data <- list(...)[[1]]

    # Load data
    region_map <- get_data(all_data, "common/GCAM_region_names")
    income_dist_row <- get_data(all_data, "socioeconomics/income_shares") %>%
      select(-gdp_pcap_decile)
    income_dist_eur <- get_data(all_data, "socioeconomics/Rao_multimodel_income_deciles") %>%
      left_join_error_no_match(region_map, by = "GCAM_region_ID")

    # Create a income distribution datasets that substitutes non-Europe coutires by latest data (socioeconomics/income_shares)
    income_dist_pre <- income_dist_eur %>%
      anti_join(income_dist_row, by = "region") %>%
      bind_rows(income_dist_row) %>%
      filter(region %in% unique(region_map$region)) %>%
      mutate(year = if_else(year == 2020, 2021L, year))

    # Process
    income_dist_pre %>%
      filter(sce %in% c("Historical data", ssp),
             year %in% MODEL_YEARS,
             model %in% c(model_type, "Historical data")) %>%
      mutate(subregional.population.share = 0.1) %>% #TODO: Don't hard code this?
      rename(subregional.income.share = shares,
             gcam.consumer = category) %>%
      select(region, gcam.consumer, subregional.population.share, subregional.income.share, year) ->
      L106.income_distributions

    # Verify that income shares add up to ~1
    L106.income_distributions %>%
      group_by(region, year) %>%
      summarize(value = sum(subregional.income.share)) -> test_shares
    if(max(test_shares$value) > 1.001 || min(test_shares$value < 0.999)){
      stop("Income shares don't add up to 1")
    }

    # Need to extend the shares to 2021:
    L106.income_distributions <- L106.income_distributions %>%
      complete(nesting(region, gcam.consumer), year = MODEL_BASE_YEARS) %>%
      group_by(region, gcam.consumer) %>%
      mutate(subregional.population.share = approx_fun(year, subregional.population.share, rule = 2),
              subregional.income.share = approx_fun(year, subregional.income.share, rule = 2)) %>%
      ungroup()


    # Produce outputs, add appropriate flags and comments
    tibble(L106.income_distributions) %>%
      add_units("None") %>%
      add_precursors("common/GCAM_region_names", "socioeconomics/income_shares", "socioeconomics/Rao_multimodel_income_deciles") %>%
      add_comments("Income distributions filtered by SSP and model type") ->
      L106.income_distributions

    return_data(L106.income_distributions)
  } else {
    stop("Unknown command")
  }
}
