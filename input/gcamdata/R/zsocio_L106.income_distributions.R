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
             FILE = "socioeconomics/Rao_multimodel_income_deciles"))
  } else if(command == driver.MAKE) {

    # Set SSP and Model type that we want to use
    ssp <- "SSP2"
    model_type <- "PCA algorithm (Two Components)"

    all_data <- list(...)[[1]]

    # Load data
    region_map <- get_data(all_data, "common/GCAM_region_names")
    income_dists_raw <- get_data(all_data, "socioeconomics/Rao_multimodel_income_deciles")

    # Process
    income_dists_raw %>%
      filter(sce %in% c("Historical data", ssp),
             year %in% MODEL_YEARS,
             model %in% c(model_type, "Historical data")) %>%
      mutate(subregional.population.share = 0.1) %>% #TODO: Don't hard code this?
      left_join(region_map, by = "GCAM_region_ID") %>%
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

    # Produce outputs, add appropriate flags and comments
    tibble(L106.income_distributions) %>%
      add_units("None") %>%
      add_precursors("common/GCAM_region_names", "socioeconomics/Rao_multimodel_income_deciles") %>%
      add_comments("Income distributions filtered by SSP and model type") ->
      L106.income_distributions

    return_data(L106.income_distributions)
  } else {
    stop("Unknown command")
  }
}
