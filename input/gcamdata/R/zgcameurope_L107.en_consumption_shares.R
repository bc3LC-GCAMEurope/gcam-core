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
    FILE  = "gcam-europe/A07.hh_DIAMOND",
    FILE  = "gcam-europe/A07.hh_DIAMOND_varList",
    FILE  = "gcam-europe/A44.en_consumption_shares_hp_EUR",
    FILE = "gcam-europe/mappings/hh_items_techs_map")
  MODULE_OUTPUTS <- c("L107.en_consumption_shares_EUR")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    n_cons <- as.numeric(length(unique(A07.hh_DIAMOND$Decile)))

    # NOTE: we assume same population accros hh groups
    A07.hh_DIAMOND <- A07.hh_DIAMOND %>%
      # # rename deciles column
      mutate(Decile = as.character(Decile),
             Decile = paste0('d',as.character(Decile))) %>%
      # select relevant coicop items
      select(ISO3, Decile,
             EUR_HE0451, EUR_HE0452, EUR_HE0453, EUR_HE0454, EUR_HE0455) %>%
      pivot_longer(cols = c('EUR_HE0451', 'EUR_HE0452', 'EUR_HE0453', 'EUR_HE0454', 'EUR_HE0455'), names_to = 'COICOP', values_to = 'value') %>%
      left_join(A07.hh_DIAMOND_varList, by = 'COICOP') %>%
      # compute shares by country & technology among Deciles
      group_by(ISO3, Item) %>%
      mutate(total_tech_expenditure = sum(value),
             share = value / total_tech_expenditure) %>%
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

    # Load pre-computed shares for heatpump technologies

    # First select the model to estimate the shares: c("liquidity", "blend", "smoothed", "engel", "quadratic")
    hp_model <- "smoothed"

    A07.hh_DIAMOND_hp <- A44.en_consumption_shares_hp_EUR %>%
      mutate(GCAM_region_ID = as.integer(GCAM_region_ID)) %>%
      select(-engel_eps) %>%
      pivot_longer(cols = -c(region, decile, GCAM_region_ID, consumption.category),
                   names_to = "model", values_to = "share") %>%
      filter(model == hp_model) %>%
      select(-model)


    L107.en_consumption_shares_EUR <- bind_rows(
      A07.hh_DIAMOND,
      A07.hh_DIAMOND_hp
    )

    # Manual fix: set Germany as Austria's proxy (HH DIAMOND db misses Austria) + no data info

    proxy_map <- tibble::tribble(
      ~proxy_region, ~region,    ~GCAM_region_ID,
      "Germany",     "Austria",   28,
      "Croatia",     "Albania",   55,
      "Croatia",     "Bosnia and Herzegovina",   57,
      "Finland",     "Iceland",   58,
      "Croatia",     "Macedonia",   59,
      "Bulgaria",     "Moldova",   60,
      "Sweden",     "Norway",   61,
      "Croatia",     "Serbia and Montenegro",   62,
      "Greece",     "Turkey",   64,
      "Ireland",     "UK",   65,
      "Bulgaria",     "Ukraine",   66,
    )

    proxy_rows <- purrr::pmap_dfr(proxy_map, function(proxy_region, region, GCAM_region_ID) {
      L107.en_consumption_shares_EUR %>%
        filter(region == proxy_region) %>%
        mutate(region = .env$region, GCAM_region_ID = .env$GCAM_region_ID)
    })


    L107.en_consumption_shares_EUR <- bind_rows(L107.en_consumption_shares_EUR, proxy_rows)


    # Expand shares to all technologies
    L107.en_consumption_shares_EUR <- L107.en_consumption_shares_EUR %>%
      left_join(hh_items_techs_map, by  = "consumption.category",
                relationship = "many-to-many") %>%
      select(-consumption.category)


    # # If all shares equal to zero it is because the tech is not in the DIAMOND data
    # # Build an artifact (equal split) to ensure all cmbinatios are present and avoid joining issues
    L107.en_consumption_shares_EUR <- L107.en_consumption_shares_EUR %>%
      group_by(GCAM_region_ID, fuel, service, subsector, technology) %>%
      mutate(share = if (sum(share) == 0) 1/n_cons else share) %>%
      ungroup()


    #===================================================
    # Produce outputs
    L107.en_consumption_shares_EUR %>%
      add_title("Technology consumption shares by country and hh group") %>%
      add_units("Share") %>%
      add_comments("Only EU-12 & EU-15 countries") %>%
      add_legacy_name("L107.en_consumption_shares_EUR") %>%
      add_precursors("common/GCAM32_to_EU","gcam-europe/A07.hh_DIAMOND",
                     "gcam-europe/A07.hh_DIAMOND_varList", "gcam-europe/A44.en_consumption_shares_hp_EUR") ->
      L107.en_consumption_shares_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
