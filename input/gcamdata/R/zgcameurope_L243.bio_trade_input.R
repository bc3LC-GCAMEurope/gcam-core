# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L243.bio_trade_input
#'
#' Add European Single Market to bioenergy trade.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs:
#' @details This chunk sets up a structure for regionally differentiated bioenergy trade. Each
#' region consumes a blend of domestic and international bioenergy. Regions can supply to either
#' the domestic or international market. Share weights in the default case depend on the amount of
#' cropland a region has, with the largest region having a share weight of 1. Share weights in SSP3
#' and SSP4 are adjusted to reflect the trade frictions in the SSP storylines.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter full_join if_else group_by left_join mutate select summarize
#' @importFrom tidyr replace_na
#' @author RH October 2024
module_gcameurope_L243.bio_trade_input <- function(command, ...) {
  OUTPUTS_TO_ADJUST <- c("L243.Supplysector_Bio",
                        "L243.SectorUseTrialMarket_Bio",
                        "L243.SubsectorLogit_Bio",
                        "L243.SubsectorShrwtFllt_TradedBio",
                        "L243.StubTechCoef_ImportedBio",
                        "L243.TechCoef_TradedBio",
                        "L243.TechShrwt_TradedBio")
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     "L239.Pop_europeSingleMarket",
                     "Europe_Single_Market_Regions",
                     OUTPUTS_TO_ADJUST)
  MODULE_OUTPUTS <- paste0(OUTPUTS_TO_ADJUST, "_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    get_data_list(all_data, MODULE_INPUTS)

    SINGLE_MARKET_NAME <- unique(L239.Pop_europeSingleMarket$region)
    # Function ----------------
    change_region_to_EUR <- function(df){
      if (!"region" %in% names(df)){stop("No region column to change")}
      if ("market.name" %in% names(df)){
        df_traded <- filter(df, dplyr::if_any(where(is.character), ~ grepl("traded", .))) %>%
          mutate(region_origin = stringr::str_extract(subsector, ".*(?= traded)"),
                 region = if_else(region_origin %in% Europe_Single_Market_Regions$GCAMEU_region,
                                  SINGLE_MARKET_NAME, region)) %>% select(-region_origin)

        df_traded_addons <- df_traded %>%
          select(-subsector, -technology, -market.name) %>%
          distinct() %>%
          mutate(market.name = if_else(region == SINGLE_MARKET_NAME, gcam.USA_REGION,SINGLE_MARKET_NAME),
                 subsector = if_else(market.name == gcam.USA_REGION,
                                     paste("global", supplysector),
                                     paste(market.name, supplysector)),
                 technology = subsector,
                 minicam.energy.input = supplysector)

        return(filter(df, !dplyr::if_any(where(is.character), ~ grepl("traded", .))) %>%
          bind_rows(df_traded, df_traded_addons))
      }
      if ("subsector" %in% names(df)){
        # if there are subsectors, we also need to filter to the correct european regions
        df_traded <- filter(df, dplyr::if_any(where(is.character), ~ grepl("traded", .))) %>%
          mutate(region_origin = stringr::str_extract(subsector, ".*(?= traded)"),
                 region = if_else(region_origin %in% Europe_Single_Market_Regions$GCAMEU_region,
                                  SINGLE_MARKET_NAME, region)) %>% select(-region_origin)

        if ("technology" %in% names(df)){
          df_traded_addons <- df_traded %>%
            select(-subsector, -technology) %>%
            distinct() %>%
            mutate(subsector = if_else(region == SINGLE_MARKET_NAME, paste("global", supplysector),
                                       paste(SINGLE_MARKET_NAME, supplysector)),
                   technology = subsector)
        } else {
          df_traded_addons <- df_traded %>%
            select(-subsector) %>%
            distinct() %>%
            mutate(subsector = if_else(region == SINGLE_MARKET_NAME, paste("global", supplysector),
                                       paste(SINGLE_MARKET_NAME, supplysector)))
        }


        return(filter(df, !dplyr::if_any(where(is.character), ~ grepl("traded", .))) %>%
          bind_rows(df_traded, df_traded_addons))
      } else {
        # if only supplysector, just change the region
        filter(df, if_any(where(is.character), ~ grepl("traded", .))) %>%
          mutate(region = SINGLE_MARKET_NAME) %>%
          bind_rows(df)
      }
    }

    change_market_to_EUR <- function(df){
      if (!"market" %in% names(df)){stop("No market column to change")}
        df %>%
          mutate(market = if_else(region %in% Europe_Single_Market_Regions$GCAMEU_region,
                 SINGLE_MARKET_NAME, market))
    }

    # Change market names ---------------------
    L243.Supplysector_Bio_EUR <- change_region_to_EUR(L243.Supplysector_Bio)
    L243.SectorUseTrialMarket_Bio_EUR <- change_region_to_EUR(L243.SectorUseTrialMarket_Bio)
    L243.SubsectorLogit_Bio_EUR <- change_region_to_EUR(L243.SubsectorLogit_Bio)
    L243.StubTechCoef_ImportedBio_EUR <- change_market_to_EUR(L243.StubTechCoef_ImportedBio)
    L243.TechCoef_TradedBio_EUR <- change_region_to_EUR(L243.TechCoef_TradedBio)
    L243.TechShrwt_TradedBio_EUR <- change_region_to_EUR(L243.TechShrwt_TradedBio)

    # Subsector Shareweights---------------
    # shareweights are based on cropland area
    # for european market, just keep same shareweights, so relative shareweight between countries is the same
    L243.SubsectorShrwtFllt_TradedBio_EUR <- L243.SubsectorShrwtFllt_TradedBio %>%
      select(-share.weight) %>%
      change_region_to_EUR() %>%
      left_join(L243.SubsectorShrwtFllt_TradedBio %>%  select(-region), by = c("supplysector", "subsector", "year.fillout"))

    # For european exports to global market, sum european shareweights
    europe_to_global_shrwt <- L243.SubsectorShrwtFllt_TradedBio_EUR %>%
      filter(region == SINGLE_MARKET_NAME) %>%
      summarise(share.weight = sum(share.weight, na.rm = T)) %>%  pull

    # For global exports to european market, give same as sum of european shareweights
    # equal shareweight to domestic and foreign biomass
    global_to_europe_shrwt <- europe_to_global_shrwt

    L243.SubsectorShrwtFllt_TradedBio_EUR <- L243.SubsectorShrwtFllt_TradedBio_EUR %>%
      mutate(share.weight = if_else(subsector == paste("global", supplysector), global_to_europe_shrwt, share.weight),
             share.weight = if_else(subsector == paste(SINGLE_MARKET_NAME, supplysector), europe_to_global_shrwt, share.weight))


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
