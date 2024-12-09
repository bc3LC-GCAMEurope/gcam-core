# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L238.iron_steel_trade
#'
#' Model input for regional and (globally) traded iron and steel
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L238.Supplysector_tra},
#'   \code{L238.SectorUseTrialMarket_tra}, \code{L238.SubsectorAll_tra}, \code{L238.TechShrwt_tra},
#'   \code{L238.TechCost_tra}, \code{L238.TechCoef_tra}, \code{L238.Production_tra}, \code{L238.Supplysector_reg},
#'   \code{L238.SubsectorAll_reg}, \code{L238.TechShrwt_reg}, \code{L238.TechCoef_reg}, \code{L238.Production_reg_imp},
#'   \code{L238.Production_reg_dom}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else left_join mutate rename select
#' @importFrom tidyr replace_na
#' @importFrom tibble tibble
#' @author Siddarth Durga July 2022
module_gcameurope_L238.iron_steel_trade <- function(command, ...) {
  OUTPUTS_TO_ADJUST <-
    c("L238.Supplysector_tra", # done
      "L238.SectorUseTrialMarket_tra", # done
      "L238.SubsectorAll_tra", # done
      "L238.TechShrwt_tra", # done
      "L238.TechCost_tra", # done
      "L238.TechCoef_tra", # done
      "L238.Production_tra", # done
      # "L238.Supplysector_reg", # no changes needed
      # "L238.SubsectorAll_reg", # no changes needed
      # "L238.TechShrwt_reg", # no changes needed
      "L238.TechCoef_reg") # done
      # "L238.Production_reg_imp", # no changes needed
      # "L238.Production_reg_dom") # no changes needed
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     "Europe_Single_Market_Regions",
                     "LB1092.Single_Market_Trade_Steel",
                     "L238.Production_reg_imp",
                     OUTPUTS_TO_ADJUST)
  MODULE_OUTPUTS <- paste(OUTPUTS_TO_ADJUST, "EUR", sep = "_")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs -----------------------
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)
    SINGLE_MARKET_NAME <- LB1092.Single_Market_Trade_Steel %>% distinct(Exporter_Region) %>% filter(Exporter_Region != "global") %>% pull

    # 0: functions ----------
    copy_for_EUR <- function(df){
      if ("region" %in% names(df)){
        df %>%
          mutate(region = SINGLE_MARKET_NAME) %>%
          bind_rows(df)
      } else { df }
    }

    add_single_market <- function(df){
      df_singleMarket <- df %>%
        mutate(region_tmp = stringr::str_extract(subsector, ".*(?= traded)"),
               region = if_else(region_tmp %in% Europe_Single_Market_Regions$GCAMEU_region, SINGLE_MARKET_NAME, region))
      # now copy one region and rename it to global tech
      df_subsector_global <- df_singleMarket %>%
        filter(region == SINGLE_MARKET_NAME)

      df_subsector_EUR <- df_singleMarket %>%
        filter(region == gcam.USA_REGION)

      if ("year" %in% names(df_singleMarket)){
        df_subsector_global <- group_by(df_subsector_global, supplysector, year)
        df_subsector_EUR <- group_by(df_subsector_EUR, supplysector, year)
      } else {
        df_subsector_global <- group_by(df_subsector_global, supplysector)
        df_subsector_EUR <- group_by(df_subsector_EUR, supplysector)
      }

      df_subsector_global <-  dplyr::slice_head(df_subsector_global) %>%
        ungroup %>%
        mutate(subsector = stringr::str_replace(subsector, region_tmp, "global"))

      df_subsector_EUR <- dplyr::slice_head(df_subsector_EUR) %>%
        ungroup %>%
        mutate(subsector = stringr::str_replace(subsector, region_tmp, SINGLE_MARKET_NAME)) %>%
        bind_rows(df_subsector_global, df_singleMarket)

      if ("technology" %in% names(df)){
        df_subsector_EUR %>% mutate(technology = subsector)  %>% select(-region_tmp)
      } else { df_subsector_EUR %>% select(-region_tmp) }
    }

    # 1a: Add euro market to supplysector region ----------------------
    L238.Supplysector_tra_EUR <- copy_for_EUR(L238.Supplysector_tra)
    L238.SectorUseTrialMarket_tra_EUR <- copy_for_EUR(L238.SectorUseTrialMarket_tra)

    # 1b: Add euro market to subsector ------------------------------------
    L238.SubsectorAll_tra_EUR <- add_single_market(L238.SubsectorAll_tra)

    # 1c: Add euro market to techs ----------------------
    L238.TechShrwt_tra_EUR <- add_single_market(L238.TechShrwt_tra)
    L238.TechCost_tra_EUR <- add_single_market(L238.TechCost_tra)
    L238.TechCoef_tra_EUR <- add_single_market(L238.TechCoef_tra)  %>%
      mutate(market.name = if_else(grepl(SINGLE_MARKET_NAME, subsector), SINGLE_MARKET_NAME, market.name),
             market.name = if_else(grepl("global", subsector), gcam.USA_REGION, market.name),
             minicam.energy.input = if_else(market.name == SINGLE_MARKET_NAME | grepl("global", subsector),
                                            supplysector, minicam.energy.input))
    # Here only need to change market.name
    L238.TechCoef_reg_EUR <- L238.TechCoef_reg %>%
      mutate(market.name = if_else(grepl("imported", subsector) & region %in% Europe_Single_Market_Regions$GCAMEU_region,
                                   SINGLE_MARKET_NAME,
                                   market.name))

    # 2a: Calculate trade between europe and globe ------------------------------------
    # Calculate net trade in GCAM
    Europe_imports <- L238.Production_reg_imp %>%
      filter(region %in% Europe_Single_Market_Regions$GCAMEU_region) %>%
      group_by(sector = subsector, year) %>%
      summarise(imports = sum(calOutputValue )) %>%
      ungroup %>%
      mutate(region = SINGLE_MARKET_NAME,
             sector = stringr::str_remove(sector, "imported "))

    Europe_exports <- L238.Production_tra %>%
      mutate(region_export = stringr::str_extract(subsector, ".*(?= traded)")) %>%
      filter(region_export %in% Europe_Single_Market_Regions$GCAMEU_region) %>%
      group_by(sector = supplysector, year) %>%
      summarise(exports = sum(calOutputValue )) %>%
      ungroup %>%
      mutate(region = SINGLE_MARKET_NAME,
             sector = stringr::str_remove(sector, "traded "))

    Europe_net_trade_GCAM <- left_join_error_no_match(Europe_imports, Europe_exports,
                                                      by = c("sector", "year", "region")) %>%
      mutate(net_exports_GCAM = exports - imports) %>%
      rename(exports_GCAM = exports, imports_GCAM = imports)

    commodity_input_map <- L238.TechCoef_tra %>%
      distinct(sector = supplysector, GCAM_commodity = minicam.energy.input) %>%
      mutate(sector = stringr::str_remove(sector, "traded "))

    # scale exports and imports according to net exports in GCAM
    Europe_net_trade_calib_calc <- LB1092.Single_Market_Trade_Steel %>%
      # Calculate EU net exports
      filter(Exporter_Region != Importer_Region,
             year %in% MODEL_BASE_YEARS) %>%
      mutate(flow = if_else(Exporter_Region == SINGLE_MARKET_NAME, "exports", "imports")) %>%
      select(-Exporter_Region, -Importer_Region) %>%
      tidyr::pivot_wider(names_from = flow, values_from = Weight_Mt) %>%
      # now add in GCAM calibration and scale
      repeat_add_columns(commodity_input_map) %>%
      full_join(Europe_net_trade_GCAM, by = c("year", "sector")) %>%
      # there will be NAs for base years that have not been included
      # simply assume alll net_exports go to either exports/imports
      mutate(exports = if_else(is.na(exports), pmax(0, net_exports_GCAM), exports),
             imports = if_else(is.na(imports), pmax(0, -net_exports_GCAM), imports),
             net_exports = exports - imports) %>%
      # net exports are far more negative in GCAM, so using scalars will create crazy results
      # instead going to increase/decrease exports/imports by same amount to reach net_exports in GCAM
      mutate(diff = net_exports_GCAM - net_exports ,
             # if the scalar is negative, we are just going to add to imports/exports to reach net_exports
             exports = exports + diff / 2,
             imports = imports - diff / 2,
             net_exports_new = exports - imports) %>%
      select(sector, region, year, exports, imports) %>%
      tidyr::pivot_longer(cols = c(exports, imports), names_to = "flow", values_to = "value") %>%
      mutate(region = if_else(flow == "imports", "global", region),
             subsector = paste(region, "traded", sector, sep = " ")) %>%
      select(subsector, year, value)

    L238.Production_tra_EUR <- L238.Production_tra %>%
      # this will add in global/Europe single markets, but the cal output values are wrong
      add_single_market %>%
      left_join(Europe_net_trade_calib_calc, by = c("subsector", "year")) %>%
      mutate(calOutputValue = if_else(is.na(value), calOutputValue, value),
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])
    #


    # Outputs ----------------------------------
    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
