# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L240.ag_trade
#'
#' Adjust ag trade for European Single Market
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs:
#' @details Build datasets for ssp4 agricultural trade: food and nonfood trade coefficients, feed trade
#' coefficients, restricted agricultural trade, and trade regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else left_join mutate rename select
#' @importFrom tidyr replace_na
#' @importFrom tibble tibble
#' @author RH November 2024
module_gcameurope_L240.ag_trade <- function(command, ...) {
  OUTPUTS_TO_ADJUST <-
    c("L240.Supplysector_tra",
      "L240.SectorUseTrialMarket_tra",
      "L240.SubsectorAll_tra",
      "L240.TechShrwt_tra",
      "L240.TechCost_tra",
      "L240.TechCoef_tra",
      "L240.Production_tra",
      "L240.Supplysector_reg",
      # "L240.SubsectorAll_reg", # no need to copy
      # "L240.TechShrwt_reg", # no need to copy
      # "L240.Production_reg_dom", # no need to copy
      "L240.TechCoef_reg") # done

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "common/iso_GCAM_regID",
      FILE = "gcam-europe/A_ff_RegionalTechnology_EUR",
      FILE = "gcam-europe/A_agRegionalSector_EEA",
      FILE = "gcam-europe/A_agTradedSector_EEA",
      "Europe_Single_Market_Regions",
      "EuroSingleMarket_BiTrade_Ag",
      "L240.Production_reg_imp", # no need to adjust, but need data
      OUTPUTS_TO_ADJUST)

  MODULE_OUTPUTS <- paste(OUTPUTS_TO_ADJUST, "EUR", sep = "_")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    SINGLE_MARKET_NAME <- unique(A_ff_RegionalTechnology_EUR$market.name)
    SINGLE_MARKET_NAME <- SINGLE_MARKET_NAME[SINGLE_MARKET_NAME != "regional"]


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
    L240.Supplysector_tra_EUR <- mutate(A_agTradedSector_EEA, logit.year.fillout = min(MODEL_BASE_YEARS),
           region = SINGLE_MARKET_NAME) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type")) %>%
      bind_rows(L240.Supplysector_tra)

    L240.Supplysector_reg_EUR <- mutate(A_agRegionalSector_EEA, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                           filter(GCAM_region_names, region %in% Europe_Single_Market_Regions$GCAMEU_region)) %>%
      bind_rows(L240.Supplysector_reg %>% filter(!region %in% Europe_Single_Market_Regions$GCAMEU_region))

    L240.SectorUseTrialMarket_tra_EUR <- copy_for_EUR(L240.SectorUseTrialMarket_tra)

    # 1b: Add euro market to subsector ------------------------------------
    L240.SubsectorAll_tra_EUR <- add_single_market(L240.SubsectorAll_tra)

    #
    # 1c: Add euro market to techs ----------------------
    L240.TechShrwt_tra_EUR <- add_single_market(L240.TechShrwt_tra)
    L240.TechCost_tra_EUR <- add_single_market(L240.TechCost_tra)
    L240.TechCoef_tra_EUR <- add_single_market(L240.TechCoef_tra)  %>%
      mutate(market.name = if_else(grepl(SINGLE_MARKET_NAME, subsector), SINGLE_MARKET_NAME, market.name),
             market.name = if_else(grepl("global", subsector), gcam.USA_REGION, market.name),
             minicam.energy.input = if_else(market.name == SINGLE_MARKET_NAME | grepl("global", subsector),
                                            supplysector, minicam.energy.input))

    # Here only need to change market.name
    L240.TechCoef_reg_EUR <- L240.TechCoef_reg %>%
      mutate(market.name = if_else(grepl("imported", subsector) & region %in% Europe_Single_Market_Regions$GCAMEU_region,
                                   SINGLE_MARKET_NAME,
                                   market.name))

    # 2a: Calculate trade between europe and globe ------------------------------------
    # Calculate net trade in GCAM
    Europe_imports <- L240.Production_reg_imp %>%
      filter(region %in% Europe_Single_Market_Regions$GCAMEU_region) %>%
      group_by(sector = subsector, year) %>%
      summarise(imports = sum(calOutputValue )) %>%
      ungroup %>%
      mutate(region = SINGLE_MARKET_NAME,
             sector = stringr::str_remove(sector, "imported "))

    Europe_exports <- L240.Production_tra %>%
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

    commodity_input_map <- L240.TechCoef_tra %>%
      distinct(sector = supplysector, GCAM_commodity = minicam.energy.input) %>%
      mutate(sector = stringr::str_remove(sector, "traded "))

    # Calculate net trade from FAO data
    Europe_net_trade_FAO <- EuroSingleMarket_BiTrade_Ag %>%
      filter(import_GCAM_region_ID < 0 | export_GCAM_region_ID < 0,
             GCAM_commodity %in% c(aglu.TRADED_CROPS, aglu.TRADED_MEATS)) %>%
      mutate(flow = if_else(import_GCAM_region_ID < 0, "exports", "imports")) %>%
      group_by(GCAM_commodity, flow, year) %>%
      # data is in thousand tonnes, convert to Mt
      summarise(value = sum(value) / 1000) %>%
      ungroup %>%
      tidyr::pivot_wider(names_from = "flow", values_from = "value") %>%
      mutate(net_exports = exports - imports)

    # scale exports and imports according to net exports in GCAM
    Europe_net_trade_calib_calc <- Europe_net_trade_FAO %>%
      left_join_error_no_match(commodity_input_map, by = "GCAM_commodity") %>%
      left_join_error_no_match(Europe_net_trade_GCAM, by = c("year", "sector")) %>%
      mutate(scalar = net_exports_GCAM / net_exports,
             # if the scalar is negative, we are just going to add to imports/exports to reach net_exports
             exports = if_else(scalar > 0, exports * scalar,
                                  if_else(net_exports_GCAM > 0, net_exports_GCAM + imports, exports)),
             imports = if_else(scalar > 0, imports * scalar,
                                  if_else(net_exports_GCAM < 0, -net_exports_GCAM + exports, imports)),
             # the limit of the exports is the sum of the exports from each region
             # technically this is not necessary, but prevents explicit re-exportation (ie of oil palm)
             imports = if_else(exports > exports_GCAM, imports_GCAM, imports),
             exports = if_else(exports > exports_GCAM, exports_GCAM, exports),
             net_exports_new = exports - imports) %>%
      select(sector, region, year, exports, imports)

    # add in any years we don't have in FAO with the GCAM net export values
    Europe_net_trade_calib <- Europe_net_trade_calib_calc %>%
      bind_rows(Europe_net_trade_GCAM %>%
                  mutate(exports = if_else(net_exports_GCAM > 0, net_exports_GCAM, 0),
                         imports = if_else(net_exports_GCAM < 0, -net_exports_GCAM, 0)) %>%
                  select(names(Europe_net_trade_calib_calc)) %>%
                  anti_join(Europe_net_trade_calib_calc, by = c("sector", "year", "region"))) %>%
      tidyr::pivot_longer(cols = c(exports, imports), names_to = "flow", values_to = "value") %>%
      mutate(region = if_else(flow == "imports", "global", region),
             subsector = paste(region, "traded", sector, sep = " ")) %>%
      select(subsector, year, value)

    L240.Production_tra_EUR <- L240.Production_tra %>%
      # this will add in global/Europe single markets, but the cal output values are wrong
      add_single_market %>%
      left_join(Europe_net_trade_calib, by = c("subsector", "year")) %>%
      mutate(calOutputValue = if_else(is.na(value), calOutputValue, value),
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])
    #

    # outputs -------------------
    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
