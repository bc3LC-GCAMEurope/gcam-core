# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L2011.ff_ALL_R_C_Y
#'
#' Calculate fossil fuel energy balances, by region / commodity / year and harmonize GCAM's data to Comtrade's.
#' Need to combine Eurostat and IEA data since the production values differ and therefore the calibrated net trade data differs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2011.ff_ALL_EJ_R_C_Y_EUR} \code{L2011.ff_GrossTrade_EJ_R_C_Y_EUR}.
#' @details This chunk combines fossil fuel production and consumption to calculate energy balances by
#' GCAM region, commodity and year. After calculating GCAM's (assumed) net trade
#' Comtrade's gross trade is adjusted to match.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RH Feb 2024
module_gcameurope_L2011.ff_ALL_R_C_Y <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     "L1012.en_bal_EJ_R_Si_Fi_Yh",
                     "L121.in_EJ_R_TPES_crude_Yh",
                     "L121.in_EJ_R_TPES_unoil_Yh",
                     "L111.Prod_EJ_R_F_Yh",
                     "L1011.ff_GrossTrade_EJ_R_C_Y",
                     "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
                     "L121.in_EJ_R_TPES_crude_Yh_EUR",
                     "L111.Prod_EJ_R_F_Yh_EUR",
                     "L1011.ff_trade_Europe_EJ_R_Y",
                     "Europe_Single_Market_Regions")
  MODULE_OUTPUTS <- c("L2011.ff_ALL_EJ_R_C_Y_EUR",
                      "L2011.ff_GrossTrade_EJ_R_C_Y_EUR",
                      "L2011.ff_GrossTrade_EJ_R_C_Y_singleMarket")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- GCAM_commodity <- region <-  fuel <- year <-
      consumption <- calOutputValue <- . <- sector <- subsector <- production <-
      cal.production <- net_trade <- GrossExp_EJ <- GrossImp_EJ <- technology <-
      GCAM_net_trade <- GrossExp_EJ_old <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    SINGLE_MARKET_NAME <- unique(L1011.ff_trade_Europe_EJ_R_Y$region_importer[L1011.ff_trade_Europe_EJ_R_Y$region_importer != "Global"])

    # Part 0: Replace IEA data with Eurostat data wherever available ----------------------------
    L1012.en_bal_EJ_R_Si_Fi_Yh <- L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      anti_join(L1012.en_bal_EJ_R_Si_Fi_Yh_EUR, by = c("GCAM_region_ID")) %>%
      bind_rows(L1012.en_bal_EJ_R_Si_Fi_Yh_EUR)
    L121.in_EJ_R_TPES_crude_Yh <- L121.in_EJ_R_TPES_crude_Yh %>%
      anti_join(L121.in_EJ_R_TPES_crude_Yh_EUR, by = c("GCAM_region_ID")) %>%
      bind_rows(L121.in_EJ_R_TPES_crude_Yh_EUR)
    L111.Prod_EJ_R_F_Yh <- L111.Prod_EJ_R_F_Yh %>%
      anti_join(L111.Prod_EJ_R_F_Yh_EUR, by = c("GCAM_region_ID")) %>%
      bind_rows(L111.Prod_EJ_R_F_Yh_EUR) %>%
      mutate(fuel = if_else(fuel == "gas","natural gas", fuel),
             technology = if_else(technology == "gas","natural gas", technology),
             fuel = if_else(fuel == "refined liquids","crude oil", fuel),
             technology = if_else(technology == "refined liquids","crude oil", technology))

    #There is no single file in GCAM that calculates net trade of fossil fuels. To build regional
    # markets for fossil fuels (gas, oil, coal) we need to maintain GCAM's calibrations, so we
    # calculate GCAM's implied net-trade to maintain.

    #This treats natural gas and LNG as one fuel type, but that should be changed in the future.
    # Total production is taken from L111.Prod_EJ_R_F_Yh and total consumption is calculated from
    # L1012.en_bal_EJ_R_Si_Fi_Yh and L121.in_EJ_R_TPES_crude_Yh/unoil.

    # Part 1: Calculate total consumption of fuels by region ----------------------------
    bind_rows(L1012.en_bal_EJ_R_Si_Fi_Yh,
              L121.in_EJ_R_TPES_crude_Yh,
              L121.in_EJ_R_TPES_unoil_Yh) %>%
      filter(sector == "TPES",
             year %in% HISTORICAL_YEARS,
             fuel %in% c("gas", "coal", "crude oil", "unconventional oil")) %>%
      #Unconventional oil is crude oil
      mutate(fuel = if_else(fuel == "gas", "natural gas", if_else(fuel == "unconventional oil","crude oil",fuel))) %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      select(GCAM_region_ID, fuel, year, consumption = value) ->
      ff_consumption

    # Part 2: Gather total production of fossil fuels ----------------------------
    L111.Prod_EJ_R_F_Yh %>%
      select(GCAM_region_ID, fuel, year, production = value, technology) %>%
      mutate(fuel= if_else(technology=="unconventional oil","crude oil",fuel)) %>%
      complete(fuel = unique(ff_consumption$fuel),
               GCAM_region_ID = unique(GCAM_region_names$GCAM_region_ID),
               year = ff_consumption$year,
               fill = list(production = 0)) %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      mutate(production= sum(production)) %>%
      ungroup() %>%
      select(GCAM_region_ID, fuel, year, production) %>%
      distinct()->
      ff_production

    # Part 3: Calculate net-trade by subtracting consumption from production by region and year ----------------------------
    ff_production %>%
      left_join_error_no_match(ff_consumption, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(net_trade = production - consumption) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, fuel, year, production, consumption, net_trade) ->
      L2011.ff_ALL_EJ_R_C_Y_EUR

    # Part 4a: Prepare eurostat data for global market ----------------------------
    # Assign the single market a regionID of 0
    GCAM_region_names <- GCAM_region_names %>%
      bind_rows(tibble(GCAM_region_ID = 0, region = SINGLE_MARKET_NAME))

    # We need to create the European_Single_Market and add in EUrostat trade data
    L2011.ff_ALL_EJ_R_C_Y_EUR_SingleMarket <- L2011.ff_ALL_EJ_R_C_Y_EUR %>%
      mutate(region = if_else(region %in% Europe_Single_Market_Regions$GCAMEU_region, SINGLE_MARKET_NAME, region)) %>%
      group_by(region, fuel, year) %>%
      summarise(production = sum(production),
                consumption = sum(consumption),
                net_trade = sum(net_trade)) %>%
      ungroup

    # We want to calculate trade between european market and globe
    # Since we only have Swiss data reported by other regions, we are going to subtract
    # reported EU trade from eurostat from the gross exports and imports reported in comstat
    SWISS_ID <- GCAM_region_names %>% filter(region == "Switzerland") %>%  pull(GCAM_region_ID)

    # Swiss trade with other single market countries (eurostat)
    intraEU_Swiss_trade <- L1011.ff_trade_Europe_EJ_R_Y %>%
      filter(fuel %in% L1011.ff_GrossTrade_EJ_R_C_Y$GCAM_Commodity,
             import_ctry == "Switzerland" | export_ctry == "Switzerland",
             region_importer == SINGLE_MARKET_NAME & region_exporter == SINGLE_MARKET_NAME) %>%
      mutate(flow = if_else(import_ctry == "Switzerland", "GrossImp_EJ", "GrossExp_EJ")) %>%
      group_by(GCAM_Commodity = fuel, year, flow) %>%
      summarise(value_intraEU = sum(value)) %>%
      ungroup

    # total swiss trade from comstat
    extraEU_Swiss_trade <- L1011.ff_GrossTrade_EJ_R_C_Y %>%
      filter(GCAM_region_ID == SWISS_ID) %>%
      select(-net_trade) %>%
      tidyr::pivot_longer(cols = c("GrossExp_EJ", "GrossImp_EJ"), names_to = "flow") %>%
      # match in eurostat data
      left_join(intraEU_Swiss_trade, by = c("GCAM_Commodity", "year", "flow")) %>%
      mutate(value = if_else(is.na(value_intraEU), value, value - value_intraEU)) %>%
      select(fuel = GCAM_Commodity, year, flow, value)

    # The european trade in Comtrade includes intra-european trade
    # So we need to replace it with the external trade from the eurostat data
    L1011.EUR_SingleMarket_Global_Trade <- L1011.ff_trade_Europe_EJ_R_Y %>%
      filter(fuel %in% L1011.ff_GrossTrade_EJ_R_C_Y$GCAM_Commodity,
             region_importer != region_exporter,
             !(import_ctry == "Switzerland" | export_ctry == "Switzerland")) %>%
      mutate(flow = if_else(region_importer == SINGLE_MARKET_NAME, "GrossImp_EJ", "GrossExp_EJ")) %>%
      bind_rows(extraEU_Swiss_trade) %>%
      group_by(GCAM_Commodity = fuel, year, flow) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      tidyr::pivot_wider(names_from = flow, values_from = value) %>%
      mutate(net_trade = GrossExp_EJ - GrossImp_EJ,
             region = SINGLE_MARKET_NAME) %>%
      left_join_error_no_match(GCAM_region_names, by = "region")

    # Part 4b: Adjust Comtrade and Eurostat trade to match GCAM's calibrated data (GLOBAL trade)----------------------------
    L1011.ff_GrossTrade_EJ_R_C_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # replace european data
      filter(!region %in% Europe_Single_Market_Regions$GCAMEU_region) %>%
      bind_rows(L1011.EUR_SingleMarket_Global_Trade) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      complete(GCAM_Commodity = unique(L2011.ff_ALL_EJ_R_C_Y_EUR_SingleMarket$fuel),
               nesting(GCAM_region_ID, region, year)) %>%
      left_join_error_no_match(L2011.ff_ALL_EJ_R_C_Y_EUR_SingleMarket %>% select(region, fuel, year, GCAM_net_trade = net_trade),
                               by = c("region", "GCAM_Commodity" = "fuel", "year")) %>%
      mutate(net_trade = if_else(is.na(net_trade), GCAM_net_trade, net_trade),
             GrossExp_EJ = if_else(is.na(GrossExp_EJ), if_else(GCAM_net_trade>0, GCAM_net_trade, 0), GrossExp_EJ ),
             GrossImp_EJ = if_else(is.na(GrossImp_EJ), if_else(GCAM_net_trade<=0, -1*GCAM_net_trade, 0), GrossImp_EJ )) %>%
      #We will maintain GCAM's calibration values and harmonize net_trade by scaling Comtrade's imports and exports by the ratio between GCAM's net trade and comtrade's
      mutate(GrossExp_EJ = if_else(!is.na(GrossExp_EJ * GCAM_net_trade/net_trade) & !is.infinite(GrossExp_EJ * GCAM_net_trade/net_trade), GrossExp_EJ * GCAM_net_trade/net_trade, GrossExp_EJ),
             GrossImp_EJ = if_else(!is.na(GrossImp_EJ * GCAM_net_trade/net_trade) & !is.infinite(GrossImp_EJ * GCAM_net_trade/net_trade), GrossImp_EJ * GCAM_net_trade/net_trade, GrossImp_EJ),
             net_trade = GrossExp_EJ - GrossImp_EJ) %>%
      #There are a few rows where Comtrade says a region is an importer and GCAM says they're an exporter
      #This discrepency makes the gross imports and exports negative which cannot happen.
      #We turn the values positive and flip imports and exports to maintain GCAM's values
      mutate(GrossExp_EJ_old = GrossExp_EJ,
             GrossExp_EJ = if_else(GrossExp_EJ<0, -1*GrossImp_EJ, GrossExp_EJ),
             GrossImp_EJ = if_else(GrossImp_EJ<0, -1*GrossExp_EJ_old, GrossImp_EJ),
             net_trade = GrossExp_EJ - GrossImp_EJ) %>%
      select(names(L1011.ff_GrossTrade_EJ_R_C_Y)) ->
      L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year

    # This structure does not allow regions to trade more product than they produce, so decrease
    # Exports and Imports for any region where GrossExp is greater than production
    L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(L2011.ff_ALL_EJ_R_C_Y_EUR_SingleMarket %>% select(region, fuel, year, production),
                               by = c("region", "GCAM_Commodity" = "fuel", "year")) %>%
      mutate(GrossImp_EJ = if_else(GrossExp_EJ>production, GrossImp_EJ - (GrossExp_EJ-production), if_else(GrossExp_EJ==production, GrossImp_EJ - (GrossExp_EJ-0.95*production),GrossImp_EJ)),
             GrossExp_EJ = if_else(GrossExp_EJ>production, production, if_else(GrossExp_EJ==production, 0.95*production,GrossExp_EJ))) %>%
      distinct() %>%
      mutate(GrossImp_EJ = if_else(GrossExp_EJ==production, GrossImp_EJ - (GrossExp_EJ-0.95*production),GrossImp_EJ),
             GrossExp_EJ = if_else(GrossExp_EJ==production, 0.95*production,GrossExp_EJ)) %>%
      select(names(L1011.ff_GrossTrade_EJ_R_C_Y)) ->
      L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year_adj

    #Only the final calibration period's calibration matters, so for earlier periods simply assume that
    # each region is solely an importer or an exporter.
    L2011.ff_ALL_EJ_R_C_Y_EUR_SingleMarket %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      filter(! year %in% L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year_adj$year) %>%
      mutate(GrossExp_EJ = if_else(net_trade<=0, 0, net_trade),
             GrossImp_EJ = if_else(net_trade<0, -1*net_trade, 0),
             GCAM_Commodity = fuel) %>%
      select(names(L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year_adj)) %>%
      bind_rows(L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year_adj)->
      L2011.ff_GrossTrade_EJ_R_C_Y_EUR

    # Part 4c: European Single Market Trade ----------------------------
    # Get gross exports, imports and net trade for each country in single market
    # Want all trade (both within market and outside) since we need to match net trade of single market region
    L2011.ff_trade_EJ_R_Y_singleMarket <- L1011.ff_trade_Europe_EJ_R_Y %>%
      filter(fuel %in% L1011.ff_GrossTrade_EJ_R_C_Y$GCAM_Commodity) %>%
      select(-region_importer, -region_exporter) %>%
      mutate(ID = row_number()) %>%
      tidyr::pivot_longer(cols = c("import_ctry", "export_ctry"), names_to = "flow", values_to = "region") %>%
      group_by(GCAM_Commodity = fuel, region, flow, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      filter(region %in% Europe_Single_Market_Regions$GCAMEU_region) %>%
      complete(nesting(GCAM_Commodity, flow, year), region, fill = list(value = 0)) %>%
      mutate(flow = if_else(flow == "import_ctry", "GrossImp_EJ", "GrossExp_EJ")) %>%
      tidyr::pivot_wider(names_from = flow, values_fill = 0) %>%
      mutate(net_trade = GrossExp_EJ - GrossImp_EJ) %>%
      # need to use comstat for switzerland
      filter(region != "Switzerland") %>%
      bind_rows(L1011.ff_GrossTrade_EJ_R_C_Y %>%  filter(GCAM_region_ID == SWISS_ID) %>%
                  select(-GCAM_region_ID) %>% mutate(region = "Switzerland"))

    # Adjust eurostat net trade based on GCAM implied net trade
    L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year_singleMarket <-  L2011.ff_trade_EJ_R_Y_singleMarket %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      complete(GCAM_Commodity = unique(L2011.ff_ALL_EJ_R_C_Y_EUR_SingleMarket$fuel),
               nesting(region, year)) %>%
      left_join_error_no_match(L2011.ff_ALL_EJ_R_C_Y_EUR %>% select(region, fuel, year, GCAM_net_trade = net_trade),
                               by = c("region", "GCAM_Commodity" = "fuel", "year")) %>%
      mutate(net_trade = if_else(is.na(net_trade), GCAM_net_trade, net_trade),
             GrossExp_EJ = if_else(is.na(GrossExp_EJ), if_else(GCAM_net_trade > 0, GCAM_net_trade, 0), GrossExp_EJ ),
             GrossImp_EJ = if_else(is.na(GrossImp_EJ), if_else(GCAM_net_trade <= 0, -1 * GCAM_net_trade, 0), GrossImp_EJ )) %>%
      #We will maintain GCAM's calibration values and harmonize net_trade by scaling eurostat's imports and exports by the ratio between GCAM's net trade and comtrade's
      mutate(GrossExp_EJ = if_else(!is.na(GrossExp_EJ * GCAM_net_trade/net_trade) & !is.infinite(GrossExp_EJ * GCAM_net_trade/net_trade), GrossExp_EJ * GCAM_net_trade/net_trade, GrossExp_EJ),
             GrossImp_EJ = if_else(!is.na(GrossImp_EJ * GCAM_net_trade/net_trade) & !is.infinite(GrossImp_EJ * GCAM_net_trade/net_trade), GrossImp_EJ * GCAM_net_trade/net_trade, GrossImp_EJ),
             net_trade = GrossExp_EJ - GrossImp_EJ) %>%
      #There are a few rows where eurostat says a region is an importer and GCAM says they're an exporter
      #This discrepency makes the gross imports and exports negative which cannot happen.
      #We turn the values positive and flip imports and exports to maintain GCAM's values
      mutate(GrossExp_EJ_old = GrossExp_EJ,
             GrossExp_EJ = if_else(GrossExp_EJ < 0, -1 * GrossImp_EJ, GrossExp_EJ),
             GrossImp_EJ = if_else(GrossImp_EJ < 0, -1 * GrossExp_EJ_old, GrossImp_EJ),
             net_trade = GrossExp_EJ - GrossImp_EJ) %>%
      # sometimes eurostat has no trade, but GCAM has a bit, so we add the GCAM trade to imports/exports
      mutate(GrossExp_EJ = if_else(net_trade == 0 & GCAM_net_trade > 0, GCAM_net_trade, GrossExp_EJ),
             GrossImp_EJ = if_else(net_trade == 0 & GCAM_net_trade < 0, -1 * GCAM_net_trade, GrossImp_EJ),
             net_trade = GrossExp_EJ - GrossImp_EJ) %>%
      select(names(L2011.ff_trade_EJ_R_Y_singleMarket))

    # This structure does not allow regions to trade more product than they produce, so decrease
    # Exports and Imports for any region where GrossExp is greater than production
    L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year_adj_singleMarket <- L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year_singleMarket %>%
      left_join_error_no_match(L2011.ff_ALL_EJ_R_C_Y_EUR %>% select(region, fuel, year, production),
                               by = c("region", "GCAM_Commodity" = "fuel", "year")) %>%
      mutate(GrossImp_EJ = if_else(GrossExp_EJ>production, GrossImp_EJ - (GrossExp_EJ-production), if_else(GrossExp_EJ==production, GrossImp_EJ - (GrossExp_EJ-0.95*production),GrossImp_EJ)),
             GrossExp_EJ = if_else(GrossExp_EJ>production, production, if_else(GrossExp_EJ==production, 0.95*production,GrossExp_EJ))) %>%
      mutate(GrossImp_EJ = if_else(GrossExp_EJ==production, GrossImp_EJ - (GrossExp_EJ-0.95*production),GrossImp_EJ),
             GrossExp_EJ = if_else(GrossExp_EJ==production, 0.95*production,GrossExp_EJ)) %>%
      select(names(L2011.ff_trade_EJ_R_Y_singleMarket))

    #Only the final calibration period's calibration matters, so for earlier periods simply assume that
    # each region is solely an importer or an exporter.
    L2011.ff_GrossTrade_EJ_R_C_Y_singleMarket <- L2011.ff_ALL_EJ_R_C_Y_EUR %>%
      filter(region %in% Europe_Single_Market_Regions$GCAMEU_region,
             ! year %in% L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year_adj_singleMarket$year) %>%
      mutate(GrossExp_EJ = if_else(net_trade <= 0, 0, net_trade),
             GrossImp_EJ = if_else(net_trade < 0, -1 * net_trade, 0),
             GCAM_Commodity = fuel) %>%
      select(names(L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year_adj_singleMarket)) %>%
      bind_rows(L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year_adj_singleMarket)


    # Produce outputs ----------------------------
    L2011.ff_ALL_EJ_R_C_Y_EUR %>%
      add_title("L2011.ff_ALL_EJ_R_C_Y_EUR") %>%
      add_units("EJ") %>%
      add_comments("Calculate fossil fuel net trade by GCAM region, commodity and year") %>%
      add_precursors("common/GCAM_region_names",
                     "L1012.en_bal_EJ_R_Si_Fi_Yh",
                     "L121.in_EJ_R_TPES_crude_Yh",
                     "L121.in_EJ_R_TPES_unoil_Yh",
                     "L111.Prod_EJ_R_F_Yh",
                     "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
                     "L121.in_EJ_R_TPES_crude_Yh_EUR",
                     "L111.Prod_EJ_R_F_Yh_EUR") ->
      L2011.ff_ALL_EJ_R_C_Y_EUR

    L2011.ff_GrossTrade_EJ_R_C_Y_EUR %>%
      add_title("Global Fossil Trade - with Europe as one market") %>%
      add_units("EJ") %>%
      add_comments("Adjust Comtrade fossil fuel net trade to match GCAM's calibrated values by GCAM region, commodity and year") %>%
      add_precursors(MODULE_INPUTS) ->
      L2011.ff_GrossTrade_EJ_R_C_Y_EUR

    L2011.ff_GrossTrade_EJ_R_C_Y_singleMarket  %>%
      add_title("European Single Market Trade") %>%
      add_units("EJ") %>%
      add_precursors(MODULE_INPUTS) ->
      L2011.ff_GrossTrade_EJ_R_C_Y_singleMarket


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
