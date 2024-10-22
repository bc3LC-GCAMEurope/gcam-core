# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L239.ff_trade
#'
#' Model input for regionally and globally traded fossil fuel commodities
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L239.PrimaryConsKeyword_en_EUR}, \code{L239.Supplysector_tra_EUR},
#'   \code{L239.SectorUseTrialMarket_tra_EUR}, \code{L239.SubsectorAll_tra_EUR}, \code{L239.TechShrwt_tra_EUR},
#'   \code{L239.TechCost_tra_EUR}, \code{L239.TechCoef_tra_EUR}, \code{L239.Production_tra_EUR}, \code{L239.Supplysector_reg_EUR},
#'   \code{L239.SubsectorAll_reg_EUR}, \code{L239.TechShrwt_reg_EUR}, \code{L239.TechCoef_reg_EUR}, \code{L239.Production_reg_imp_EUR},
#'   \code{L239.Production_reg_dom_EUR}, \code{L239.Consumption_intraregional_EUR}, \code{L239.CarbonCoef_EUR}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else left_join mutate rename select
#' @importFrom tibble tibble
#' @author RH February 2024
module_gcameurope_L239.ff_trade <- function(command, ...) {
  OUTPUTS_TO_COPY_FILTER <- c( "L239.PrimaryConsKeyword_en",
                               "L239.Supplysector_tra",
                               "L239.SectorUseTrialMarket_tra",
                               "L239.SubsectorAll_tra", # add in global imports
                               "L239.TechShrwt_tra", # add in global imports
                               "L239.TechCost_tra", # add in global imports
                               "L239.TechCoef_tra", # add in global imports
                               "L239.Supplysector_reg",
                               "L239.SubsectorAll_reg",
                               "L239.TechShrwt_reg",
                               "L239.TechCoef_reg",
                               "L239.Consumption_intraregional",
                               "L239.CarbonCoef")
  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "energy/A_ff_RegionalTechnology",
      FILE = "energy/A_ff_TradedTechnology",
      FILE = "gcam-europe/A_ff_RegionalTechnology_EUR",
      "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
      "L2011.ff_GrossTrade_EJ_R_C_Y_EUR",
      "L2011.ff_ALL_EJ_R_C_Y_EUR",
      "L239.Production_tra",
      "L2011.ff_GrossTrade_EJ_R_C_Y_singleMarket",
      "Europe_Single_Market_Regions",
      OUTPUTS_TO_COPY_FILTER)

  MODULE_OUTPUTS <-
    c("L239.Production_tra_EUR",
      "L239.Production_reg_imp_EUR",
      "L239.Production_reg_dom_EUR",
      "L239.Pop_europeSingleMarket",
      "L239.GDP_europeSingleMarket",
      paste0(OUTPUTS_TO_COPY_FILTER, "_EUR"))

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- region <- supplysector <- subsector <- GCAM_Commodity <- GrossExp_EJ <-
      calOutputValue <- subs.share.weight <- market.name <- minicam.energy.input <-
      GrossImp_EJ <- Prod_EJ <- fuel <- technology <- primary.consumption <- PrimaryFuelCO2Coef.name <- PrimaryFuelCO2Coef <-
      production <- consumption <- GCAM_region_ID <- NULL # silence package check notes

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS)

    # Create outputs that are simply copied form main scripts and filtered to Eurostat
    # Only for non-traded outputs
    copy_filter_europe(all_data, OUTPUTS_TO_COPY_FILTER[!grepl("_tra", OUTPUTS_TO_COPY_FILTER)],
                       regions_to_keep = c(gcameurope.EUROSTAT_COUNTRIES, "Switzerland"))

    # Single market names and adjustments
    SINGLE_MARKET_NAME <- unique(A_ff_RegionalTechnology_EUR$market.name[A_ff_RegionalTechnology_EUR$market.name != "regional"])

    GCAM_region_names <- GCAM_region_names %>% dplyr::add_row(GCAM_region_ID = 0, region = SINGLE_MARKET_NAME)

    EURO_TRADE_REGIONS <- c(gcameurope.EUROSTAT_COUNTRIES, SINGLE_MARKET_NAME, "Switzerland")

    # Combine the single market and global market output
    L239.ff_GrossTrade_EJ_R_C_Y_EUR <- L2011.ff_GrossTrade_EJ_R_C_Y_EUR %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID) %>%
      bind_rows(L2011.ff_GrossTrade_EJ_R_C_Y_singleMarket)

    # need to add in Euro single market to trade
    L239.Supplysector_tra_EUR <- L239.Supplysector_tra %>%
      mutate(region = SINGLE_MARKET_NAME) %>%
      bind_rows(L239.Supplysector_tra)
    L239.SectorUseTrialMarket_tra_EUR <- L239.SectorUseTrialMarket_tra %>%
      mutate(region = SINGLE_MARKET_NAME) %>%
      bind_rows(L239.SectorUseTrialMarket_tra)
    L239.CarbonCoef_EUR <- L239.CarbonCoef %>%
      mutate(region = SINGLE_MARKET_NAME) %>%
      bind_rows(L239.CarbonCoef)

    # Adjust trade for regions with production in Eurostat, but not IEA ----------------------------
    # Some regions may have had no trade in IEA but have trade in Eurostat
    missing_exports_R_C <- L2011.ff_GrossTrade_EJ_R_C_Y_EUR %>%
      filter(GCAM_region_ID > 0) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      bind_rows(L2011.ff_GrossTrade_EJ_R_C_Y_singleMarket) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      group_by(region, GCAM_Commodity) %>%
      filter(any(GrossExp_EJ > 0)) %>%
      ungroup %>%
      mutate(supplysector = paste("traded", GCAM_Commodity),
             supplysector = gsub("crude oil", "oil", supplysector),
             subsector = paste(region, supplysector),
             market.name = region) %>%
      distinct(supplysector, subsector, market.name) %>%
      anti_join(L239.SubsectorAll_tra, by = c("supplysector", "subsector"))

    # Plus add in new european single market region
    EUR_market <- L239.Supplysector_tra_EUR %>%
      filter(!region %in% unique(A_ff_RegionalTechnology$market.name)) %>%
      distinct(market.name = region, supplysector) %>%
      mutate(subsector = paste(market.name, supplysector))

    # First add in missing markets
    # Then adjust region to create European single market
    add_single_market <- function(df){
      df_singleMarket <- df %>%
        mutate(region_tmp = stringr::str_extract(subsector, ".*(?= traded)"),
               region = if_else(region_tmp %in% Europe_Single_Market_Regions$GCAMEU_region, unique(EUR_market$market.name), region))
      # now copy one region and rename it to global tech
      df_subsector <- df_singleMarket %>%
        filter(region == SINGLE_MARKET_NAME, region_tmp == dplyr::first(region_tmp)) %>%
        mutate(subsector = stringr::str_replace(subsector, region_tmp, "global")) %>%
        bind_rows(df_singleMarket)

      if ("technology" %in% names(df)){
        df_subsector %>%
          mutate(technology = if_else(grepl("global", subsector), subsector, technology))  %>% select(-region_tmp)
      } else { df_subsector %>% select(-region_tmp) }
    }

    L239.SubsectorAll_tra_EUR <- L239.SubsectorAll_tra %>%
      distinct(region, logit.year.fillout, logit.exponent, year.fillout,
               share.weight, apply.to, from.year, to.year, to.value, interpolation.function, logit.type) %>%
      repeat_add_columns(bind_rows(missing_exports_R_C, EUR_market) %>%  select(-market.name)) %>%
      bind_rows(L239.SubsectorAll_tra) %>%
      add_single_market

    # function to add missing export regions and add single market
    tech_tra_adjust <- function(df, cols){
      # we are just going to repeat the USA traded tech for the EU single market
      EUR_market_add <- EUR_market %>%
        mutate(technology = subsector) %>%
        repeat_add_columns(df %>% filter(region == stringr::str_extract(subsector, ".*(?= traded)")) %>%
                             select(all_of(c("region", "year", cols))) %>% distinct)

      df %>%
        distinct(across(all_of(c("region", "supplysector", "year", cols)))) %>%
        right_join(missing_exports_R_C, by = "supplysector") %>%
        mutate(technology = subsector) %>%
        bind_rows(df, EUR_market_add) %>%
        add_single_market %>%
        select(names(df))
    }

    L239.TechShrwt_tra_EUR <- tech_tra_adjust(L239.TechShrwt_tra, cols = "share.weight")

    L239.TechCost_tra_EUR <- tech_tra_adjust(L239.TechCost_tra, cols = c("minicam.non.energy.input", "input.cost"))

    L239.TechCoef_tra_EUR  <- tech_tra_adjust(L239.TechCoef_tra, cols = c("minicam.energy.input", "coefficient")) %>%
      mutate(minicam.energy.input = if_else(market.name == SINGLE_MARKET_NAME, supplysector, minicam.energy.input),
             market.name = if_else(grepl("global", subsector), gcam.USA_REGION, market.name),
             minicam.energy.input = if_else(grepl("global", subsector), supplysector, minicam.energy.input))

    # Base technology-level table for several tables to be written out" ------------
    A_ff_TradedTechnology_R_Y <- repeat_add_columns(A_ff_TradedTechnology,
                                                    tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names %>% filter_regions_europe(EURO_TRADE_REGIONS)) %>%
      mutate(subsector = paste(region, subsector, sep = " "),
             technology = subsector,
             market.name = region,
             region = if_else(region %in% Europe_Single_Market_Regions$GCAMEU_region, SINGLE_MARKET_NAME, gcam.USA_REGION)) %>%
      # Need to remove exports from regions with no fossil resources
      semi_join(L239.SubsectorAll_tra_EUR, by = c("supplysector", "subsector"))

    A_ff_regionalTechnology_R_Y <-  repeat_add_columns(A_ff_RegionalTechnology,
                                                       tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(tibble(region = c(gcameurope.EUROSTAT_COUNTRIES, "Switzerland"))) %>%
      mutate(market.name = if_else(market.name == "regional", region, if_else(region %in% Europe_Single_Market_Regions$GCAMEU_region, SINGLE_MARKET_NAME, gcam.USA_REGION)))

    # L239.Production_tra_EUR: Output (gross exports) of traded technologies -------------
    L239.GrossExports_EJ_R_C_Y <- L239.ff_GrossTrade_EJ_R_C_Y_EUR %>%
      select(region, GCAM_Commodity, year, GrossExp_EJ)

    # Negative net trade to single market is imports from USA global pool
    # except in 2015, needs to be the correct value
    singleMarket_globalImports <- L239.ff_GrossTrade_EJ_R_C_Y_EUR %>%
      filter(region == SINGLE_MARKET_NAME, net_trade < 0,
             year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(distinct(A_ff_TradedTechnology_R_Y, supplysector, minicam.energy.input),
                               by = c("GCAM_Commodity" = "minicam.energy.input")) %>%
      mutate(market.name = gcam.USA_REGION,
             subsector = paste0("global ", supplysector),
             technology = subsector,
             calOutputValue = if_else(year == MODEL_FINAL_BASE_YEAR, GrossImp_EJ,-net_trade),
             calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    L239.Production_tra_EUR <- filter(A_ff_TradedTechnology_R_Y, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L239.GrossExports_EJ_R_C_Y,
                               by = c(market.name = "region", minicam.energy.input = "GCAM_Commodity", "year")) %>%
      filter(market.name %in% EURO_TRADE_REGIONS) %>%
      rename(calOutputValue = GrossExp_EJ) %>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) %>%
      # add in non-europe regions
      bind_rows(L239.Production_tra %>% filter(!grepl(paste(EURO_TRADE_REGIONS, collapse = "|"), subsector)),
                singleMarket_globalImports)

    # L239.Production_reg_imp_EUR: Output (flow) of gross imports -----------------
    L239.GrossImports_EJ_R_C_Y <- L239.ff_GrossTrade_EJ_R_C_Y_EUR %>%
      left_join(select(A_ff_TradedTechnology, supplysector, minicam.energy.input),
                by = c(GCAM_Commodity = "minicam.energy.input")) %>%
      select(region, supplysector, year, GrossImp_EJ)

    L239.Production_reg_imp_EUR <- A_ff_regionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "import", subsector)) %>%
      left_join_error_no_match(L239.GrossImports_EJ_R_C_Y %>% filter(region != SINGLE_MARKET_NAME),
                               by = c("region", minicam.energy.input = "supplysector", "year")) %>%
      filter(region %in% EURO_TRADE_REGIONS) %>%
      rename(calOutputValue = GrossImp_EJ) %>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # L239.Production_reg_dom_EUR: Output (flow) of domestic -----------------
    # Domestic "output" is equal to production (production in L2011.ff_ALL_EJ_R_C_Y_EUR) minus gross exports (calculated in L2011.ff_GrossTrade_EJ_R_C_Y_EUR)
    ### DOMESTIC TECHNOLOGY OUTPUT = AG PRODUCTION - GROSS EXPORTS
    L239.Prod_EJ_R_C_Y <- select(L2011.ff_ALL_EJ_R_C_Y_EUR, region, GCAM_Commodity = fuel, year, Prod_EJ = production)

    L239.Production_reg_dom_EUR <- A_ff_regionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "domestic", subsector)) %>%
      left_join_error_no_match(L239.GrossExports_EJ_R_C_Y,
                               by = c("region", minicam.energy.input = "GCAM_Commodity", "year")) %>%
      left_join_error_no_match(L239.Prod_EJ_R_C_Y,
                               by = c("region", minicam.energy.input = "GCAM_Commodity", "year")) %>%
      filter_regions_europe(EURO_TRADE_REGIONS) %>%
      mutate(calOutputValue = round(Prod_EJ - GrossExp_EJ, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # Adjust domestic techs for regions with production in Eurostat, but not IEA --------------------
    # regions with zero production in all historical years should be removed from all tables
    zero_domestic_consume <- L239.Production_reg_dom_EUR %>%
      group_by(region, supplysector, subsector, technology) %>%
      filter(all(calOutputValue <= 0)) %>%
      ungroup %>%
      distinct(region, supplysector, subsector, technology)

    # regions with production in Eurostat but not IEA should be added in to tables
    missing_domestic_consume <- L239.Production_reg_dom_EUR  %>%
      anti_join(L239.SubsectorAll_reg_EUR, by = c("region", "supplysector", "subsector")) %>%
      distinct(region, supplysector, subsector, technology)

    L239.Production_reg_dom_EUR <- L239.Production_reg_dom_EUR  %>%  anti_join(zero_domestic_consume, by = c("supplysector", "subsector", "technology", "region"))

    L239.PrimaryConsKeyword_en_EUR <-  L239.PrimaryConsKeyword_en_EUR %>%
      distinct(supplysector, subsector, technology, primary.consumption, year) %>%
      right_join(missing_domestic_consume, by = c("supplysector", "subsector", "technology")) %>%
      bind_rows(L239.PrimaryConsKeyword_en_EUR) %>%
      anti_join(zero_domestic_consume, by = c("supplysector", "subsector", "technology", "region"))

    L239.TechShrwt_reg_EUR <- L239.TechShrwt_reg_EUR %>%
      distinct(supplysector, subsector, technology, year, share.weight) %>%
      right_join(missing_domestic_consume, by = c("supplysector", "subsector", "technology")) %>%
      bind_rows(L239.TechShrwt_reg_EUR) %>%
      anti_join(zero_domestic_consume, by = c("supplysector", "subsector", "technology", "region"))

     L239.TechCoef_reg_EUR <- L239.TechCoef_reg %>%
       # need to keep market name  == USA
       filter(region %in% EURO_TRADE_REGIONS) %>%
       distinct(supplysector, subsector, technology, year, minicam.energy.input, coefficient) %>%
       right_join(missing_domestic_consume, by = c("supplysector", "subsector", "technology")) %>%
       mutate(market.name = region) %>%
       bind_rows(L239.TechCoef_reg %>% filter(region %in% EURO_TRADE_REGIONS)) %>%
       anti_join(zero_domestic_consume, by = c("supplysector", "subsector", "technology", "region")) %>%
       mutate(market.name = if_else(grepl("imported", subsector) & region %in% Europe_Single_Market_Regions$GCAMEU_region, SINGLE_MARKET_NAME, market.name))

     L239.SubsectorAll_reg_EUR <- L239.SubsectorAll_reg_EUR %>%
       select(-region) %>%
       distinct() %>%
       right_join(missing_domestic_consume %>% distinct(region, supplysector, subsector), by = c("supplysector", "subsector")) %>%
       bind_rows(L239.SubsectorAll_reg_EUR) %>%
       anti_join(zero_domestic_consume, by = c("supplysector", "subsector", "region"))

    # Pop and gdp fill in for euro single market ------------------
     L239.Pop_europeSingleMarket <- tibble(region = SINGLE_MARKET_NAME, year = MODEL_YEARS, totalPop = 1)
     L239.GDP_europeSingleMarket <- tibble(region = SINGLE_MARKET_NAME, year = MODEL_YEARS, GDP = 1)


    # Produce outputs ----
    L239.Production_tra_EUR %>%
      add_title("Technology calibration for traded ff commodities") %>%
      add_units("EJ") %>%
      add_comments("Regional exports of commodities that are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_tradedTechnology",
                     "L2011.ff_GrossTrade_EJ_R_C_Y_EUR",
                     "L239.Production_tra") ->
      L239.Production_tra_EUR

    L239.Production_reg_imp_EUR %>%
      add_title("Technology calibration for regional ff commodities: imports") %>%
      add_units("EJ") %>%
      add_comments("Consumption of commodities that are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_regionalTechnology",
                     "L2011.ff_GrossTrade_EJ_R_C_Y_EUR") ->
      L239.Production_reg_imp_EUR

    L239.Production_reg_dom_EUR %>%
      add_title("Technology calibration for regional ff commodities: consumption of domestic production") %>%
      add_units("EJ") %>%
      add_comments("Consumption of commodities produced within-region") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_regionalTechnology",
                     "L2011.ff_ALL_EJ_R_C_Y_EUR",
                     "L2011.ff_GrossTrade_EJ_R_C_Y_EUR") ->
      L239.Production_reg_dom_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
