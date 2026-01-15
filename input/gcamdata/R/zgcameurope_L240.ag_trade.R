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
      "L240.SubsectorAll_reg",
      "L240.TechShrwt_reg",
      "L240.Production_reg_dom", # to do
      "L240.Production_reg_imp", # to do
      "L240.TechCoef_reg")

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "common/iso_GCAM_regID",
      FILE = "gcam-europe/A_agRegionalSector_EEA",
      FILE = "gcam-europe/A_agTradedSector_EEA",
      FILE = "gcam-europe/mappings/ag_regions",
      "EuroSingleMarket_BiTrade_Ag_crops",
      "EuroSingleMarket_BiTrade_Ag_noncrops",
      "L2012.AgSupplySector_EU",
      OUTPUTS_TO_ADJUST)

  MODULE_OUTPUTS <- c(paste(OUTPUTS_TO_ADJUST, "EUR", sep = "_"),
                      "L240.TechCost_reg")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    SINGLE_MARKET_NAME <- ag_regions %>% filter(region == gcameurope.TRADE_REGION) %>% pull(trade_region)
    crops <- A_agRegionalSector_EEA$supplysector
    TOTAL_CROPS <- c(crops[grepl(paste(aglu.TRADED_CROPS, collapse = "|"), crops, ignore.case = TRUE)],
                      "total nuts_seeds", "total root_tuber")
    TRADED_CROPS <- gsub("total", "traded", TOTAL_CROPS)
    TOTAL_NON_CROP <- setdiff(crops, TOTAL_CROPS)
    TRADED_NON_CROP <- gsub("total", "traded", TOTAL_NON_CROP)
    TRADED_NON_CROP <- gsub("regional", "traded", TRADED_NON_CROP)

    # 0: functions ----------
    copy_for_EUR <- function(df){
      if ("region" %in% names(df)){
        df %>%
          mutate(region = SINGLE_MARKET_NAME) %>%
          bind_rows(df)
      } else { df }
    }

    add_single_market <- function(df){
      EUR_regions <-  filter(ag_regions, trade_region == SINGLE_MARKET_NAME) %>% pull(region)
      df_singleMarket <- df %>%
        mutate(region_tmp = stringr::str_extract(subsector, ".*(?= traded)"),
               region = if_else(region_tmp %in% EUR_regions, SINGLE_MARKET_NAME, region))
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
        repeat_add_columns(distinct(ag_regions, trade_region)) %>%
        mutate(subsector = stringr::str_replace(subsector, region_tmp, trade_region)) %>%
        filter(!(trade_region != SINGLE_MARKET_NAME & !supplysector %in% TRADED_CROPS)) %>%
        select(-trade_region) %>%
        bind_rows(df_subsector_global,
                  df_singleMarket %>%
                    filter(!(supplysector %in% TRADED_CROPS &
                             region_tmp %in% (ag_regions %>% filter(trade_region != SINGLE_MARKET_NAME) %>% pull(region)))))

      if ("technology" %in% names(df)){
        df_subsector_EUR %>% mutate(technology = subsector)  %>% select(-region_tmp)
      } else { df_subsector_EUR %>% select(-region_tmp) }
    }

    single_euro_mkt <- function(df){
      df_noEU <- df %>%
        mutate(region_tmp = stringr::str_extract(subsector, ".*(?= traded)")) %>%
        filter(!(region_tmp %in% Europe_Single_Market_Regions$GCAMEU_region &
                   supplysector %in% TRADED_CROPS)) %>%
        select(-region_tmp)

      df_EU <- df_noEU %>%
        filter(region == SINGLE_MARKET_NAME & supplysector %in% TRADED_CROPS) %>%
        mutate(subsector = gsub("global", SINGLE_MARKET_NAME, subsector))

      if ("technology" %in% names(df_EU)){
        df_EU <- df_EU %>%
          mutate(technology = gsub("global", SINGLE_MARKET_NAME, technology))
      }

      bind_rows(df_noEU, df_EU)
    }

    filter_euro_mkt <- function(df){
      df %>%
        filter(!(region == SINGLE_MARKET_NAME & supplysector %in% TRADED_CROPS))
    }

    adj_animal <- function(df) {
      df %>%
        filter(
          !(
            grepl("global", subsector, ignore.case = TRUE) &
              region == SINGLE_MARKET_NAME &
              supplysector %in% TRADED_NON_CROP
          )
        ) %>%

        filter(
          !(
            grepl("European_Single_Market", subsector, ignore.case = TRUE) &
              region == gcam.USA_REGION &
              supplysector %in% TRADED_NON_CROP
          )
        )

    }
    #
    # 1a: Add euro market to supplysector region ----------------------
    L240.Supplysector_tra_EUR <- mutate(A_agTradedSector_EEA, logit.year.fillout = min(MODEL_BASE_YEARS),
           region = SINGLE_MARKET_NAME) %>%
      filter(!supplysector %in% TRADED_CROPS) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type")) %>%
      bind_rows(L240.Supplysector_tra)

    L240.Supplysector_reg_EUR <- mutate(A_agRegionalSector_EEA, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                           filter(GCAM_region_names, region %in% ag_regions$region)) %>%
      bind_rows(L240.Supplysector_reg %>% filter(!region %in% ag_regions$region)) %>%
      left_join(ag_regions, by = c("region")) %>%
      mutate(region = if_else(!is.na(trade_region) &
                                supplysector %in% TOTAL_CROPS,
                              trade_region, region)) %>%
      select(-ag_region, -trade_region) %>%
      distinct()

    L240.SectorUseTrialMarket_tra_EUR <- copy_for_EUR(L240.SectorUseTrialMarket_tra) %>%
      filter(!supplysector %in% TRADED_CROPS)

    # 1b: Add euro market to subsector ------------------------------------
    L240.SubsectorAll_tra_EUR <- add_single_market(L240.SubsectorAll_tra)  %>%
      filter_euro_mkt() %>%
      adj_animal()

    #
    # 1c: Add euro market to techs ----------------------
    L240.TechShrwt_tra_EUR <- add_single_market(L240.TechShrwt_tra) %>% filter_euro_mkt() %>% adj_animal()
    L240.TechCost_tra_EUR <- add_single_market(L240.TechCost_tra) %>% filter_euro_mkt() %>% adj_animal()
    L240.TechCoef_tra_EUR <- add_single_market(L240.TechCoef_tra)  %>% adj_animal() %>%
      filter_euro_mkt() %>%
      mutate(market.name = stringr::str_extract(subsector, ".*(?= traded)"),
             market.name = if_else(grepl("global", subsector), gcam.USA_REGION, market.name),
             minicam.energy.input = if_else((market.name == SINGLE_MARKET_NAME & !supplysector %in% TRADED_CROPS) | grepl("global", subsector),
                                            supplysector, minicam.energy.input))
    # Here only need to change market.name
    L240.TechCoef_reg_EUR_pre <- L240.TechCoef_reg %>%
      left_join(ag_regions, by = c("region")) %>%
      mutate(market.name = case_when(
        grepl("imported", subsector) & trade_region == SINGLE_MARKET_NAME ~ trade_region,
        TRUE ~ market.name)
        )


    L240.TechCoef_reg_EUR_noEU <- L240.TechCoef_reg_EUR_pre %>%
      filter(!(!is.na(ag_region) &
                 supplysector %in% TOTAL_CROPS))  %>%
      select(-trade_region, -ag_region)

    L240.TechCoef_reg_EUR_EU <- L240.TechCoef_reg_EUR_pre %>%
      filter(!is.na(ag_region) &
                 supplysector %in% TOTAL_CROPS)  %>%
      distinct(supplysector, subsector, technology, year, minicam.energy.input) %>%
      repeat_add_columns(distinct(ag_regions, region = trade_region)) %>%
      mutate(market.name = if_else(grepl("imported", subsector), "USA", region),
             coefficient = 1)

    L240.TechCoef_reg_EUR <- bind_rows(L240.TechCoef_reg_EUR_noEU, L240.TechCoef_reg_EUR_EU)

    # Adjust supplysector files with new animal structure:
    L240.SubsectorAll_tra_EUR <- L240.SubsectorAll_tra_EUR %>% adj_animal()

    L240.SubsectorAll_tra_EUR_tmp <- L240.SubsectorAll_tra_EUR %>%
      filter(region == SINGLE_MARKET_NAME & supplysector %in% TRADED_NON_CROP) %>%
      mutate(region = gcam.USA_REGION)

    L240.SubsectorAll_tra_EUR <- bind_rows(
      L240.SubsectorAll_tra_EUR,
      L240.SubsectorAll_tra_EUR_tmp
    )

    # Add new subsectors distinguishing between EEA and non-EEA trade
    Europe_regions <- ag_regions  %>% pull(region)

    L240.SubsectorAll_reg_tmp <- L240.SubsectorAll_reg %>%
      filter(region %in% Europe_regions, supplysector %in% TOTAL_NON_CROP, grepl("imported", subsector)) %>%
      repeat_add_columns(tibble(adj = c("Europe", "non_Europe"))) %>%
      mutate(subsector = paste(subsector, adj)) %>%
      select(-adj)

    L240.SubsectorAll_reg <- L240.SubsectorAll_reg %>%
        filter(
          !(
            region %in% Europe_regions &
              supplysector %in% TOTAL_NON_CROP &
              grepl("imported", subsector)
          )
        ) %>%
          bind_rows(L240.SubsectorAll_reg_tmp)

    # 2a: Calculate trade between europe and globe ------------------------------------
    # Calculate net trade in GCAM
    Europe_imports_crops <- L240.Production_reg_imp %>%
      filter(supplysector %in% TOTAL_CROPS) %>%
      left_join(ag_regions, by = c("region")) %>%
      filter(!is.na(trade_region)) %>%
      group_by(region = trade_region, sector = subsector, year) %>%
      summarise(imports = sum(calOutputValue )) %>%
      ungroup %>%
      mutate(sector = stringr::str_remove(sector, "imported "))

    Europe_imports_noncrops <- L240.Production_reg_imp %>%
      filter(supplysector %!in% TOTAL_CROPS) %>%
      left_join(ag_regions, by = c("region")) %>%
      filter(!is.na(trade_region)) %>%
      mutate(subsector = stringr::str_remove(subsector, "imported ")) %>%
      select(region, sector = subsector, year, imports = calOutputValue)

    Europe_exports_crops <- L240.Production_tra %>%
      filter(supplysector %in% TRADED_CROPS) %>%
      mutate(region_export = stringr::str_extract(subsector, ".*(?= traded)")) %>%
      left_join(ag_regions, by = c("region_export" = "region")) %>%
      filter(!is.na(trade_region)) %>%
      group_by(region = trade_region, sector = supplysector, year) %>%
      summarise(exports = sum(calOutputValue )) %>%
      ungroup %>%
      mutate(sector = stringr::str_remove(sector, "traded "))

    Europe_exports_noncrops <- L240.Production_tra %>%
      filter(supplysector %in% TRADED_NON_CROP) %>%
      mutate(region_export = stringr::str_extract(subsector, ".*(?= traded)")) %>%
      left_join(ag_regions, by = c("region_export" = "region")) %>%
      filter(!is.na(trade_region)) %>%
      mutate(subsector = stringr::str_remove(subsector, "traded ")) %>%
      select(region = region_export, sector = supplysector, year, exports = calOutputValue) %>%
      mutate(sector = gsub("traded ", "", sector))


    Europe_net_trade_GCAM_crops<- left_join_error_no_match(Europe_imports_crops, Europe_exports_crops,
                                                 by = c("sector", "year", "region")) %>%
      mutate(net_exports_GCAM = exports - imports) %>%
      rename(exports_GCAM = exports, imports_GCAM = imports)

    Europe_net_trade_GCAM_noncrops<- left_join_error_no_match(Europe_imports_noncrops, Europe_exports_noncrops,
                                                           by = c("sector", "year", "region")) %>%
      mutate(net_exports_GCAM = exports - imports) %>%
      rename(exports_GCAM = exports, imports_GCAM = imports)

    commodity_input_map <- L240.TechCoef_tra %>%
      distinct(sector = supplysector, GCAM_commodity = minicam.energy.input) %>%
      mutate(sector = stringr::str_remove(sector, "traded "))

    # Calculate net trade from FAO data, only for European Single Market
    Europe_net_trade_FAO_crops <- tibble()
    for (TRADE_RGN in unique(ag_regions$trade_region)){
      df <- EuroSingleMarket_BiTrade_Ag_crops %>%
        filter(import_trade_region == TRADE_RGN | export_trade_region == TRADE_RGN,
               import_trade_region != export_trade_region,
               GCAM_commodity %in% c(aglu.TRADED_CROPS)) %>%
        mutate(flow = if_else(import_trade_region == TRADE_RGN, "imports", "exports")) %>%
        group_by(GCAM_commodity, flow, year) %>%
        # data is in thousand tonnes, convert to Mt
        summarise(value = sum(value) / 1000) %>%
        ungroup %>%
        tidyr::pivot_wider(names_from = "flow", values_from = "value") %>%
        mutate(net_exports = exports - imports,
               region = TRADE_RGN)

      Europe_net_trade_FAO_crops <- bind_rows(df, Europe_net_trade_FAO_crops)
    }

    European_Single_Market_rg <- ag_regions %>% filter(trade_region == "European_Single_Market") %>% pull(region)
    Europe_Eastern_Market_rg <- ag_regions %>% filter(trade_region == "Europe_Eastern_Market") %>% pull(region)

    # TODO: Avoid hard coding of markets
    # markets <- unique(ag_regions$trade_region)
    # region_to_market <- ag_regions %>%
    #   select(region, trade_region)

    Europe_net_trade_FAO_noncrops <- tibble()
    for (TRADE_RGN in unique(EuroSingleMarket_BiTrade_Ag_noncrops$region)){

      df <- EuroSingleMarket_BiTrade_Ag_noncrops %>%
        filter(region == TRADE_RGN | export_trade_region == TRADE_RGN,
               import_trade_region != export_trade_region,
               GCAM_commodity %in% c(aglu.TRADED_MEATS)) %>%
        mutate(flow = if_else(region == TRADE_RGN, "imports", "exports"))

      market_rgn_import <- ag_regions %>%
        rbind(c("ROW", "ROW", "ROW")) %>%
        filter(region == TRADE_RGN) %>%
        pull(trade_region)

      df <- df %>%
        mutate(market_import = market_rgn_import) %>%
        left_join(ag_regions %>%
                    rename(export_trade_region = region,
                           market_export = trade_region) %>%
                    select(-ag_region), by = "export_trade_region") %>%
        replace_na(list(market_import = "ROW",
                        market_export = "ROW")) %>%
        mutate(flow2 = if_else(flow == "exports" & import_trade_region == market_import, "exports_Europe", "exports_non_Europe")) %>%
        mutate(flow2 = if_else(flow == "imports" & import_trade_region == market_export, "imports_Europe", flow2)) %>%
        mutate(flow2 = if_else(flow == "imports" & import_trade_region != market_export, "imports_non_Europe", flow2)) %>%
        mutate(flow = flow2) %>%
        # select(region, import_trade_region, export) %>%
        group_by(GCAM_commodity, flow, year) %>%
        # data is in thousand tonnes, convert to Mt
        summarise(value = sum(value) / 1000,   .groups = "drop") %>%
        ungroup %>%
        tidyr::pivot_wider(names_from = "flow", values_from = "value") %>%
        mutate(region = TRADE_RGN)

      if (TRADE_RGN == "ROW") {
        df <- df %>%
          mutate(
            exports_Europe = 0,
            imports_Europe = 0
          )
      }

      df <- df %>%
        tidyr::replace_na(list(
          exports_Europe        = 0,
          imports_Europe        = 0,
          exports_non_Europe    = 0,
          imports_non_Europe    = 0
        )) %>%
        mutate(
          net_exports_Europe     = exports_Europe - imports_Europe,
          net_exports_non_Europe = exports_non_Europe - imports_non_Europe,
          region = TRADE_RGN
        ) %>%
        mutate(exports = exports_Europe + exports_non_Europe,
               imports = imports_Europe + imports_non_Europe,
               net_exports = net_exports_Europe + net_exports_non_Europe) %>%
        mutate(share_exp_europe = exports_Europe / exports,
               share_exp_non_Europe = exports_non_Europe / exports,
               share_imp_europe = imports_Europe / imports,
               share_imp_non_Europe = imports_non_Europe / imports,
               share_netexp_europe = net_exports_Europe / net_exports,
               share_netexp_europe = net_exports_non_Europe / net_exports)

      Europe_net_trade_FAO_noncrops <- bind_rows(df, Europe_net_trade_FAO_noncrops)
    }


    # scale exports and imports according to net exports in GCAM
    Europe_net_trade_calib_calc_crops <- Europe_net_trade_FAO_crops %>%
      left_join_error_no_match(commodity_input_map, by = "GCAM_commodity") %>%
      left_join_error_no_match(Europe_net_trade_GCAM_crops, by = c("year", "sector", "region")) %>%
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

    Europe_net_trade_calib_calc_noncrops <- Europe_net_trade_FAO_noncrops %>%
      filter(region != "ROW") %>%
      select(
        GCAM_commodity, year, region,
        exports, imports, net_exports,
        starts_with("share")
      ) %>%
      mutate(
        across(starts_with("share"), ~ replace(.x, is.na(.x) | is.nan(.x), 0))
      ) %>%
      left_join_error_no_match(commodity_input_map, by = "GCAM_commodity") %>%
      left_join_error_no_match(Europe_net_trade_GCAM_noncrops, by = c("year", "sector", "region")) %>%
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
      mutate(imports_europe = imports * share_imp_europe,
             imports_non_Europe = imports * share_imp_non_Europe,
             exports_europe = imports * share_exp_europe,
             exports_non_Europe = imports * share_exp_non_Europe) %>%
      select(sector, region, year, imports_europe, imports_non_Europe, exports_europe, exports_non_Europe)

    # add in any years we don't have in FAO with the GCAM net export values
    Europe_net_trade_calib_crops <- Europe_net_trade_calib_calc_crops %>%
      bind_rows(Europe_net_trade_GCAM_crops %>%
                  mutate(exports = if_else(net_exports_GCAM > 0, net_exports_GCAM, 0),
                         imports = if_else(net_exports_GCAM < 0, -net_exports_GCAM, 0)) %>%
                  select(names(Europe_net_trade_calib_calc_crops)) %>%
                  anti_join(Europe_net_trade_calib_calc_crops, by = c("sector", "year", "region"))) %>%
      tidyr::pivot_longer(cols = c(exports, imports), names_to = "flow", values_to = "value") %>%
      mutate(subsector = if_else(flow == "imports",
                                 paste0("global traded ", sector),
                                 paste(region, "traded", sector, sep = " ")),
             region = if_else(flow == "exports", "USA", region)) %>%
      select(region, subsector, year, value)

    # add in any years we don't have in FAO with the GCAM net export values for non-crops

    # Create shares for non-crop disaggregation
    shares_Europe_net_trade_GCAM_noncrops <- Europe_net_trade_FAO_noncrops %>%
      filter(year == min(year)) %>%
      select(GCAM_commodity, region, starts_with("share"), -share_netexp_europe) %>%
      mutate(across(everything(), ~replace_na(., 0)),
             sector = tolower(GCAM_commodity)) %>%
      select(-GCAM_commodity)

    # Adjust Europe_net_trade_GCAM_noncrops_adj to have EEA vs nonEEA prior to 2010
    Europe_net_trade_GCAM_noncrops_adj <- Europe_net_trade_GCAM_noncrops %>%
      filter(region != "ROW") %>%
      # use left_join fro some missing data
      left_join(shares_Europe_net_trade_GCAM_noncrops, by = c("region", "sector")) %>%
      # there is no data for some region (Belarus) so assume everything is outside EEA. TODO: Wood products
      replace_na(list(
        share_exp_europe = 0,
        share_exp_non_Europe = 1,
        share_imp_europe = 0,
        share_imp_non_Europe = 1
      )) %>%
      mutate(
        imports_europe = imports_GCAM * share_imp_europe,
        imports_non_Europe = imports_GCAM * share_imp_non_Europe,
        exports_europe = exports_GCAM * share_exp_europe,
        exports_non_Europe = imports_GCAM * share_exp_non_Europe,
      )


    Europe_net_trade_calib_noncrops <- Europe_net_trade_calib_calc_noncrops %>%
      left_join_error_no_match(ag_regions, by = "region") %>%
      select(-ag_region) %>%
      bind_rows(Europe_net_trade_GCAM_noncrops_adj %>%
                  left_join_error_no_match(ag_regions, by = "region") %>%
                  select(-ag_region) %>%
                  mutate(exports = if_else(net_exports_GCAM > 0, net_exports_GCAM, 0),
                         imports = if_else(net_exports_GCAM < 0, -net_exports_GCAM, 0)) %>%
                  select(names(Europe_net_trade_calib_calc_noncrops), "trade_region") %>%
                  anti_join(Europe_net_trade_calib_calc_noncrops, by = c("sector", "year", "region"))) %>%
      tidyr::pivot_longer(cols = c(exports_europe, exports_non_Europe, imports_europe, imports_non_Europe), names_to = "flow", values_to = "value") %>%
      mutate(subsector = if_else(flow == "imports_europe",
                                 paste0("imported ", sector, " Europe"),
                                 sector),
             subsector = if_else(flow == "imports_non_Europe",
                                 paste0("imported ", sector, " non_Europe"),
                                 subsector),
             subsector = if_else(flow %in% c("exports_europe","exports_non_Europe"),
                                 paste0(region, " traded ", sector),
                                 subsector),
             region = if_else(flow == "exports_non_Europe", "USA", region),
             region = if_else(flow == "exports_europe", trade_region, region)) %>%
      select(region, subsector, year, value)


     L240.Production_tra_EUR_crops <- L240.Production_tra %>%
      filter(supplysector %in% TRADED_CROPS) %>%
      # this will add in global/Europe single markets, but the cal output values are wrong
      add_single_market %>%
      filter_euro_mkt %>%
      left_join(Europe_net_trade_calib_crops, by = c("region", "subsector", "year")) %>%
      mutate(calOutputValue = if_else(is.na(value), calOutputValue, value),
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

      L240.Production_tra_EUR_noncrops_non_europe <- L240.Production_tra %>%
        filter(supplysector %!in% TRADED_CROPS) %>%
        left_join(Europe_net_trade_calib_noncrops, by = c("region", "subsector", "year")) %>%
        mutate(calOutputValue = if_else(is.na(value), calOutputValue, value),
               subs.share.weight = if_else(calOutputValue > 0, 1, 0),
               tech.share.weight = subs.share.weight) %>%
        select(LEVEL2_DATA_NAMES[["Production"]])

      L240.Production_tra_EUR_noncrops_europe <- L240.Production_tra %>%
        filter(supplysector %!in% TRADED_CROPS) %>%
        select(-region) %>%
        left_join(Europe_net_trade_calib_noncrops %>%
                    filter(region %in% unique(ag_regions$trade_region)), by = c( "subsector", "year")) %>%
        filter(!is.na(region)) %>%
        mutate(calOutputValue = if_else(is.na(value), calOutputValue, value),
               subs.share.weight = if_else(calOutputValue > 0, 1, 0),
               tech.share.weight = subs.share.weight) %>%
        select(LEVEL2_DATA_NAMES[["Production"]])

      L240.Production_tra_EUR <- bind_rows(
        L240.Production_tra_EUR_crops,
        L240.Production_tra_EUR_noncrops_non_europe,
        L240.Production_tra_EUR_noncrops_europe
      )
    #

    # 2b. Adjust traded_crops for europe ----------------------
    # L240.SubsectorAll_reg_EU
    L240.SubsectorAll_reg_noEU <- L240.SubsectorAll_reg %>%
      left_join(ag_regions, by = c("region")) %>%
      filter(!(!is.na(trade_region) &
                 supplysector %in% TOTAL_CROPS))

    L240.SubsectorAll_reg_EUR <- L240.SubsectorAll_reg %>%
      left_join(ag_regions, by = c("region")) %>%
      filter(!is.na(trade_region) &
               supplysector %in% TOTAL_CROPS) %>%
      select(-region) %>%
      distinct() %>%
      mutate(region = trade_region) %>%
      bind_rows(L240.SubsectorAll_reg_noEU) %>%
      select(-trade_region, -ag_region)

    # L240.TechShrwt_reg
    L240.TechShrwt_reg_noEU <- L240.TechShrwt_reg %>%
      filter(!(region %in% ag_regions$region &
                 supplysector %in% TOTAL_CROPS))

    L240.TechShrwt_reg_EUR <- L240.TechShrwt_reg %>%
      inner_join(ag_regions, by = c("region")) %>%
      filter(supplysector %in% TOTAL_CROPS) %>%
      select(-region) %>%
      distinct() %>%
      mutate(region = trade_region) %>%
      bind_rows(L240.TechShrwt_reg_noEU) %>%
      select(-trade_region, -ag_region)

    # L240.Production_reg_dom
    L240.Production_reg_dom_noEU <- L240.Production_reg_dom %>%
      filter(!(region %in% ag_regions$region &
                 supplysector %in% TOTAL_CROPS))

    # add in missing intra-EEA trade to domestic consumption
    EEA_exports <- L240.Production_tra %>%
      mutate(region_tmp = stringr::str_trim(stringr::str_remove(subsector, supplysector))) %>%
      inner_join(ag_regions, by = c("region_tmp" = "region")) %>%
      filter(supplysector %in% TRADED_CROPS) %>%
      group_by(trade_region, supplysector, year) %>%
      summarise(exports = sum(calOutputValue)) %>%
      ungroup()

    missing_exports <- L240.Production_tra_EUR %>%
      mutate(region_tmp = stringr::str_trim(stringr::str_remove(subsector, supplysector))) %>%
      filter(region == "USA", supplysector %in% TRADED_CROPS,
             region_tmp %in% ag_regions$trade_region) %>%
      left_join_error_no_match(EEA_exports, by = c("supplysector", "year", "region_tmp" = "trade_region")) %>%
      mutate(calOutputValue = exports - calOutputValue) %>%
      select(supplysector, year, calOutputValue, trade_region = region_tmp) %>%
      mutate(supplysector = gsub("traded", "total", supplysector),
             subsector = gsub("total", "domestic", supplysector),
             technology = subsector)

    # EEA domestic consumption is sum of countries
    L240.Production_reg_dom_EUR <- L240.Production_reg_dom %>%
      inner_join(ag_regions, by = c("region")) %>%
      filter(supplysector %in% TOTAL_CROPS) %>%
      bind_rows(missing_exports) %>%
      group_by(region = trade_region, supplysector, subsector, technology, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup() %>%
      mutate(share.weight.year = min(MODEL_BASE_YEARS),
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      bind_rows(L240.Production_reg_dom_noEU)

    # L240.Production_reg_imp
    L240.Production_reg_imp_noEU_crops <- L240.Production_reg_imp %>%
      filter(region %!in% ag_regions$region,
            supplysector %in% TOTAL_CROPS)

    L240.Production_reg_imp_noEU_noncrops <- L240.Production_reg_imp %>%
      filter(region %!in% ag_regions$region,
             supplysector %in% TOTAL_NON_CROP)

    # EEA import consumption is from previously calculated trade in Europe_net_trade_calib
    Europe_imports_crops <- Europe_net_trade_calib_crops %>%
      filter(grepl("global", subsector)) %>%
      mutate(supplysector = gsub("global traded", "total", subsector)) %>%
      select(-subsector)

    L240.Production_reg_imp_EUR_crops <- L240.Production_reg_imp %>%
      inner_join(ag_regions, by = c("region")) %>%
      filter(supplysector %in% TOTAL_CROPS) %>%
      distinct(region = trade_region, supplysector, subsector, technology, year, share.weight.year) %>%
      left_join_error_no_match(Europe_imports_crops, by = c("region", "supplysector", "year")) %>%
      rename(calOutputValue = value) %>%
      mutate(subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      bind_rows(L240.Production_reg_imp_noEU_crops)

    Europe_imports_noncrops <- Europe_net_trade_calib_noncrops %>%
      filter(region %in% unique(ag_regions$region)) %>%
      mutate(supplysector = gsub("imported", "total", subsector),
             supplysector = gsub(" Europe", "", supplysector),
             supplysector = gsub(" non_Europe", "", supplysector)) %>%
      mutate(supplysector = if_else(grepl("wood", supplysector), gsub("total", "regional", supplysector) ,supplysector))

    L240.Production_reg_imp_EUR_noncrops <- L240.Production_reg_imp %>%
      inner_join(ag_regions, by = c("region")) %>%
      select(-subsector, -technology, -calOutputValue, -ag_region) %>%
      filter(supplysector %!in% TOTAL_CROPS) %>%
      distinct() %>%
      left_join(Europe_imports_noncrops, by = c("region", "supplysector", "year")) %>%
      rename(calOutputValue = value) %>%
      mutate(subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      mutate(technology = subsector) %>%
      bind_rows(L240.Production_reg_imp_noEU_noncrops) %>%
      rename(market.name = trade_region) %>%
      mutate(market.name = if_else(grepl("non_Europe", subsector), "USA", market.name),
             market.name = if_else(is.na(market.name), "USA", market.name))

    L240.Production_reg_imp_EUR <- bind_rows(
      L240.Production_reg_imp_EUR_crops,
      L240.Production_reg_imp_EUR_noncrops
    )

    L240.TechCost_reg <- L240.Production_reg_dom_EUR %>%
      filter(supplysector %in% TOTAL_CROPS,
             region %in% ag_regions$trade_region) %>%
      left_join_error_no_match(L2012.AgSupplySector_EU %>%
                                 mutate(supplysector = paste0("total ", tolower(AgSupplySector)),
                                        supplysector = gsub("nutsseeds", "nuts_seeds", supplysector),
                                        supplysector = gsub("roottuber", "root_tuber", supplysector)) %>%
                                 select(region, supplysector, price),
                                        by = c("region", "supplysector")) %>%
      mutate(minicam.non.energy.input = "calPrice",
             input.cost = price) %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]])

    # Adjust output structure -------------------

    #L240.TechShrwt_reg_EUR
    L240.TechShrwt_reg_EUR_adj <- L240.TechShrwt_reg_EUR %>%
      filter(region %in% unique(ag_regions$region),
             supplysector %in% TOTAL_NON_CROP,
             !grepl("domestic", subsector))

    L240.TechShrwt_reg_EUR_adj2 <- L240.TechShrwt_reg_EUR_adj %>%
      repeat_add_columns(tibble(adj = c(" Europe", " non_Europe"))) %>%
      mutate(subsector = paste0(subsector, adj),
             technology = paste0(technology, adj)) %>%
      select(-adj)

    L240.TechShrwt_reg_EUR <- L240.TechShrwt_reg_EUR %>%
      anti_join(L240.TechShrwt_reg_EUR_adj, by = c("region", "supplysector", "subsector", "technology")) %>%
      bind_rows(L240.TechShrwt_reg_EUR_adj2)

  #L240.TechCoef_reg
    L240.TechCoef_reg_EUR_adj <- L240.TechCoef_reg_EUR %>%
      filter(region %in% unique(ag_regions$region),
             supplysector %in% TOTAL_NON_CROP,
             !grepl("domestic", subsector))

    L240.TechCoef_reg_EUR_adj2 <- L240.TechCoef_reg_EUR_adj %>%
      repeat_add_columns(tibble(adj = c(" Europe", " non_Europe"))) %>%
      mutate(subsector = paste0(subsector, adj),
             technology = paste0(technology, adj)) %>%
      select(-adj)

    L240.TechCoef_reg_EUR <- L240.TechCoef_reg_EUR %>%
      anti_join(L240.TechCoef_reg_EUR_adj, by = c("region", "supplysector", "subsector", "technology")) %>%
      bind_rows(L240.TechCoef_reg_EUR_adj2) %>%
      left_join(ag_regions %>% select(region, trade_region), by = "region") %>%
      mutate(
        market.name = if_else(grepl(" Europe", subsector), trade_region, market.name),
        market.name = if_else(grepl("non_Europe", subsector), "USA", market.name)
      ) %>%
      select(-trade_region)

    #
    # outputs -------------------
    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
