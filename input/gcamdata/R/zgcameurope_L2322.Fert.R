# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L2322.Fert
#'
#' Provide supply sector information/keywords, subsector shareweights, global technology lifetime,
#' energy inputs and coefficients, global fertilizer manufacturing technologies, etc. for the fertilizer sector.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:  \code{L2322.Supplysector_Fert_EUR}, \code{L2322.FinalEnergyKeyword_Fert_EUR}, \code{L2322.SubsectorLogit_Fert_EUR},
#' \code{L2322.SubsectorShrwtFllt_Fert_EUR}, \code{L2322.SubsectorInterp_Fert_EUR}, \code{L2322.StubTech_Fert_EUR},
#' \code{L2322.StubTechProd_FertProd_EUR}, \code{L2322.StubTechCoef_Fert_EUR}. The corresponding file in the
#' original data system was \code{L2322.Fert.R} (energy level2).
#' @details This chunk provides supply sector information/keywords, subsector shareweights, global technology lifetime,
#' energy inputs and coefficients, global fertilizer manufacturing technologies, etc. for the fertilizer sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select
#' @importFrom tidyr complete nesting
#' @author LF September 2017
module_gcameurope_L2322.Fert <- function(command, ...) {
  GLOBAL_INPUTS_ADJUST <- c("L2322.Supplysector_Fert",
                            "L2322.SectorUseTrialMarket_tra",
                            "L2322.FinalEnergyKeyword_Fert",
                            "L2322.SubsectorLogit_Fert",
                            "L2322.SubsectorShrwtFllt_Fert",
                            "L2322.SubsectorInterp_Fert",
                            "L2322.StubTech_Fert",
                            "L2322.TechShrwt_TradedFert",
                            "L2322.TechCoef_TradedFert",
                            "L2322.StubTechMarket_FertImports",
                            "L2322.StubTechProd_FertProd",
                            "L2322.StubTechCoef_Fert",
                            "L2322.Production_FertExport",
                            "L2322.StubTechProd_FertImport",
                            "L2322.StubTechProd_FertDomCons",
                            "L2322.StubTechProd_NtoAg")
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     FILE = "energy/calibrated_techs",
                     FILE = "energy/A322.sector",
                     FILE = "energy/A322.subsector_interp",
                     FILE = "energy/A322.subsector_logit",
                     FILE = "energy/A322.subsector_shrwt",
                     FILE = "energy/A322.globaltech_coef",
                     FILE = "energy/A322.globaltech_shrwt",
                     FILE = "energy/A322.globaltech_co2capture",
                     FILE = "energy/A322.globaltech_retirement",
                     FILE = "gcam-europe/trade_balances/estat_ammonia_fert",
                     FILE = "gcam-europe/A_ff_RegionalTechnology_EUR",
                     "L1322.Fert_Prod_MtNH3_R_F_Y_EUR",
                     "L1322.IO_R_Fert_F_Yh_EUR",
                     "L1322.Fert_NEcost_75USDkgNH3_F_EUR",
                     "L142.ag_Fert_NetExp_MtN_R_Y",
                     "Europe_Single_Market_Regions",
                     GLOBAL_INPUTS_ADJUST)
  MODULE_OUTPUTS <- paste0(GLOBAL_INPUTS_ADJUST, "_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ------------
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = T)

    GCAM_region_names <- filter_regions_europe(GCAM_region_names)
    L142.ag_Fert_NetExp_MtN_R_Y <- L142.ag_Fert_NetExp_MtN_R_Y %>% filter_regions_europe(region_ID_mapping = GCAM_region_names)
    SINGLE_MARKET_NAME <- unique(A_ff_RegionalTechnology_EUR$market.name[A_ff_RegionalTechnology_EUR$market.name != "regional"])

    # 1a. Supplysector information --------------------------
    # L2322.Supplysector_Fert_EUR: Supply sector information for fertilizer sector
    A322.sector %>%
      filter(supplysector != 'traded ammonia') %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME),
                           GCAM_region_names,
                           has_traded = TRUE) ->
      L2322.Supplysector_Fert_EUR

    # L2322.SectorUseTrialMarket_tra_EUR: Create solved markets for the traded sectors
    L2322.SectorUseTrialMarket_tra_EUR <- filter(A322.sector, traded == 1) %>%
      mutate(region = gcam.USA_REGION,
             use.trial.market = 1) %>%
      select(LEVEL2_DATA_NAMES[["SectorUseTrialMarket"]])

    # L2322.FinalEnergyKeyword_Fert_EUR: Supply sector keywords for fertilizer sector
    A322.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]],
                           GCAM_region_names,
                           has_traded = TRUE) %>%
      na.omit ->
      L2322.FinalEnergyKeyword_Fert_EUR

    # 1b. Subsector information --------------------------
    # L2322.SubsectorLogit_Fert_EUR: Subsector logit exponents of fertilizer sector
    A322.subsector_logit %>%
      filter(supplysector != 'traded ammonia') %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME),
                           GCAM_region_names,
                           has_traded = TRUE) ->
      L2322.SubsectorLogit_Fert_EUR

    # L2322.SubsectorShrwtFllt_Fert_EUR: Subsector shareweights of fertilizer sector
    A322.subsector_shrwt %>%
      filter(!is.na(year.fillout), supplysector != 'traded ammonia') %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]],
                           GCAM_region_names,
                           has_traded = TRUE) ->
      L2322.SubsectorShrwtFllt_Fert_EUR

    # L2322.SubsectorInterp_Fert_EUR: Subsector shareweight interpolation of fertilizer sector
    A322.subsector_interp %>%
      filter(is.na(to.value), supplysector != 'traded ammonia') %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]],
                           GCAM_region_names,
                           has_traded = TRUE) ->
      L2322.SubsectorInterp_Fert_EUR

    # 1c. Technology information --------------------------
    # L2322.StubTech_Fert_EUR: Identification of stub technologies of fertilizer sector
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    # Traded technologies are not represented as stub technologies and are dropped here
    A322.globaltech_shrwt %>%
      filter(traded == 0) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]],
                           GCAM_region_names,
                           has_traded = FALSE) %>%
      rename(stub.technology = technology) ->
      L2322.StubTech_Fert_EUR

    # Traded technologies are assigned to all regions within a given region
    A322.globaltech_shrwt %>%
      filter(traded == 1) %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology, traded), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["TechShrwt"]],
                           GCAM_region_names = GCAM_region_names,
                           has_traded = TRUE) %>%
      mutate(region = gcam.USA_REGION) ->
      L2322.TechShrwt_TradedFert_EUR

    # L2322.GlobalTechCoef_Fert: Energy inputs and coefficients of global fertilizer energy use and feedstocks technologies
    # Traded (export) technologies are not assigned to the global technology database
    A322.globaltech_coef %>%
      filter(traded == 0) %>%
      gather_years(value_col = "coefficient") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2322.GlobalTechCoef_Fert

    # Traded (export) technologies are treated as standard technologies
    A322.globaltech_coef %>%
      filter(traded == 1) %>%
      gather_years(value_col = "coefficient") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, traded), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["TechCoef"]],
                           GCAM_region_names = GCAM_region_names,
                           has_traded = TRUE,
                           set_market = TRUE) %>%
      mutate(region = gcam.USA_REGION) ->
      L2322.TechCoef_TradedFert_EUR

    # Market-names of import technologies are assigned to the USA
    L2322.GlobalTechCoef_Fert %>%
      filter(grepl("import", subsector.name)) %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["StubTechMarket"]],
                           GCAM_region_names,
                           has_traded = FALSE) %>%
      mutate(market.name = gcam.USA_REGION) ->
      L2322.StubTechMarket_FertImports_EUR

    # Carbon capture rates from technologies with CCS
    # L2322.GlobalTechCapture_Fert: CO2 capture fractions from global fertilizer production technologies with CCS
    ## No need to consider historical periods or intermittent technologies here
    A322.globaltech_co2capture %>%
      gather_years(value_col = "remove.fraction") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, remove.fraction, rule = 1),
             remove.fraction = round(remove.fraction, energy.DIGITS_REMOVE.FRACTION)) %>%
      ungroup %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(storage.market = "carbon-storage") ->
      L2322.GlobalTechCapture_Fert

    # Retirement information
    A322.globaltech_retirement %>%
      set_years %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      A322.globaltech_retirement_with_years

    # Copy the data in the last base year period through to the end year
    A322.globaltech_retirement_with_years %>%
      filter(year == MODEL_FINAL_BASE_YEAR) ->
      A322.globaltech_retirement_max_baseyear

    A322.globaltech_retirement_with_years %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(A322.globaltech_retirement_max_baseyear) ->
      L2322.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # All of these options have different headers, and all are allowed

    # 2a. Calibration and region-specific data FERTILIZERS -----------------------------
    # L2322.StubTechProd_FertProd_EUR: calibrated output of fertilizer production technologies
    L1322.Fert_Prod_MtNH3_R_F_Y_EUR %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(calOutputValue = value) %>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = 'GCAM_region_ID') %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector, technology), by = c("sector", "fuel") ) %>%
      rename(stub.technology = technology) %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2322.StubTechProd_FertProd_EUR

    # L2322.StubTechCoef_Fert_EUR: calibrated base-year coefficients of fertilizer production technologies
    L1322.IO_R_Fert_F_Yh_EUR %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(coefficient = value) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      filter(coefficient != 0) %>% # Where 0, drop from this table (to revert to assumed defaults)
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector, technology, minicam.energy.input), by = c("sector", "fuel")) %>%
      mutate(stub.technology = technology, market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) %>%
      mutate(market.name = region) ->
      L2322.StubTechCoef_Fert_EUR

    # 2b. Regional ammonia calibration -----------------------------
    # Ammonia Exports = NetExports where positive
    L142.ag_Fert_NetExp_MtN_R_Y %>%
      select(GCAM_region_ID, year, calOutputValue = value) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = if_else(calOutputValue < 0, 0,
                                      round(calOutputValue / CONV_NH3_N, energy.DIGITS_CALOUTPUT))) %>%   # Convert N export to NH3
      left_join_error_no_match(GCAM_region_names,
                               by = "GCAM_region_ID") %>%
      rename(market.name = region) %>%
      left_join_error_no_match(L2322.TechCoef_TradedFert_EUR,
                               by = c("market.name", "year")) %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L2322.Production_FertExport_EUR

    # Ammonia Imports = NetExports where negative
    L142.ag_Fert_NetExp_MtN_R_Y %>%
      select(GCAM_region_ID, year, calOutputValue = value) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = if_else(calOutputValue > 0, 0,
                                      round(calOutputValue * -1 / CONV_NH3_N, energy.DIGITS_CALOUTPUT))) %>%   # Convert N import to NH3
      left_join_error_no_match(GCAM_region_names,
                               by = "GCAM_region_ID") %>%
      left_join_error_no_match(filter(L2322.StubTech_Fert_EUR, grepl("imported", subsector)),
                               by = "region") %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2322.StubTechProd_FertImport_EUR

    # Ammonia Consumption of Domestic Production = Production - Exports
    L2322.Production_FertExport_EUR %>%
      mutate(region = substr(subsector, 1, regexpr("traded", subsector, fixed = T) - 2)) %>%
      select(region, year, Exports = calOutputValue) ->
      L2322.AmmoniaExports

    L2322.StubTechProd_FertProd_EUR %>%
      group_by(region, year) %>%
      summarise(Production = sum(calOutputValue)) %>%
      ungroup() %>%
      left_join_error_no_match(L2322.AmmoniaExports, by = c("region", "year")) %>%
      mutate(calOutputValue = Production - Exports) %>%
      left_join_error_no_match(filter(L2322.StubTech_Fert_EUR, grepl("domestic", subsector)),
                               by = "region") %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2322.StubTechProd_FertDomCons_EUR

    # Calibrated flow of ammonia to agricultural "N fertilizer"
    # The input of ammonia to N fertilizer is equal to the sum of consumption of domestic production plus imports,
    # times the NH3-to-N stoichiometric mass ratio
    L2322.StubTechProd_FertDomCons_EUR %>%
      select(region, year, DomConsumption = calOutputValue) %>%
      left_join_error_no_match(select(L2322.StubTechProd_FertImport_EUR,
                                      region, year, Imports = calOutputValue),
                               by = c("region", "year")) %>%
      mutate(calOutputValue = round((DomConsumption + Imports) * CONV_NH3_N,
                                    energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(filter(L2322.StubTech_Fert_EUR, supplysector == aglu.FERT_NAME),
                               by = "region") %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2322.StubTechProd_NtoAg_EUR

    # 3a. Functions for ammonia trade adjust -----------------
    combine_EUR_global <- function(df, df_EUR){
      df %>%
        anti_join(df_EUR, by = "region") %>%
        bind_rows(df_EUR)
    }

    copy_for_EUR <- function(df){
      if ("region" %in% names(df)){
        df %>%
          filter(grepl("traded", supplysector)) %>%
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

      if ("market.name" %in% names(df)){
        df_subsector_global <- df_subsector_global %>% mutate(market.name = gcam.USA_REGION)
        df_subsector_EUR <- df_subsector_EUR %>% mutate(market.name = SINGLE_MARKET_NAME)
      }

      df_subsector_EUR <- dplyr::slice_head(df_subsector_EUR) %>%
        ungroup %>%
        mutate(subsector = stringr::str_replace(subsector, region_tmp, SINGLE_MARKET_NAME)) %>%
        bind_rows(df_subsector_global, df_singleMarket)

      if ("technology" %in% names(df)){
        df_subsector_EUR %>% mutate(technology = subsector)  %>% select(-region_tmp)
      } else { df_subsector_EUR %>% select(-region_tmp) }
    }

    subsector_combine_add_market <- function(df, df_EUR){
      tmp <- combine_EUR_global(df, df_EUR)
      tmp %>%
        filter(grepl("traded", supplysector)) %>%
        add_single_market %>%
        bind_rows(tmp %>%  filter(!grepl("traded", supplysector)))
    }
    # 3a. Supplysector ------------------------------------
    L2322.Supplysector_Fert_EUR <- combine_EUR_global(L2322.Supplysector_Fert, L2322.Supplysector_Fert_EUR) %>% distinct() %>% copy_for_EUR
    L2322.SectorUseTrialMarket_tra_EUR <- copy_for_EUR(L2322.SectorUseTrialMarket_tra)
    L2322.FinalEnergyKeyword_Fert_EUR <- combine_EUR_global(L2322.FinalEnergyKeyword_Fert, L2322.FinalEnergyKeyword_Fert_EUR)

    # 3b. Subsector ------------------------------------
    L2322.SubsectorLogit_Fert_EUR <-  subsector_combine_add_market(L2322.SubsectorLogit_Fert, L2322.SubsectorLogit_Fert_EUR)
    L2322.SubsectorShrwtFllt_Fert_EUR <-  subsector_combine_add_market(L2322.SubsectorShrwtFllt_Fert, L2322.SubsectorShrwtFllt_Fert_EUR)
    L2322.SubsectorInterp_Fert_EUR <-  subsector_combine_add_market(L2322.SubsectorInterp_Fert, L2322.SubsectorInterp_Fert_EUR)

    # 3c. Technology ----------------------------------
    # techs with no trade
    L2322.StubTech_Fert_EUR <- combine_EUR_global(L2322.StubTech_Fert, L2322.StubTech_Fert_EUR)
    L2322.StubTechProd_NtoAg_EUR <- combine_EUR_global(L2322.StubTechProd_NtoAg, L2322.StubTechProd_NtoAg_EUR)
    L2322.StubTechProd_FertProd_EUR <- combine_EUR_global(L2322.StubTechProd_FertProd, L2322.StubTechProd_FertProd_EUR)
    L2322.StubTechCoef_Fert_EUR <- combine_EUR_global(L2322.StubTechCoef_Fert, L2322.StubTechCoef_Fert_EUR)
    L2322.StubTechProd_FertImport_EUR  <- combine_EUR_global(L2322.StubTechProd_FertImport, L2322.StubTechProd_FertImport_EUR)
    L2322.StubTechProd_FertDomCons_EUR  <- combine_EUR_global(L2322.StubTechProd_FertDomCons, L2322.StubTechProd_FertDomCons_EUR)

    # change region/market
    L2322.TechShrwt_TradedFert_EUR  <- add_single_market(L2322.TechShrwt_TradedFert)
    L2322.TechCoef_TradedFert_EUR <- add_single_market(L2322.TechCoef_TradedFert)  %>%
      mutate(market.name = if_else(grepl(SINGLE_MARKET_NAME, subsector), SINGLE_MARKET_NAME, market.name),
             market.name = if_else(grepl("global", subsector), gcam.USA_REGION, market.name),
             minicam.energy.input = if_else(market.name == SINGLE_MARKET_NAME | grepl("global", subsector),
                                            supplysector, minicam.energy.input))
    L2322.StubTechMarket_FertImports_EUR <- L2322.StubTechMarket_FertImports %>%
      mutate(market.name = if_else(grepl("imported", subsector) & region %in% Europe_Single_Market_Regions$GCAMEU_region,
                                   SINGLE_MARKET_NAME,
                                   market.name))

    # 3d. Calculate single market trade for L2322.Production_FertExport_EUR --------------------------
    # We don't have full bilateral trade data
    # But from eurostat we have pulled the trade between EU and EU/non-EU
    # Will use to calibrate trade between single market and globe
    Eurostat_ammonia <- estat_ammonia_fert %>%
      group_by(reporter, partner, flow, unit = indicators, year = TIME_PERIOD) %>%
      summarise(value = sum(OBS_VALUE)) %>%
      ungroup %>%
      # convert from 100kg to Mt
      mutate(value = value / 1e10,
             unit = "Mt",
             flow = case_when(
               flow == 1 ~ "imports",
               flow == 2 ~ "exports")) %>%
      filter(partner != "EU_INTRA") %>%
      tidyr::pivot_wider(names_from = flow) %>%
      mutate(net_exports = exports - imports) %>%
      select(-reporter, -partner)

    # Calculate net trade in GCAM
    Europe_imports <- L2322.StubTechProd_FertImport_EUR %>%
      filter(region %in% Europe_Single_Market_Regions$GCAMEU_region) %>%
      group_by(sector = subsector, year) %>%
      summarise(imports = sum(calOutputValue )) %>%
      ungroup %>%
      mutate(region = SINGLE_MARKET_NAME,
             sector = stringr::str_remove(sector, "imported "))

    Europe_exports <- L2322.Production_FertExport %>%
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

    commodity_input_map <- L2322.TechCoef_TradedFert_EUR %>%
      distinct(sector = supplysector, GCAM_commodity = minicam.energy.input) %>%
      mutate(sector = stringr::str_remove(sector, "traded "))

    # scale exports and imports according to net exports in GCAM
    Europe_net_trade_calib_calc <- Eurostat_ammonia %>%
      # Calculate EU net exports
      filter(year %in% MODEL_BASE_YEARS) %>%
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
             net_exports_new = exports - imports,
             imports_adj = case_when(
               (exports < 0 | imports < 0) & net_exports_new > 0 ~ 0,
               (exports < 0 | imports < 0) & net_exports_new <= 0 ~ net_exports_new,
               is.numeric(imports) ~ imports) ,
             exports_adj = case_when(
               (exports < 0 | imports < 0) & net_exports_new > 0 ~ net_exports_new,
               (exports < 0 | imports < 0) & net_exports_new <= 0 ~ 0,
               is.numeric(exports) ~ exports)
             ) %>%
      select(sector, region, year, exports = exports_adj, imports = imports_adj) %>%
      tidyr::pivot_longer(cols = c(exports, imports), names_to = "flow", values_to = "value") %>%
      mutate(region = if_else(flow == "imports", "global", region),
             subsector = paste(region, "traded", sector, sep = " ")) %>%
      select(subsector, year, value)

    L2322.Production_FertExport_EUR <- L2322.Production_FertExport %>%
      # this will add in global/Europe single markets, but the cal output values are wrong
      add_single_market %>%
      left_join(Europe_net_trade_calib_calc, by = c("subsector", "year")) %>%
      mutate(calOutputValue = if_else(is.na(value), calOutputValue, value),
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])


    #
    # Produce outputs ===================================================

    L2322.Supplysector_Fert_EUR %>%
      add_title("Supply sector information for fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A322.sector is expended into all GCAM regions") %>%
      add_legacy_name("L2322.Supplysector_Fert_EUR") %>%
      add_precursors("common/GCAM_region_names", "energy/A322.sector") ->
      L2322.Supplysector_Fert_EUR

    L2322.SectorUseTrialMarket_tra_EUR %>%
      add_title("Supplysector flag indicating to make trial markets for traded ammonia") %>%
      add_units("NA") %>%
      add_comments("This helps model solution") %>%
      add_precursors("energy/A322.sector") ->
      L2322.SectorUseTrialMarket_tra_EUR

    L2322.FinalEnergyKeyword_Fert_EUR %>%
      add_title("Supply sector keywords for fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector, the supply sector final energy keywords from A322.sector are expended into all GCAM regions") %>%
      add_legacy_name("L2322.FinalEnergyKeyword_Fert_EUR") %>%
      add_precursors("common/GCAM_region_names", "energy/A322.sector") ->
      L2322.FinalEnergyKeyword_Fert_EUR

    L2322.SubsectorLogit_Fert_EUR %>%
      add_title("Subsector logit exponents of fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector, the subsector logit exponents from A322.subsector_logit are expanded into all GCAM regions") %>%
      add_legacy_name("L2322.SubsectorLogit_Fert_EUR") %>%
      add_precursors("energy/A322.subsector_logit", "common/GCAM_region_names") ->
      L2322.SubsectorLogit_Fert_EUR

    L2322.SubsectorShrwtFllt_Fert_EUR %>%
      add_title("Subsector shareweights of fertilizer") %>%
      add_units("Unitless") %>%
      add_comments("For fertilizer sector, the subsector shareweights from A322.subsector_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2322.SubsectorShrwtFllt_Fert_EUR") %>%
      add_precursors("energy/A322.subsector_shrwt", "common/GCAM_region_names") ->
      L2322.SubsectorShrwtFllt_Fert_EUR

    L2322.SubsectorInterp_Fert_EUR %>%
      add_title("Subsector shareweight interpolation of fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector, the subsector shareweight interpolation function infromation from A322.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L2322.SubsectorInterp_Fert_EUR") %>%
      add_precursors("energy/A322.subsector_interp", "common/GCAM_region_names") ->
      L2322.SubsectorInterp_Fert_EUR

    L2322.StubTech_Fert_EUR %>%
      add_title("Stub-technology (coal, coal CCS, and etc.) for fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector, the stub technologies from A322.globaltech_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2322.StubTech_Fert_EUR") %>%
      add_precursors("energy/A322.globaltech_shrwt", "common/GCAM_region_names") ->
      L2322.StubTech_Fert_EUR

    L2322.TechShrwt_TradedFert_EUR %>%
      add_title("Shareweights of traded fertilizer sector technologies") %>%
      add_units("Unitless") %>%
      add_comments("For fertilizer sector, the share weights from A322.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_precursors("common/GCAM_region_names") %>%
      same_precursors_as("energy/A322.globaltech_shrwt") ->
      L2322.TechShrwt_TradedFert_EUR

    L2322.TechCoef_TradedFert_EUR %>%
      add_title("Energy inputs, coefficients, and market names of traded fertilizer technologies") %>%
      add_units("unitless IO") %>%
      add_comments("Traded technologies are not assigned to the global technology database") %>%
      add_precursors("common/GCAM_region_names") %>%
      same_precursors_as("energy/A322.globaltech_shrwt") ->
      L2322.TechCoef_TradedFert_EUR

    L2322.StubTechMarket_FertImports_EUR %>%
      add_title("Market-name of ammonia import technologies in each region") %>%
      add_units("unitless IO") %>%
      add_comments("Import markets are cleared within the USA region") %>%
      add_precursors("common/GCAM_region_names") %>%
      same_precursors_as("energy/A322.globaltech_shrwt") ->
      L2322.StubTechMarket_FertImports_EUR

    L2322.StubTechProd_FertProd_EUR %>%
      add_title("calibrated output of fertilizer technologies") %>%
      add_units("Mt N") %>%
      add_comments("Values are calculated using L1322.Fert_Prod_MtNH3_R_F_Y_EUR then added GCAM region information") %>%
      add_legacy_name("L2322.StubTechProd_Fert") %>%
      add_precursors("L1322.Fert_Prod_MtNH3_R_F_Y_EUR", "common/GCAM_region_names", "energy/calibrated_techs") ->
      L2322.StubTechProd_FertProd_EUR

    L2322.StubTechCoef_Fert_EUR %>%
      add_title("calibrated base-year coefficients of fertilizer production technologies") %>%
      add_units("Unitless") %>%
      add_comments("Coefficients are calculated using L1322.IO_R_Fert_F_Yh_EUR") %>%
      add_legacy_name("L2322.StubTechCoef_Fert_EUR") %>%
      add_precursors("L1322.IO_R_Fert_F_Yh_EUR", "common/GCAM_region_names", "energy/calibrated_techs") ->
      L2322.StubTechCoef_Fert_EUR

    L2322.Production_FertExport_EUR %>%
      add_title("calibrated base-year exports of ammonia") %>%
      add_units("Mt NH3") %>%
      add_comments("Calibrated exports of ammonia") %>%
      same_precursors_as(L2322.TechCoef_TradedFert_EUR) %>%
      add_precursors("L142.ag_Fert_NetExp_MtN_R_Y") ->
      L2322.Production_FertExport_EUR

    L2322.StubTechProd_FertImport_EUR %>%
      add_title("calibrated base-year imports of ammonia") %>%
      add_units("Mt NH3") %>%
      add_comments("Calibrated imports of ammonia") %>%
      add_precursors("L142.ag_Fert_NetExp_MtN_R_Y",
                     "common/GCAM_region_names",
                     "energy/A322.globaltech_shrwt") ->
      L2322.StubTechProd_FertImport_EUR

    L2322.StubTechProd_FertDomCons_EUR %>%
      add_title("calibrated base-year consumption of domestically produced ammonia") %>%
      add_units("Mt NH3") %>%
      add_comments("Calculated as production minus exports") %>%
      add_precursors("L142.ag_Fert_NetExp_MtN_R_Y",
                     "common/GCAM_region_names",
                     "energy/A322.globaltech_shrwt") ->
      L2322.StubTechProd_FertDomCons_EUR

    L2322.StubTechProd_NtoAg_EUR %>%
      add_title("calibrated base-year flow of synthetic nitrogen to the agricultural sector") %>%
      add_units("Mt NH3") %>%
      add_comments("Calculated as ammonia consumption of domestic production plus imports, times the N/NH3 mass ratio") %>%
      same_precursors_as(L2322.StubTechProd_FertDomCons_EUR) ->
      L2322.StubTechProd_NtoAg_EUR


    return_data(L2322.Supplysector_Fert_EUR, L2322.SectorUseTrialMarket_tra_EUR,
                L2322.FinalEnergyKeyword_Fert_EUR, L2322.SubsectorLogit_Fert_EUR,
                L2322.SubsectorShrwtFllt_Fert_EUR, L2322.SubsectorInterp_Fert_EUR,
                L2322.StubTech_Fert_EUR, L2322.TechShrwt_TradedFert_EUR,
                L2322.TechCoef_TradedFert_EUR, L2322.StubTechMarket_FertImports_EUR,
                L2322.StubTechProd_FertProd_EUR, L2322.StubTechCoef_Fert_EUR,
                L2322.Production_FertExport_EUR, L2322.StubTechProd_FertImport_EUR, L2322.StubTechProd_FertDomCons_EUR,
                L2322.StubTechProd_NtoAg_EUR)
  } else {
    stop("Unknown command")
  }
}
