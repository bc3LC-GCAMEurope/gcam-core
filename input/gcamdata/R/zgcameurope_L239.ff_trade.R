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
                               "L239.SubsectorAll_tra",
                               "L239.TechShrwt_tra",
                               "L239.TechCost_tra",
                               "L239.TechCoef_tra",
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
      "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
      "L2011.ff_GrossTrade_EJ_R_C_Y_EUR",
      "L2011.ff_ALL_EJ_R_C_Y_EUR",
      "L239.Production_tra",
      OUTPUTS_TO_COPY_FILTER)

  MODULE_OUTPUTS <-
    c("L239.Production_tra_EUR",
      "L239.Production_reg_imp_EUR",
      "L239.Production_reg_dom_EUR",
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
    copy_filter_europe(all_data, OUTPUTS_TO_COPY_FILTER[!grepl("_tra", OUTPUTS_TO_COPY_FILTER)])

    # need to repeat and keep USA for trade outputs
    L239.Supplysector_tra_EUR <- L239.Supplysector_tra
    L239.SectorUseTrialMarket_tra_EUR <- L239.SectorUseTrialMarket_tra

    # Adjust trade for regions with production in Eurostat, but not IEA ----------------------------
    # Some regions may have had no trade in IEA but have trade in Eurostat
    missing_exports_R_C <- L2011.ff_GrossTrade_EJ_R_C_Y_EUR %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      group_by(region, GCAM_Commodity) %>%
      filter(any(GrossExp_EJ) > 0) %>%
      ungroup %>%
      mutate(supplysector = paste("traded", GCAM_Commodity),
             supplysector = gsub("crude oil", "oil", supplysector),
              subsector = paste(region, supplysector),
             market.name = region) %>%
    distinct(supplysector, subsector, market.name) %>%
    anti_join(L239.SubsectorAll_tra, by = c("supplysector", "subsector"))

    L239.SubsectorAll_tra_EUR <- L239.SubsectorAll_tra %>%
      distinct(region, logit.year.fillout, logit.exponent, year.fillout,
               share.weight, apply.to, from.year, to.year, to.value, interpolation.function, logit.type) %>%
      repeat_add_columns(missing_exports_R_C %>%  select(-market.name)) %>%
      bind_rows(L239.SubsectorAll_tra)

    L239.TechShrwt_tra_EUR <- L239.TechShrwt_tra %>%
      distinct(region, supplysector, year, share.weight) %>%
      right_join(missing_exports_R_C %>%  select(-market.name), by = "supplysector") %>%
      mutate(technology = subsector) %>%
      bind_rows(L239.TechShrwt_tra)
    L239.TechCost_tra_EUR <- L239.TechCost_tra %>%
      distinct(region, supplysector, year, minicam.non.energy.input, input.cost) %>%
      right_join(missing_exports_R_C %>%  select(-market.name), by = "supplysector") %>%
      mutate(technology = subsector) %>%
      bind_rows(L239.TechCost_tra)
    L239.TechCoef_tra_EUR  <- L239.TechCoef_tra %>%
      distinct(region, supplysector, year, minicam.energy.input, coefficient) %>%
      right_join(missing_exports_R_C, by = "supplysector") %>%
      mutate(technology = subsector) %>%
      bind_rows(L239.TechCoef_tra)

    # Base technology-level table for several tables to be written out" ------------
    A_ff_TradedTechnology_R_Y <- repeat_add_columns(A_ff_TradedTechnology,
                                                    tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names %>% filter_regions_europe) %>%
      mutate(subsector = paste(region, subsector, sep = " "),
             technology = subsector,
             market.name = region,
             region = gcam.USA_REGION) %>%
      # Need to remove exports from regions with no fossil resources
      semi_join(L239.SubsectorAll_tra_EUR, by = c("supplysector", "subsector"))


    A_ff_regionalTechnology_R_Y <-  repeat_add_columns(A_ff_RegionalTechnology,
                                                       tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(tibble(region = gcameurope.EUROSTAT_COUNTRIES)) %>%
      mutate(market.name = if_else(market.name == "regional", region, market.name))

    # L239.Production_tra_EUR: Output (gross exports) of traded technologies -------------
    L239.GrossExports_EJ_R_C_Y <- left_join_error_no_match(L2011.ff_GrossTrade_EJ_R_C_Y_EUR,
                                                           GCAM_region_names,
                                                           by = "GCAM_region_ID") %>%
      select(region, GCAM_Commodity, year, GrossExp_EJ)
    L239.Production_tra_EUR <- filter(A_ff_TradedTechnology_R_Y, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L239.GrossExports_EJ_R_C_Y,
                               by = c(market.name = "region", minicam.energy.input = "GCAM_Commodity", "year")) %>%
      filter(market.name %in% gcameurope.EUROSTAT_COUNTRIES) %>%
      rename(calOutputValue = GrossExp_EJ) %>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) %>%
    # add in non-europe regions
      bind_rows(L239.Production_tra %>% filter(!grepl(paste(gcameurope.EUROSTAT_COUNTRIES, collapse = "|"), subsector)))

    # L239.Production_reg_imp_EUR: Output (flow) of gross imports -----------------
    L239.GrossImports_EJ_R_C_Y <- left_join_error_no_match(L2011.ff_GrossTrade_EJ_R_C_Y_EUR,
                                                           GCAM_region_names,
                                                           by = "GCAM_region_ID") %>%
      left_join(select(A_ff_TradedTechnology, supplysector, minicam.energy.input),
                by = c(GCAM_Commodity = "minicam.energy.input")) %>%
      select(region, supplysector, year, GrossImp_EJ)

    L239.Production_reg_imp_EUR <- A_ff_regionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "import", subsector)) %>%
      left_join_error_no_match(L239.GrossImports_EJ_R_C_Y,
                               by = c("region", minicam.energy.input = "supplysector", "year")) %>%
      filter(region %in% gcameurope.EUROSTAT_COUNTRIES) %>%
      rename(calOutputValue = GrossImp_EJ) %>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # L239.Production_reg_dom_EUR: Output (flow) of domestic -----------------
    # Domestic "output" is equal to production (production in L2011.ff_ALL_EJ_R_C_Y_EUR) minus gross exports (calculated in L2011.ff_GrossTrade_EJ_R_C_Y_EUR)

    ### DOMESTIC TECHNOLOGY OUTPUT = AG PRODUCTION - GROSS EXPORTS
    L239.GrossExports_EJ_R_C_Y <- left_join_error_no_match(L2011.ff_GrossTrade_EJ_R_C_Y_EUR,
                                                           GCAM_region_names,
                                                           by = "GCAM_region_ID") %>%
      select(region, GCAM_Commodity, year, GrossExp_EJ)
    L239.Prod_EJ_R_C_Y <- select(L2011.ff_ALL_EJ_R_C_Y_EUR, region, GCAM_Commodity = fuel, year, Prod_EJ = production)

    L239.Production_reg_dom_EUR <- A_ff_regionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "domestic", subsector)) %>%
      left_join_error_no_match(L239.GrossExports_EJ_R_C_Y,
                               by = c("region", minicam.energy.input = "GCAM_Commodity", "year")) %>%
      left_join_error_no_match(L239.Prod_EJ_R_C_Y,
                               by = c("region", minicam.energy.input = "GCAM_Commodity", "year")) %>%
      filter_regions_europe() %>%
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
       filter(region %in% gcameurope.EUROSTAT_COUNTRIES) %>%
       distinct(supplysector, subsector, technology, year, minicam.energy.input, coefficient) %>%
       right_join(missing_domestic_consume, by = c("supplysector", "subsector", "technology")) %>%
       mutate(market.name = region) %>%
       bind_rows(L239.TechCoef_reg %>%filter(region %in% gcameurope.EUROSTAT_COUNTRIES)) %>%
       anti_join(zero_domestic_consume, by = c("supplysector", "subsector", "technology", "region"))

     L239.SubsectorAll_reg_EUR <- L239.SubsectorAll_reg_EUR %>%
       select(-region) %>%
       distinct() %>%
       right_join(missing_domestic_consume %>% distinct(region, supplysector, subsector), by = c("supplysector", "subsector")) %>%
       bind_rows(L239.SubsectorAll_reg_EUR) %>%
       anti_join(zero_domestic_consume, by = c("supplysector", "subsector", "region"))

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
