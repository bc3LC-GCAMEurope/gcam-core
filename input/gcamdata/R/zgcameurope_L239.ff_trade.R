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
    L239.SubsectorAll_tra_EUR <- L239.SubsectorAll_tra # %>%
      # filter(grepl(paste(gcameurope.EUROSTAT_COUNTRIES, collapse = "|"), subsector))
    L239.TechShrwt_tra_EUR <- L239.TechShrwt_tra # %>%
      # filter(grepl(paste(gcameurope.EUROSTAT_COUNTRIES, collapse = "|"), subsector))
    L239.TechCost_tra_EUR <- L239.TechCost_tra # %>%
      # filter(grepl(paste(gcameurope.EUROSTAT_COUNTRIES, collapse = "|"), subsector))
    L239.TechCoef_tra_EUR  <- L239.TechCoef_tra # %>%
      # filter(grepl(paste(gcameurope.EUROSTAT_COUNTRIES, collapse = "|"), subsector))
    L239.TechShrwt_reg_EUR <- L239.TechShrwt_reg %>%
      filter(region %in% c(gcameurope.EUROSTAT_COUNTRIES))
    L239.TechCoef_reg_EUR <- L239.TechCoef_reg %>%
      filter(region %in% gcameurope.EUROSTAT_COUNTRIES)

    # Base technology-level table for several tables to be written out")
    A_ff_TradedTechnology_R_Y <- repeat_add_columns(A_ff_TradedTechnology,
                                                    tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names %>% filter_regions_europe) %>%
      mutate(subsector = paste(region, subsector, sep = " "),
             technology = subsector,
             market.name = region,
             region = gcam.USA_REGION)

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

    # # Add in extra domestic consumption from negative IEA_TPES_diff values
    # IEA_TPES_diff_prod <- L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
    #   filter(sector == "IEA_TPES_diff",
    #          fuel %in% c("coal", "gas", "refined liquids"),
    #          value < 0) %>%
    #   mutate(value = value * -1,
    #          GCAM_Commodity = case_when(
    #            fuel == "gas" ~ "natural gas",
    #            fuel == "refined liquids" ~ "crude oil",
    #            fuel == "coal" ~ "coal")) %>%
    #   left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
    #   select(region, GCAM_Commodity, year, Prod_EJ = value)
    #
    # L239.Prod_EJ_R_C_Y <- L239.Prod_EJ_R_C_Y %>%
    #   bind_rows(IEA_TPES_diff_prod) %>%
    #   group_by(region, GCAM_Commodity, year) %>%
    #   summarise(Prod_EJ = sum(Prod_EJ)) %>%
    #   ungroup

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
