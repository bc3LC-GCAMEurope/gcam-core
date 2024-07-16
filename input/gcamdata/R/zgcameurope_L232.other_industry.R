# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L232.other_industry
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for industry-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L232.SectorLogitTables[[ curr_table ]]$data},
#' \code{L232.Supplysector_ind_EUR}, \code{L232.SubsectorLogitTables[[ curr_table ]]$data},
#' \code{L232.SubsectorLogit_ind_EUR}, \code{L232.FinalEnergyKeyword_ind_EUR},
#' \code{L232.SubsectorShrwtFllt_ind_EUR}, \code{L232.SubsectorInterp_ind_EUR},
#' \code{L232.StubTech_ind_EUR}, \code{L232.StubTechInterp_ind_EUR},
#' \code{L232.StubTechCalInput_indenergy_EUR}, \code{L232.StubTechCalInput_indfeed_EUR},
#' \code{L232.StubTechProd_industry_EUR}, \code{L232.StubTechCoef_industry_EUR},
#' \code{L232.FuelPrefElast_indenergy_EUR}, \code{L232.PerCapitaBased_ind_EUR},
#' \code{L232.PriceElasticity_ind_EUR}, \code{L232.BaseService_ind_EUR},
#' \code{L232.IncomeElasticity_ind_EUR_gcam3}, \code{L232.IncomeElasticity_ind_EUR_gssp1},
#' \code{L232.IncomeElasticity_ind_EUR_gssp2}, \code{L232.IncomeElasticity_ind_EUR_gssp3},
#' \code{L232.IncomeElasticity_ind_EUR_gssp4}, \code{L232.IncomeElasticity_ind_EUR_gssp5},
#' \code{L232.IncomeElasticity_ind_EUR_ssp1}, \code{L232.IncomeElasticity_ind_EUR_ssp2},
#' \code{L232.IncomeElasticity_ind_EUR_ssp3}, \code{L232.IncomeElasticity_ind_EUR_ssp4},
#' \code{L232.IncomeElasticity_ind_EUR_ssp5}, \code{object}. The corresponding file in the
#' original data system was \code{L232.industry.R} (energy level2).
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, supplysector/subsector share weights, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join arrange bind_rows distinct filter if_else group_by lag left_join mutate right_join select summarise
#' @importFrom tidyr complete nesting
#' @author LF October 2017
module_gcameurope_L232.other_industry <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     FILE = "energy/calibrated_techs",
                     FILE = "energy/A_regions",
                     FILE = "energy/A23.chp_elecratio",
                     FILE = "energy/A32.sector",
                     FILE = "energy/A32.subsector_interp",
                     FILE = "energy/A32.subsector_logit",
                     FILE = "energy/A32.subsector_shrwt",
                     FILE = "energy/A32.globaltech_coef",
                     FILE = "energy/A32.globaltech_cost",
                     FILE = "energy/A32.globaltech_eff",
                     FILE = "energy/A32.globaltech_shrwt",
                     FILE = "energy/A32.globaltech_interp",
                     FILE = "energy/A32.nonenergy_Cseq",
                     FILE = "energy/A32.fuelprefElasticity",
                     FILE = "energy/A32.globaltech_retirement",
                     FILE = "energy/A32.demand",
                     FILE = "gcam-europe/mappings/grid_regions",
                     "L123.in_EJ_R_indchp_F_Yh_EUR",
                     "L123.eff_R_indchp_F_Yh_EUR",
                     "L1326.in_EJ_R_indenergy_F_Yh_EUR",
                     "L1324.in_EJ_R_indfeed_F_Yh_EUR",
                     "L123.in_EJ_R_indchp_F_Yh",
                     "L1326.in_EJ_R_indenergy_F_Yh",
                     "L1324.in_EJ_R_indfeed_F_Yh",
                     FILE = "socioeconomics/A32.inc_elas_output",
                     "L101.Pop_thous_GCAM3_R_Y",
                     "L102.pcgdp_thous90USD_GCAM3_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y",
                     "L232.GlobalTechEff_ind")

  INCOME_ELASTICITY_OUTPUTS <- c("GCAM3",
                                 paste0("gSSP", 1:5),
                                 paste0("SSP", 1:5))

  MODULE_OUTPUTS <- c("L232.Supplysector_ind_EUR",
                      "L232.SubsectorLogit_ind_EUR",
                      "L232.FinalEnergyKeyword_ind_EUR",
                      "L232.SubsectorShrwtFllt_ind_EUR",
                      "L232.SubsectorInterp_ind_EUR",
                      "L232.StubTech_ind_EUR",
                      "L232.StubTechInterp_ind_EUR",
                      "L232.StubTechCalInput_indenergy_EUR",
                      "L232.StubTechCalInput_indfeed_EUR",
                      "L232.StubTechProd_industry_EUR",
                      "L232.StubTechCoef_industry_EUR",
                      "L232.StubTechSecOut_ind_EUR",
                      "L232.FuelPrefElast_indenergy_EUR",
                      "L232.PerCapitaBased_ind_EUR",
                      "L232.PriceElasticity_ind_EUR",
                      "L232.BaseService_ind_EUR",
                      paste("L232.IncomeElasticity_ind_EUR", tolower(INCOME_ELASTICITY_OUTPUTS), sep = "_"))

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    GCAM_region_names <- GCAM_region_names %>% filter_regions_europe(regions_to_keep_name = unique(c(grid_regions$region, gcameurope.EUROSTAT_COUNTRIES)))
    A_regions <- A_regions %>% filter_regions_europe(regions_to_keep_name = unique(c(grid_regions$region, gcameurope.EUROSTAT_COUNTRIES)))
    L101.Pop_thous_GCAM3_R_Y <- L101.Pop_thous_GCAM3_R_Y %>% filter_regions_europe(region_ID_mapping = GCAM_region_names)
    L102.pcgdp_thous90USD_GCAM3_R_Y <- L102.pcgdp_thous90USD_GCAM3_R_Y %>% filter_regions_europe(region_ID_mapping = GCAM_region_names)
    L102.pcgdp_thous90USD_Scen_R_Y <- L102.pcgdp_thous90USD_Scen_R_Y %>% filter_regions_europe(region_ID_mapping = GCAM_region_names)

    # Add in segment regions not in Eurostat
    L123.in_EJ_R_indchp_F_Yh_EUR <- replace_with_eurostat(L123.in_EJ_R_indchp_F_Yh, L123.in_EJ_R_indchp_F_Yh_EUR) %>%
      filter_regions_europe(regions_to_keep_name = GCAM_region_names$region, region_ID_mapping = GCAM_region_names)

    L1326.in_EJ_R_indenergy_F_Yh_EUR <- replace_with_eurostat(L1326.in_EJ_R_indenergy_F_Yh, L1326.in_EJ_R_indenergy_F_Yh_EUR) %>%
      filter_regions_europe(regions_to_keep_name = GCAM_region_names$region, region_ID_mapping = GCAM_region_names)

    L1324.in_EJ_R_indfeed_F_Yh_EUR <- replace_with_eurostat(L1324.in_EJ_R_indfeed_F_Yh, L1324.in_EJ_R_indfeed_F_Yh_EUR) %>%
      filter_regions_europe(regions_to_keep_name = GCAM_region_names$region, region_ID_mapping = GCAM_region_names)

    # ===================================================
    # 0. Give binding for variable names used in pipeline
    has_district_heat <- sector <- fuel <- supplysector <- subsector <-
      technology <- year.fillout <- to.value <- year <- share.weight <-
      efficiency <- minicam.energy.input <- secondary.output <- coefficient <-
      elec_ratio <- output.ratio <- . <- year.x <- output.ratio.x <- output.ratio.y <-
      input.cost <- minicam.non.energy.input <- GCAM_region_ID <- value <-
      calibrated.value <- sector.name <- subsector.name <- region <-
      calOutputValue <- subs.share.weight <- calOutputValue.x <- calOutputValue.y <-
      output_tot <- value.x <- value.y <- total <- fuelprefElasticity <-
      terminal_coef <- criteria <- scenario <- temp_lag <- base.service <- energy.final.demand <-
      parameter <- income.elasticity <- L232.IncomeElasticity_ind_EUR_gcam3 <-
      L232.IncomeElasticity_ind_EUR_gssp1 <- L232.IncomeElasticity_ind_EUR_gssp2 <-
      L232.IncomeElasticity_ind_EUR_gssp3 <- L232.IncomeElasticity_ind_EUR_gssp4 <-
      L232.IncomeElasticity_ind_EUR_gssp5 <- L232.IncomeElasticity_ind_EUR_ssp1 <-
      L232.IncomeElasticity_ind_EUR_ssp2 <- L232.IncomeElasticity_ind_EUR_ssp3 <-
      L232.IncomeElasticity_ind_EUR_ssp4 <- L232.IncomeElasticity_ind_EUR_ssp5 <-
      market.name <- stub.technology <- year.y <- NULL

    # ===================================================
    # 1. Perform computations
    # Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
    has_not_heat <- filter(A_regions, has_district_heat == 0) # intermediate tibble

    calibrated_techs %>%
      filter(grepl("industry", sector) & fuel == "heat") %>%
      select(supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(GCAM_region_ID = has_not_heat[["GCAM_region_ID"]])) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L232.rm_heat_techs_R # intermediate tibble

    # 1a. Supplysector information
    # L232.Supplysector_ind_EUR: Supply sector information for industry sector
    A32.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L232.Supplysector_ind_EUR

    # L232.FinalEnergyKeyword_ind_EUR: Supply sector keywords for industry sector
    A32.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L232.FinalEnergyKeyword_ind_EUR

    # 1b. Subsector information
    # L232.SubsectorLogit_ind_EUR: Subsector logit exponents of industry sector
    A32.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) -> # Remove non-existent heat subsectors from each region
      L232.SubsectorLogit_ind_EUR

    # L232.SubsectorShrwtFllt_ind_EUR: Subsector shareweights of industry sector
    A32.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) %>%
      anti_join(L232.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat technologies from each region
      L232.SubsectorShrwtFllt_ind_EUR

    # L232.SubsectorInterp_ind_EUR: Subsector shareweight interpolation of industry sector
    A32.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) %>%
      anti_join(L232.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat technologies from each region
      L232.SubsectorInterp_ind_EUR

    # 1c. Technology information
    # L232.StubTech_ind_EUR: Identification of stub technologies of industrial sector
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A32.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      anti_join(L232.rm_heat_techs_R, by = c("region", "technology")) %>% # Remove non-existent heat technologies from each region
      rename(stub.technology = technology) ->
      L232.StubTech_ind_EUR

    # L232.StubTechInterp_ind_EUR: Shareweight interpolation of global industrial sector technologies
    A32.globaltech_interp %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["TechInterp"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L232.StubTechInterp_ind_EUR

    # Energy inputs and efficiency of global industrial energy use and feedstocks technologies
    A32.globaltech_eff %>%
      gather_years(value_col = "efficiency") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, secondary.output),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, secondary.output, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, secondary.output) %>%
      mutate(efficiency = approx_fun(year, efficiency, rule = 1),
             efficiency = round(efficiency, energy.DIGITS_EFFICIENCY)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) ->
      L232.globaltech_eff.long # intermediate tibble

    # Calibration and region-specific data
    # L232.StubTechCalInput_indenergy_EUR: calibrated input of industrial energy use technologies (including cogen)
    L1326.in_EJ_R_indenergy_F_Yh_EUR %>%
      bind_rows(L123.in_EJ_R_indchp_F_Yh_EUR) %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, MODEL_BASE_YEARS)) %>%
      arrange(GCAM_region_ID, sector, fuel, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(select(calibrated_techs, sector, fuel, supplysector, subsector, technology), by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) ->
      L232.in_EJ_R_indenergy_F_Yh # intermediate tibble

    L232.in_EJ_R_indenergy_F_Yh %>%
      left_join_error_no_match(distinct(select(A32.globaltech_eff, subsector, technology, minicam.energy.input)),
                               by = c("subsector", "stub.technology" = "technology")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year) %>%
      rename(calOutputValue = calibrated.value) %>%  # temporary column name change to accommodate function set_subsector_shrwt
      set_subsector_shrwt %>%
      rename(calibrated.value = calOutputValue) %>% # temporary column name change to accommodate function set_subsector_shrwt
      mutate(tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L232.StubTechCalInput_indenergy_EUR

    # L232.StubTechCalInput_indfeed_EUR: calibrated input of industrial feedstock technologies
    L1324.in_EJ_R_indfeed_F_Yh_EUR %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(distinct(select(calibrated_techs, sector, fuel, supplysector, subsector, technology)),
                               by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) ->
      L232.in_EJ_R_indfeed_F_Yh # intermediate tibble

    L232.in_EJ_R_indfeed_F_Yh %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], "value") %>%
      left_join(distinct(select(A32.globaltech_eff, subsector, technology, minicam.energy.input)),
                by = c("subsector", "stub.technology" = "technology")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      select(-value) %>%
      mutate(share.weight.year = year) %>%
      rename(calOutputValue = calibrated.value) %>%  # temporary column name change to accommodate function set_subsector_shrwt
      set_subsector_shrwt %>%
      rename(calibrated.value = calOutputValue) %>% # temporary column name change to accommodate function set_subsector_shrwt
      mutate(tech.share.weight = if_else(calibrated.value > 0, 1, 0)) ->
      L232.StubTechCalInput_indfeed_EUR

    # L232.StubTechProd_industry_EUR: calibrated output of industrial sector
    # First, calculate service output by technology, for energy-use and feedstocks
    L232.in_EJ_R_indenergy_F_Yh %>%
      bind_rows(L232.in_EJ_R_indfeed_F_Yh) %>%
      left_join_error_no_match(select(L232.globaltech_eff.long, sector.name, subsector.name, technology, year,efficiency),
                               by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
                                      "stub.technology" = "technology", "year")) %>%
      mutate(calOutputValue = round(value * efficiency, energy.DIGITS_CALOUTPUT)) ->
      L232.out_EJ_R_ind_serv_F_Yh # intermediate tibble

    # intermediate tibble to extract industry names
    A32.globaltech_shrwt %>%
      filter(supplysector == "other industry") %>%
      select(supplysector, subsector, technology) ->
      L232.industry_names

    # Aggregate service output by region. This is the output of the industrial sector in each region.
    L232.out_EJ_R_ind_serv_F_Yh %>%
      group_by(region, GCAM_region_ID, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup %>%
      mutate(supplysector = L232.industry_names[["supplysector"]],
             subsector = L232.industry_names[["subsector"]],
             stub.technology = L232.industry_names[["technology"]],
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L232.StubTechProd_industry_EUR

    # L232.StubTechCoef_industry_EUR: calibrated output of industrial sector
    # Next, aggregate service output by sector to calculate the portion of each input
    L232.out_EJ_R_ind_serv_F_Yh %>%
      group_by(region, GCAM_region_ID, supplysector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup %>%
      left_join_error_no_match(select(L232.StubTechProd_industry_EUR, calOutputValue, region, year),
                               by = c("region", "year")) %>%
      rename(calOutputValue = calOutputValue.x,
             output_tot = calOutputValue.y) %>%
      mutate(coefficient = if_else(output_tot == 0, 0, calOutputValue / output_tot)) %>%
      rename(minicam.energy.input = supplysector) %>%
      mutate(supplysector = L232.industry_names[["supplysector"]],
             subsector = L232.industry_names[["subsector"]],
             stub.technology = L232.industry_names[["technology"]],
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L232.StubTechCoef_industry_EUR_base # intermediate tibble?

    # This set of coefficients covers only the base years; the first "future" period will default to the global tech coefficient
    # Instead, interpolate the coefficients to these global default values in a specified period
    L232.StubTechCoef_industry_EUR_base %>%
      complete(nesting(region, supplysector, subsector, stub.technology, minicam.energy.input, market.name),
               year = unique(c(MODEL_YEARS, energy.INDCOEF_CONVERGENCE_YR))) %>%
      left_join(select(A32.globaltech_coef, supplysector, subsector, technology, minicam.energy.input, terminal_coef),
                by = c("supplysector", "subsector", stub.technology = "technology", "minicam.energy.input")) %>%
      mutate(coefficient = if_else(year == energy.INDCOEF_CONVERGENCE_YR, terminal_coef, coefficient)) %>%
      select(-terminal_coef) %>%
      group_by(region, supplysector, subsector, stub.technology, minicam.energy.input) %>%
      mutate(coefficient = round(approx_fun(year, coefficient), energy.DIGITS_COEFFICIENT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) ->   # drop the terminal coef year if it's outside of the model years
      L232.StubTechCoef_industry_EUR

    # L232.StubTechSecOut_ind_EUR
    L232.StubTechSecOut_ind_EUR <- L123.eff_R_indchp_F_Yh_EUR %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L232.GlobalTechEff_ind %>%
                                 filter(grepl("cogen", technology)),
                               by = c("year", "fuel" = "subsector.name")) %>%
      mutate(output.ratio = value / efficiency,
             output.ratio = round(output.ratio, energy.DIGITS_COEFFICIENT),
             fractional.secondary.output = "electricity") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, supplysector = sector.name, subsector = fuel,
             stub.technology = technology, fractional.secondary.output, year, output.ratio) %>%
      # NOTE: holding the output ratio constant over time in future periods
      complete(nesting(region,supplysector, subsector, stub.technology, fractional.secondary.output),
               year = c(MODEL_YEARS)) %>%
      group_by(region, supplysector, subsector, stub.technology, fractional.secondary.output) %>%
      mutate(output.ratio = approx_fun(year, output.ratio, rule = 2)) %>%
      ungroup %>%
      left_join(grid_regions, by = "region") %>%
      mutate(market.name = if_else(is.na(grid_region), region, grid_region)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechFractSecOutMarket"]])

    # L232.FuelPrefElast_indenergy_EUR: fuel preference elasticities of industrial energy use
    # First, calculate the fuel shares allocated to each fuel
    L232.in_EJ_R_indenergy_F_Yh %>%
      group_by(region, GCAM_region_ID, supplysector, subsector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup ->
      L232.indenergy_fuel_shares # intermediate tibble

    L232.indenergy_fuel_shares %>%
      group_by(region, GCAM_region_ID, supplysector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      right_join(L232.indenergy_fuel_shares, by = c("region", "GCAM_region_ID", "supplysector", "year")) %>%
      rename(total = value.x,
             value = value.y) %>%
      mutate(share = value / total) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      mutate(fuelprefElasticity = 0) ->
      L232.indenergy_fuel_shares

    # Set fuel preference elasticities as indicated by exogenous rules
    # For each row of L232.indenergy_fuel_shares, we check for any entries in A32.fuelprefElasticity
    # that have the same `subsector` entry, and apply the rule specified (the share in a 'greater than'
    # entry is only used if the share value is larger; similar logic applies to 'lesser than' entries).
    # This is an inefficient way to do this, but the datasets involved are small
    A32.fuelprefElasticity.gt <- filter(A32.fuelprefElasticity, criteria == "greater than")
    for(j in seq_len(nrow(L232.indenergy_fuel_shares))) {
      for(i in seq_len(nrow(A32.fuelprefElasticity.gt))) {
        L232.indenergy_fuel_shares$fuelprefElasticity[j][
          L232.indenergy_fuel_shares$subsector[j] == A32.fuelprefElasticity.gt$subsector[i] &
            L232.indenergy_fuel_shares$share[j] > A32.fuelprefElasticity.gt$share[i] ] <-
          A32.fuelprefElasticity.gt$fuelprefElasticity[i]
      }
    }

    A32.fuelprefElasticity.lt <- filter(A32.fuelprefElasticity, criteria == "lesser than")
    for(j in seq_len(nrow(L232.indenergy_fuel_shares))) {
      for(i in seq_len(nrow(A32.fuelprefElasticity.lt))) {
        L232.indenergy_fuel_shares$fuelprefElasticity[j][
          L232.indenergy_fuel_shares$subsector[j] == A32.fuelprefElasticity.lt$subsector[i] &
            L232.indenergy_fuel_shares$share[j] < A32.fuelprefElasticity.lt$share[i] ] <-
          A32.fuelprefElasticity.lt$fuelprefElasticity[i]
      }
    }

    # The fuel preference elasticities only matter in future periods. Fill out from the first future model time period
    L232.indenergy_fuel_shares %>%
      mutate(year.fillout = min(MODEL_FUTURE_YEARS)) %>%
      filter(fuelprefElasticity != 0) %>%
      select(LEVEL2_DATA_NAMES[["FuelPrefElast"]]) ->
      L232.FuelPrefElast_indenergy_EUR

    # L232.PerCapitaBased_ind_EUR: per-capita based flag for industry final demand
    tibble(region = GCAM_region_names[["region"]]) %>%
      mutate(energy.final.demand = A32.demand[["energy.final.demand"]],
             perCapitaBased = A32.demand[["perCapitaBased"]]) ->
      L232.PerCapitaBased_ind_EUR

    # Retirement information
    A32.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L232.globaltech_retirement_base

    # Copies first future year retirment information into all future years and appends back onto base year
    L232.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L232.globaltech_retirement_future

    # filters base years from original and then appends future years
    L232.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      bind_rows(L232.globaltech_retirement_future) ->
      L232.globaltech_retirement

    # L232.PriceElasticity_ind_EUR: price elasticity of industry final demand
    # Price elasticities are only applied to future periods. Application in base years will cause solution failure
    tibble(region = rep(GCAM_region_names[["region"]], times = length(MODEL_FUTURE_YEARS))) %>%
      mutate(energy.final.demand = A32.demand[["energy.final.demand"]],
             year = sort(rep(MODEL_FUTURE_YEARS, times = nrow(GCAM_region_names))),
             price.elasticity = A32.demand[["price.elasticity"]]) ->
      L232.PriceElasticity_ind_EUR

    # L232.BaseService_ind_EUR: base-year service output of industry final demand
    # Base service is equal to the output of the industry supplysector
    L232.StubTechProd_industry_EUR %>%
      select(region, year, calOutputValue) %>%
      rename(base.service = calOutputValue) %>%
      mutate(energy.final.demand = A32.demand[["energy.final.demand"]]) ->
      L232.BaseService_ind_EUR

    # L232.IncomeElasticity_ind_EUR_scen: income elasticity of industry (scenario-specific)
    L102.pcgdp_thous90USD_GCAM3_R_Y %>%
      # Combine GCAM 3.0 with the SSPs, and subset only the relevant years
      mutate(scenario = 'GCAM3') %>%
      bind_rows(L102.pcgdp_thous90USD_Scen_R_Y) %>%
      filter(year %in% c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS)) %>%
      # Per-capita GDP ratios, which are used in the equation for demand growth
      group_by(GCAM_region_ID, scenario) %>%
      mutate(temp_lag = lag(value, 1),
             value = value / temp_lag) %>%
      ungroup %>%
      select(-temp_lag) %>%
      filter(year %in% MODEL_FUTURE_YEARS) ->
      L232.pcgdpRatio_ALL_R_Y # intermediate tibble

    # Calculate the industrial output as the base-year industrial output times the GDP ratio raised to the income elasticity
    # The income elasticity is looked up based on the prior year's output
    L232.pcgdpRatio_ALL_R_Y %>%
      select(GCAM_region_ID, scenario) %>%
      distinct %>%
      left_join_error_no_match(GCAM_region_names, by = 'GCAM_region_ID') %>%
      mutate(year = max(MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L232.BaseService_ind_EUR, by = c("year", "region")) %>%
      left_join_error_no_match(L101.Pop_thous_GCAM3_R_Y, by = c("year", "GCAM_region_ID")) %>%
      mutate(value = base.service * CONV_BIL_THOUS / value) %>%
      select(-base.service, -energy.final.demand) ->
      L232.Output_ind

    # At each time, the output is equal to the prior period's output times the GDP ratio, raised to the elasticity
    # that corresponds to the output that was observed in the prior time period. This method prevents (ideally) runaway
    # industrial production.
    elast_years <- c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS)
    for(i in seq_along(elast_years)[-1]) {
      L232.Output_ind %>%
        filter(year == elast_years[i - 1]) %>%
        left_join(filter(L232.pcgdpRatio_ALL_R_Y, year == elast_years[i]), by = c("GCAM_region_ID", "scenario")) %>% # strick left join fails timeshift test due to NAs in L102.pcgdp_thous90USD_Scen_R_Y under timeshift mode
        mutate(parameter = approx(x = A32.inc_elas_output[["pc.output_GJ"]],
                                  y = A32.inc_elas_output[["inc_elas"]],
                                  xout = value.x,
                                  rule = 2)[['y']],
               value = value.x * value.y ^ parameter,
               year = elast_years[i]) %>%
        select(GCAM_region_ID, scenario, region, year, value) %>%
        bind_rows(L232.Output_ind) ->
        L232.Output_ind
    }

    # Now that we have industrial output, we can back out the appropriate income elasticities
    L232.Output_ind %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      mutate(value = approx( x = A32.inc_elas_output[["pc.output_GJ"]],
                             y = A32.inc_elas_output[["inc_elas"]],
                             xout = value,
                             rule = 2)[["y"]]) %>%
      mutate(value = round(value, energy.DIGITS_INCELAS_IND)) %>%
      rename(income.elasticity = value) %>%
      mutate(energy.final.demand = A32.demand[["energy.final.demand"]]) ->
      L232.IncomeElasticity_ind_EUR # intermediate tibble

    # KVC: SSP1 needs lower income elasticities. Storyline has limited growth in energy-related industries
    # because of warm fuzzy feelings about environment. We are hard-coding this for a while.
    L232.IncomeElasticity_ind_EUR %>%
      filter(scenario == "SSP1") %>%
      mutate(income.elasticity = income.elasticity * 0.75) %>%
      bind_rows(filter(L232.IncomeElasticity_ind_EUR, scenario != "SSP1")) ->
      L232.IncomeElasticity_ind_EUR

    # ===================================================
    # Produce outputs

    # Extract GCAM3, SSP, and gSSP data and assign to separate tables
    for(ieo in INCOME_ELASTICITY_OUTPUTS) {
      L232.IncomeElasticity_ind_EUR %>%
        filter(scenario == ieo) %>%
        select(LEVEL2_DATA_NAMES[["IncomeElasticity"]]) %>%
        add_title(paste("Income elasticity of industry -", ieo)) %>%
        add_units("Unitless") %>%
        add_comments("First calculate industrial output as the base-year industrial output times the GDP ratio raised to the income elasticity") %>%
        add_comments("Then back out the appropriate income elasticities from industrial output") %>%
        add_comments("Note lower income elasticities for SSP1 are hard-coded.") %>%
        add_legacy_name(paste0("L232.IncomeElasticity_ind_EUR_", tolower(ieo))) %>%
        add_precursors("L102.pcgdp_thous90USD_GCAM3_R_Y", "L102.pcgdp_thous90USD_Scen_R_Y", "common/GCAM_region_names", "L1326.in_EJ_R_indenergy_F_Yh_EUR", "L123.in_EJ_R_indchp_F_Yh_EUR", "energy/calibrated_techs", "L1324.in_EJ_R_indfeed_F_Yh_EUR", "energy/A32.globaltech_eff", "energy/A32.globaltech_shrwt", "energy/A32.demand", "L101.Pop_thous_GCAM3_R_Y", "socioeconomics/A32.inc_elas_output") ->
        x
      assign(paste0("L232.IncomeElasticity_ind_EUR_", tolower(ieo)), x)
    }

    L232.Supplysector_ind_EUR %>%
      add_title("Supply sector information for industry sector") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A32.sector is expended into all GCAM regions") %>%
      add_legacy_name("L232.Supplysector_ind_EUR") %>%
      add_precursors("common/GCAM_region_names", "energy/A32.sector") ->
      L232.Supplysector_ind_EUR

    L232.SubsectorLogit_ind_EUR %>%
      add_title("Subsector logit exponents of industry sector") %>%
      add_units("Unitless") %>%
      add_comments("For industry sector, the subsector logit exponents from A32.subsector_logit are expanded into all GCAM regions with non-existent heat subsectors removed") %>%
      add_legacy_name("L232.SubsectorLogit_ind_EUR") %>%
      add_precursors("energy/A32.subsector_logit", "common/GCAM_region_names", "energy/A_regions", "energy/calibrated_techs") ->
      L232.SubsectorLogit_ind_EUR

    L232.FinalEnergyKeyword_ind_EUR %>%
      add_title("Supply sector keywords for industry sector") %>%
      add_units("NA") %>%
      add_comments("Set supply sector keywords for industry sector for all GCAM regions") %>%
      add_legacy_name("L232.FinalEnergyKeyword_ind_EUR") %>%
      add_precursors("energy/A32.sector", "common/GCAM_region_names") ->
      L232.FinalEnergyKeyword_ind_EUR

    L232.SubsectorShrwtFllt_ind_EUR %>%
      add_title("Subsector shareweights of industry sector") %>%
      add_units("Unitless") %>%
      add_comments("For industry sector, the subsector shareweights from A32.subsector_shrwt are expanded into all GCAM regions with non-existent heat technologies") %>%
      add_legacy_name("L232.SubsectorShrwtFllt_ind_EUR") %>%
      add_precursors("energy/A32.subsector_shrwt", "common/GCAM_region_names", "energy/A_regions", "energy/calibrated_techs") ->
      L232.SubsectorShrwtFllt_ind_EUR

    L232.SubsectorInterp_ind_EUR %>%
      add_title("Subsector shareweight interpolation of industry sector") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the subsector shareweight interpolation function infromation from A32.subsector_interp is expanded into all GCAM regions with non-existent heat technologies removed") %>%
      add_legacy_name("L232.SubsectorInterp_ind_EUR") %>%
      add_precursors("energy/A32.subsector_interp", "common/GCAM_region_names", "energy/A_regions", "energy/calibrated_techs") ->
      L232.SubsectorInterp_ind_EUR

    L232.StubTech_ind_EUR %>%
      add_title("Identification of stub technologies of industrial sector") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the stub technologies from A32.globaltech_shrwt are expanded into all GCAM regions with non-existent heat technologies removed") %>%
      add_legacy_name("L232.StubTech_ind_EUR") %>%
      add_precursors("energy/A32.globaltech_shrwt", "common/GCAM_region_names", "energy/A_regions", "energy/calibrated_techs") ->
      L232.StubTech_ind_EUR

    L232.StubTechInterp_ind_EUR %>%
      add_title("Shareweight interpolation of global industrial sector technologies") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the interpolation function from A32.globaltech_interp are expanded into all GCAM regions") %>%
      add_legacy_name("L232.StubTechInterp_ind_EUR") %>%
      add_precursors("energy/A32.globaltech_interp", "common/GCAM_region_names") ->
      L232.StubTechInterp_ind_EUR

    L232.StubTechCalInput_indenergy_EUR %>%
      add_title("Calibrated input of industrial energy use technologies (including cogen)") %>%
      add_units("EJ") %>%
      add_comments("Calibrated input of industrial energy use values are calculated using L1326.in_EJ_R_indenergy_F_Yh_EUR then added information such as subsector, technology, minicam.energy.input, calibration, tech.share.weight, and etc.") %>%
      add_legacy_name("L232.StubTechCalInput_indenergy_EUR") %>%
      add_precursors("L1326.in_EJ_R_indenergy_F_Yh_EUR", "energy/calibrated_techs", "energy/A32.globaltech_eff") ->
      L232.StubTechCalInput_indenergy_EUR

    L232.StubTechCalInput_indfeed_EUR %>%
      add_title("Calibrated input of industrial feedstock technologies") %>%
      add_units("EJ") %>%
      add_comments("calibrated input of industrial feedstock technologies values are calculated using L1324.in_EJ_R_indfeed_F_Yh_EUR then added information such as subsector, technology, minicam.energy.input, calibration, tech.share.weight, and etc.") %>%
      add_legacy_name("L232.StubTechCalInput_indfeed_EUR", overwrite = T) %>%
      add_precursors("L1324.in_EJ_R_indfeed_F_Yh_EUR", "common/GCAM_region_names", "energy/calibrated_techs", "energy/A32.globaltech_eff") ->
      L232.StubTechCalInput_indfeed_EUR

    L232.StubTechProd_industry_EUR %>%
      add_title("Calibrated output of industrial sector") %>%
      add_units("EJ") %>%
      add_comments("Service output values are calculated by technology, for energy-use and feedstocks then aggregated by region") %>%
      add_legacy_name("L232.StubTechProd_industry_EUR") %>%
      add_precursors("L1326.in_EJ_R_indenergy_F_Yh_EUR", "L123.in_EJ_R_indchp_F_Yh_EUR", "common/GCAM_region_names", "energy/calibrated_techs", "L1324.in_EJ_R_indfeed_F_Yh_EUR", "energy/A32.globaltech_eff", "energy/A32.globaltech_shrwt") ->
      L232.StubTechProd_industry_EUR

    L232.StubTechCoef_industry_EUR %>%
      add_title("Calibrated output of industrial sector") %>%
      add_units("Unitless") %>%
      add_comments("Service output values were first aggregated by sector to calculate the portion of each input as coefficients, then the coefficients were interpolated to cover last base year, future years and industry coefficient convergence year") %>%
      add_legacy_name("L232.StubTechCoef_industry_EUR") %>%
      add_precursors("L1326.in_EJ_R_indenergy_F_Yh_EUR", "L123.in_EJ_R_indchp_F_Yh_EUR", "common/GCAM_region_names", "energy/calibrated_techs", "L1324.in_EJ_R_indfeed_F_Yh_EUR", "energy/A32.globaltech_eff", "energy/A32.globaltech_shrwt") ->
      L232.StubTechCoef_industry_EUR

    L232.FuelPrefElast_indenergy_EUR %>%
      add_title("Fuel preference elasticities of industrial energy use") %>%
      add_units("Unitless") %>%
      add_comments("First, calculate the fuel shares allocated to each fuel, then set fuel preference elasticities as indicated by exogenous rules(A32.fuelprefElasticity), lastly  fill out elasticities from the first future model time period") %>%
      add_legacy_name("L232.FuelPrefElast_indenergy_EUR") %>%
      add_precursors("L1326.in_EJ_R_indenergy_F_Yh_EUR", "L123.in_EJ_R_indchp_F_Yh_EUR", "common/GCAM_region_names", "energy/calibrated_techs", "energy/A32.fuelprefElasticity") ->
      L232.FuelPrefElast_indenergy_EUR

    L232.PerCapitaBased_ind_EUR %>%
      add_title("Per-capita based flag for industry final demand") %>%
      add_units("NA") %>%
      add_comments("Extracted per-capita based flag for industry final demand from A32.demand") %>%
      add_legacy_name("L232.PerCapitaBased_ind_EUR") %>%
      add_precursors("common/GCAM_region_names", "energy/A32.demand") ->
      L232.PerCapitaBased_ind_EUR

    L232.PriceElasticity_ind_EUR %>%
      add_title("Price elasticity of industry final demand") %>%
      add_units("Unitless") %>%
      add_comments("Extracted price elasticity of industry final demand from A32.demand") %>%
      add_comments("Price elasticities are only applied to future periods. Application in base years will cause solution failure") %>%
      add_legacy_name("L232.PriceElasticity_ind_EUR") %>%
      add_precursors("common/GCAM_region_names", "energy/A32.demand") ->
      L232.PriceElasticity_ind_EUR

    L232.BaseService_ind_EUR %>%
      add_title("Base-year service output of industry final demand") %>%
      add_units("EJ") %>%
      add_comments("Extracted base-year service output of industry final demand from L232.StubTechProd_industry_EUR") %>%
      add_legacy_name("L232.BaseService_ind_EUR") %>%
      add_precursors("L1326.in_EJ_R_indenergy_F_Yh_EUR", "L123.in_EJ_R_indchp_F_Yh_EUR", "common/GCAM_region_names", "energy/calibrated_techs", "L1324.in_EJ_R_indfeed_F_Yh_EUR", "energy/A32.globaltech_eff", "energy/A32.globaltech_shrwt", "energy/A32.demand") ->
      L232.BaseService_ind_EUR

    L232.StubTechSecOut_ind_EUR %>%
      add_title("Calibrated sec output of elec from cogen") %>%
      add_units("EJ / EJ") %>%
      add_precursors("L123.eff_R_indchp_F_Yh_EUR", "L232.GlobalTechEff_ind", "common/GCAM_region_names") ->
      L232.StubTechSecOut_ind_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
