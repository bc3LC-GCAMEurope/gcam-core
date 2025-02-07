# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L2325.chemical
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for chemical-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2325.SectorLogitTables[[ curr_table ]]$data}, \code{L2325.Supplysector_chemical_EUR}, \code{L2325.FinalEnergyKeyword_chemical_EUR},
#' \code{L2325.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2325.SubsectorLogit_chemical_EUR}, \code{L2325.SubsectorShrwtFllt_chemical_EUR},
#' \code{L2325.SubsectorInterp_chemical_EUR}, \code{L2325.StubTech_chemical_EUR},
#' \code{L2325.StubTechProd_chemical_EUR}, \code{L2325.StubTechCalInput_chemical_EUR},
#' \code{L2325.StubTechCoef_chemical_EUR}, \code{L2325.PerCapitaBased_chemical_EUR}, \code{L2325.BaseService_chemical_EUR}, \code{L2325.PriceElasticity_chemical_EUR},
#' \code{object}. The corresponding file in the
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for chemical sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr gather spread
#' @author Yang Liu Dec 2019
module_gcameurope_L2325.chemical <- function(command, ...) {
  GLOBAL_TECH_COGEN <- c("L2325.GlobalTechShrwt_chemical",
                         "L2325.GlobalTechCoef_chemical",
                         "L2325.GlobalTechEff_chemical",
                         "L2325.GlobalTechCost_chemical",
                         "L2325.GlobalTechTrackCapital_chemical",
                         "L2325.GlobalTechCapture_chemical",
                         "L2325.GlobalTechCSeq_ind",
                         "L2325.GlobalTechShutdown_chemical",
                         "L2325.GlobalTechSCurve_chemical",
                         "L2325.GlobalTechLifetime_chemical",
                         "L2325.GlobalTechProfitShutdown_chemical",
                         "L2325.GlobalTechSecOut_chemical")

  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     FILE = "energy/calibrated_techs",
                     FILE = "energy/A_regions",
                     FILE = "energy/A325.sector",
                     FILE = "energy/A23.chp_elecratio",
                     FILE = "energy/A325.subsector_interp",
                     FILE = "energy/A325.nonenergy_Cseq",
                     FILE = "energy/A325.subsector_logit",
                     FILE = "energy/A325.subsector_shrwt",
                     FILE = "energy/A325.globaltech_coef",
                     FILE = "energy/A325.globaltech_eff",
                     FILE = "energy/A325.globaltech_co2capture",
                     FILE = "energy/A325.globaltech_cost",
                     FILE = "energy/A325.globaltech_retirement",
                     FILE = "energy/A325.globaltech_shrwt",
                     FILE = "energy/A325.demand",
                     FILE = "gcam-europe/mappings/grid_regions",
                     "L1325.in_EJ_R_chemical_F_Y",
                     "L1325.in_EJ_R_chemical_F_Y_EUR",
                     GLOBAL_TECH_COGEN)
  MODULE_OUTPUTS <- c("L2325.Supplysector_chemical_EUR",
                      "L2325.FinalEnergyKeyword_chemical_EUR",
                      "L2325.SubsectorLogit_chemical_EUR",
                      "L2325.SubsectorShrwtFllt_chemical_EUR",
                      "L2325.SubsectorInterp_chemical_EUR",
                      "L2325.StubTech_chemical_EUR",
                      "L2325.StubTechProd_chemical_EUR",
                      "L2325.StubTechCalInput_chemical_EUR",
                      "L2325.StubTechCoef_chemical_EUR",
                      "L2325.StubTechSecMarket_chemical_EUR",
                      "L2325.PerCapitaBased_chemical_EUR",
                      "L2325.BaseService_chemical_EUR",
                      "L2325.PriceElasticity_chemical_EUR",
                      paste0(GLOBAL_TECH_COGEN, "_EUR"))
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)
    GCAM_region_names <- GCAM_region_names %>%  filter_regions_europe(unique(c(grid_regions$region, gcameurope.EUROSTAT_COUNTRIES)))

    L1325.in_EJ_R_chemical_F_Y_EUR <- replace_with_eurostat(L1325.in_EJ_R_chemical_F_Y, L1325.in_EJ_R_chemical_F_Y_EUR) %>%
      filter_regions_europe(GCAM_region_names$region, region_ID_mapping = GCAM_region_names)
    # ===================================================
    # 0. Give binding for variable names used in pipeline
    has_district_heat <- year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost  <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <-
      value.x <- value.y <- parameter <- secondary.output <- efficiency <- elec_ratio <- output.ratio <-
      year.x <- year.y <- output.ratio.x <- output.ratio.y <- sector.name <- subsector.name <-
      calOutputValue.x <- calOutputValue.y <- output_tot <- stub.technology <- market.name <- terminal_coef <-
      share.weight <- interpolation.function <- NULL

    # 0. Delete technologies and subsectors in regions where heat is not modeled as a fuel ===================================================
    has_not_heat <- filter(A_regions, has_district_heat == 0) # intermediate tibble

    calibrated_techs %>%
      filter(grepl("chemical energy", sector) & fuel == "heat") %>%
      select(supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(GCAM_region_ID = has_not_heat[["GCAM_region_ID"]])) %>%
      filter_regions_europe(., region_ID_mapping = GCAM_region_names) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L2325.rm_heat_techs_R # intermediate tibble

    # 1a. Supplysector information ===================================================================
    # L2325.Supplysector_chemical_EUR: Supply sector information for chemical sector
    A325.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2325.Supplysector_chemical_EUR

    # L2325.FinalEnergyKeyword_chemical_EUR: Supply sector keywords for chemical sector
    A325.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L2325.FinalEnergyKeyword_chemical_EUR

    # 1b. Subsector information ===================================================================
    # L2325.SubsectorLogit_chemical_EUR: Subsector logit exponents of chemical sector
    A325.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) %>%
      anti_join(L2325.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2325.SubsectorLogit_chemical_EUR

    # and L2325.SubsectorShrwtFllt_chemical_EUR: Subsector shareweights of chemical sector
    A325.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) %>%
      anti_join(L2325.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2325.SubsectorShrwtFllt_chemical_EUR

    # L2325.SubsectorInterp_chemical_EUR: Subsector shareweight interpolation of chemical sector
    A325.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) %>%
      anti_join(L2325.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2325.SubsectorInterp_chemical_EUR

    # 1c. Technology information ===================================================================
    # L2325.StubTech_chemical_EUR: Identification of stub technologies of chemical
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A325.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      anti_join(L2325.rm_heat_techs_R, by = c("region", "subsector")) %>% # Remove non-existent heat subsectors from each region
      rename(stub.technology = technology) ->
      L2325.StubTech_chemical_EUR

    # L2325.GlobalTechEff_ind: Energy inputs and efficiency of global chemical energy use and feedstocks technologies
    A325.globaltech_eff %>%
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
      L2325.globaltech_eff.long # intermediate tibble

    L2325.globaltech_eff.long %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]]) ->
      L2325.GlobalTechEff_chemical

    # Retirement information
    A325.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L2325.globaltech_retirement_base

    # Copies first future year retirment information into all future years and appends back onto base year
    L2325.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L2325.globaltech_retirement_future

    # filters base years from original and then appends future years
    L2325.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      bind_rows(L2325.globaltech_retirement_future) ->
      L2325.globaltech_retirement

    # 2a. calibrated input ====================================================
    # L2325.StubTechCalInput_indenergy: calibrated input of industrial energy use technologies (including cogen)
    L2325.GlobalTechEff_chemical %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology,
             coefficient = efficiency) %>%
      mutate(efficiency = NULL) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTechCoef"]]), GCAM_region_names) %>%
      mutate(market.name =NULL,coefficient = NULL) %>%
      anti_join(L2325.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2325.chemical_tmp

    L1325.in_EJ_R_chemical_F_Y_EUR %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      complete(nesting(fuel,year,sector),region = GCAM_region_names$region) %>%
      mutate(GCAM_region_ID = NULL,value = replace_na(value,0)) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      left_join(select(calibrated_techs, sector, fuel, supplysector, subsector, technology), by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) ->
      L2325.in_EJ_R_chemical_F_Yh # intermediate tibble

    L2325.in_EJ_R_chemical_F_Yh %>%
      left_join(distinct(select(A325.globaltech_eff, subsector, technology, minicam.energy.input)),
                by = c("subsector", "stub.technology" = "technology")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year) ->
      L2325.StubTechCalInput_chemical_tmp

    L2325.chemical_tmp %>%
      left_join(L2325.StubTechCalInput_chemical_tmp,
                by = c("region", "supplysector", "subsector", "stub.technology", "year", "minicam.energy.input")) %>%
      mutate(fuel = NULL,sector = NULL, value = NULL,GCAM_region_ID  = NULL,calibrated.value = replace_na(calibrated.value,0),
             share.weight.year = year) %>%
      rename(calOutputValue = calibrated.value) %>%  # temporary column name change to accommodate function set_subsector_shrwt
      set_subsector_shrwt %>%
      rename(calibrated.value = calOutputValue) %>% # temporary column name change to accommodate function set_subsector_shrwt
      mutate(tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L2325.StubTechCalInput_chemical_EUR

    # 2b. calibrated output ====================================================
    # L2325.StubTechProd_chemical_EUR: calibrated output of chemical sector
    # First, calculate service output by technology, for energy-use and feedstocks
    L2325.in_EJ_R_chemical_F_Yh %>%
      left_join_error_no_match(select(L2325.globaltech_eff.long, sector.name, subsector.name, technology, year,efficiency),
                               by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
                                      "stub.technology" = "technology", "year")) %>%
      mutate(calOutputValue = round(value * efficiency, energy.DIGITS_CALOUTPUT)) ->
      L2325.out_EJ_R_ind_serv_F_Yh # intermediate tibble

    # intermediate tibble to extract chemical names
    A325.globaltech_shrwt %>%
      filter(supplysector == "chemical") %>%
      select(supplysector, subsector, technology) ->
      L2325.chemical_names

    # Aggregate service output by region. This is the output of the industrial sector in each region.
    L2325.out_EJ_R_ind_serv_F_Yh %>%
      group_by(region, GCAM_region_ID, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup %>%
      mutate(supplysector = L2325.chemical_names[["supplysector"]],
             subsector = L2325.chemical_names[["subsector"]],
             stub.technology = L2325.chemical_names[["technology"]],
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2325.StubTechProd_chemical_EUR

    # 2c. calibrated coef ====================================================
    # L2325.StubTechCoef_chemical_EUR: calibrated output of industrial sector
    # Next, aggregate service output by sector to calculate the portion of each input
    L2325.out_EJ_R_ind_serv_F_Yh %>%
      group_by(region, GCAM_region_ID, supplysector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup %>%
      # explicitly set zeros for any supplysectors that have been dropped
      # otherwise we get non-zero coefficients with zero calOutput for chemical feedstocks
      complete(nesting(region, GCAM_region_ID), supplysector = pull(distinct(L2325.out_EJ_R_ind_serv_F_Yh, supplysector)),
               year = MODEL_BASE_YEARS, fill = list(calOutputValue = 0)) %>%
      left_join_error_no_match(select(L2325.StubTechProd_chemical_EUR, calOutputValue, region, year),
                               by = c("region", "year")) %>%
      rename(calOutputValue = calOutputValue.x,
             output_tot = calOutputValue.y) %>%
      mutate(coefficient = replace_na(calOutputValue / output_tot,0)) %>%
      rename(minicam.energy.input = supplysector) %>%
      mutate(supplysector = L2325.chemical_names[["supplysector"]],
             subsector = L2325.chemical_names[["subsector"]],
             stub.technology = L2325.chemical_names[["technology"]],
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2325.StubTechCoef_chemical_base # intermediate tibble?

    # This set of coefficients covers only the base years; the first "future" period will default to the global tech coefficient
    # Instead, interpolate the coefficients to these global default values in a specified period
    L2325.StubTechCoef_chemical_base %>%
      complete(nesting(region, supplysector, subsector, stub.technology, minicam.energy.input, market.name),
               year = unique(c(MODEL_YEARS, energy.INDCOEF_CONVERGENCE_YR))) %>%
      left_join(select(A325.globaltech_coef, supplysector, subsector, technology, minicam.energy.input, terminal_coef),
                by = c("supplysector", "subsector", stub.technology = "technology", "minicam.energy.input")) %>%
      mutate(coefficient = if_else(year == energy.INDCOEF_CONVERGENCE_YR, terminal_coef, coefficient)) %>%
      select(-terminal_coef) %>%
      group_by(region, supplysector, subsector, stub.technology, minicam.energy.input) %>%
      mutate(coefficient = round(approx_fun(year, coefficient,rule = 2), energy.DIGITS_COEFFICIENT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) ->   # drop the terminal coef year if it's outside of the model years
      L2325.StubTechCoef_chemical_EUR

    # 2d. stubtech market ====================================================
    L2325.StubTechSecMarket_chemical_EUR  <- L2325.StubTech_chemical_EUR %>%
      filter(grepl("cogen", stub.technology)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join(grid_regions, by = "region") %>%
      mutate(market.name = if_else(is.na(grid_region), region, grid_region),
             secondary.output = "electricity") %>%
      select(LEVEL2_DATA_NAMES[["StubTechSecMarket"]])

    # 3. Demand ====================================================
    # L2325.PerCapitaBased_chemical_EUR: per-capita based flag for chemical exports final demand
    A325.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]], GCAM_region_names)  ->
      L2325.PerCapitaBased_chemical_EUR


    # L2325.BaseService_chemical_EUR: base-year service output of chemical
    L2325.StubTechProd_chemical_EUR %>%
      select(region, year, supplysector,base.service = calOutputValue) %>%
      rename(energy.final.demand = supplysector) %>%
      group_by(region,year,energy.final.demand) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup()  ->
      L2325.BaseService_chemical_EUR

    #For regions with 0 in base year, modify Subsector shareweight and interpolation
    L2325.out_EJ_R_ind_serv_F_Yh %>%
      group_by(region,supplysector, GCAM_region_ID, year) %>%
      summarise(value = sum(calOutputValue)) %>%
      ungroup %>%
      select(region, year, supplysector,value) %>%
      filter(value == 0, year == MODEL_FINAL_BASE_YEAR)  ->
      nobaseyear

    L2325.SubsectorShrwtFllt_chemical_EUR %>%
      left_join(nobaseyear, by = c("region", "supplysector")) %>%
      mutate(value = replace_na(value,1),share.weight = if_else(value ==0,0.5,share.weight),year = NULL,value = NULL) ->
      L2325.SubsectorShrwtFllt_chemical_EUR


    L2325.SubsectorInterp_chemical_EUR %>%
      left_join(nobaseyear, by = c("region", "supplysector")) %>%
      mutate(value = replace_na(value,1),interpolation.function = if_else(value ==0,"linear",interpolation.function),year = NULL,value = NULL) ->
      L2325.SubsectorInterp_chemical_EUR

    # L2325.PriceElasticity_chemical_EUR: price elasticity
    A325.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PriceElasticity"]][LEVEL2_DATA_NAMES[["PriceElasticity"]] != "year"], GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["PriceElasticity"]]) ->
      L2325.PriceElasticity_chemical_EUR

    # COGEN RENAMING ---------------------
    # Create global tech for grid region specific cogen
    env_module <- rlang::current_env()

    lapply(GLOBAL_TECH_COGEN, cogen_global_tech, env = env_module)
    L2325.GlobalTechSecOut_chemical <- L2325.GlobalTechSecOut_chemical %>%
      mutate(secondary.output = "base load generation")

    env_module <- rlang::current_env()

    lapply(MODULE_OUTPUTS, cogen_stubtech_rename, env = env_module,
           grid_region_df = grid_regions)
    # Produce outputs ===================================================
    L2325.Supplysector_chemical_EUR %>%
      add_title("Supply sector information for chemical sector") %>%
      add_units("NA") %>%
      add_comments("For chemical sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A325.sector is expended into all GCAM regions") %>%
      add_legacy_name("L2325.Supplysector_chemical_EUR") %>%
      add_precursors("energy/A325.sector", "common/GCAM_region_names") ->
      L2325.Supplysector_chemical_EUR

    L2325.FinalEnergyKeyword_chemical_EUR %>%
      add_title("Supply sector keywords for chemical sector") %>%
      add_units("NA") %>%
      add_comments("For chemical sector, the supply sector final energy keywords from A325.sector are expended into all GCAM regions") %>%
      add_legacy_name("L2325.FinalEnergyKeyword_chemical_EUR") %>%
      add_precursors("energy/A325.sector", "common/GCAM_region_names") ->
      L2325.FinalEnergyKeyword_chemical_EUR

    L2325.SubsectorLogit_chemical_EUR %>%
      add_title("Subsector logit exponents of chemical sector") %>%
      add_units("Unitless") %>%
      add_comments("For chemical sector, the subsector logit exponents from A325.subsector_logit are expanded into all GCAM regions") %>%
      add_legacy_name("L2325.SubsectorLogit_chemical_EUR") %>%
      add_precursors("energy/A325.subsector_logit", "energy/A_regions","common/GCAM_region_names") ->
      L2325.SubsectorLogit_chemical_EUR

    L2325.SubsectorShrwtFllt_chemical_EUR %>%
      add_title("Subsector shareweights of chemical sector") %>%
      add_units("unitless") %>%
      add_comments("For chemical sector, the subsector shareweights from A325.subsector_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2325.SubsectorShrwtFllt_chemical_EUR") %>%
      add_precursors("energy/A325.subsector_shrwt", "energy/A_regions","common/GCAM_region_names") ->
      L2325.SubsectorShrwtFllt_chemical_EUR

    L2325.SubsectorInterp_chemical_EUR %>%
      add_title("Subsector shareweight interpolation of chemical sector") %>%
      add_units("NA") %>%
      add_comments("For chemical sector, the subsector shareweight interpolation function infromation from A325.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L2325.SubsectorInterp_chemical_EUR") %>%
      add_precursors("energy/A325.subsector_interp", "energy/A_regions", "common/GCAM_region_names") ->
      L2325.SubsectorInterp_chemical_EUR

    L2325.StubTech_chemical_EUR %>%
      add_title("Identification of stub technologies of chemical") %>%
      add_units("NA") %>%
      add_comments("For chemical sector, the stub technologies from A325.globaltech_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2325.StubTech_chemical_EUR") %>%
      add_precursors("energy/A325.globaltech_shrwt", "energy/A_regions","common/GCAM_region_names") ->
      L2325.StubTech_chemical_EUR

    L2325.StubTechProd_chemical_EUR %>%
      add_title("calibrated chemical production") %>%
      add_units("EJ") %>%
      add_comments("Values are calculated using L1325.in_EJ_R_chemical_F_Y_EUR, then added GCAM region information and supplysector, subsector, and technology information") %>%
      add_legacy_name("L2325.StubTechProd_chemical_EUR") %>%
      add_precursors("energy/calibrated_techs",  "common/GCAM_region_names") ->
      L2325.StubTechProd_chemical_EUR

    L2325.StubTechCalInput_chemical_EUR %>%
      add_title("calibrated chemical production") %>%
      add_units("EJ") %>%
      add_comments("Values are calculated using L1325.in_EJ_R_chemical_F_Y_EUR then added GCAM region information and supplysector, subsector, technology, and input information") %>%
      add_legacy_name("L2325.StubTechCalInput_chemical_EUR") %>%
      add_precursors("energy/calibrated_techs", "L1325.in_EJ_R_chemical_F_Y_EUR", "common/GCAM_region_names") ->
      L2325.StubTechCalInput_chemical_EUR

    L2325.StubTechCoef_chemical_EUR %>%
      add_title("region-specific coefficients of chemical production technologies") %>%
      add_units("unitless") %>%
      add_comments("Coefficients from literature") %>%
      add_legacy_name("L2325.StubTechCoef_chemical_EUR") %>%
      add_precursors("energy/calibrated_techs", "common/GCAM_region_names","energy/A325.globaltech_coef") ->
      L2325.StubTechCoef_chemical_EUR

    L2325.StubTechSecMarket_chemical_EUR  %>%
      add_title("Market name for secondary output") %>%
      add_units("unitless") %>%
      add_precursors("gcam-europe/mappings/grid_regions") ->
      L2325.StubTechSecMarket_chemical_EUR

    L2325.PerCapitaBased_chemical_EUR %>%
      add_title("per-capita based flag for chemical exports final demand") %>%
      add_units("NA") %>%
      add_comments("Per-capita based flags for chemical from A325.demand are expanded into all GCAM regions") %>%
      add_legacy_name("L2325.PerCapitaBased_chemical_EUR") %>%
      add_precursors("energy/A325.demand", "common/GCAM_region_names") ->
      L2325.PerCapitaBased_chemical_EUR

    L2325.BaseService_chemical_EUR %>%
      add_title("base-year service output of chemical") %>%
      add_units("EJ") %>%
      add_comments("Transformed from L2325.StubTechProd_chemical_EUR by adding energy.final.demand") %>%
      add_legacy_name("L2325.BaseService_chemical_EUR") %>%
      add_precursors("energy/A325.demand","L1325.in_EJ_R_chemical_F_Y_EUR", "energy/calibrated_techs", "common/GCAM_region_names") ->
      L2325.BaseService_chemical_EUR

    L2325.PriceElasticity_chemical_EUR %>%
      add_title("price elasticity for chemical") %>%
      add_units("Unitless") %>%
      add_comments("The elasticity values from A325.demand are expanded into all GCAM_regions") %>%
      add_legacy_name("L2325.PriceElasticity_chemical_EUR") %>%
      add_precursors("energy/A325.demand", "common/GCAM_region_names") ->
      L2325.PriceElasticity_chemical_EUR

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
