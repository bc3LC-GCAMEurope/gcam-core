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
#' the generated outputs:  \code{L2322.Supplysector_Fert_EUR}, \code{L2322.FinalEnergyKeyword_Fert_EUR},
#'  \code{L2322.SubsectorLogit_Fert_EUR}, \code{L2322.SubsectorShrwtFllt_Fert_EUR}, \code{L2322.SubsectorInterp_Fert_EUR},
#'  \code{L2322.StubTech_Fert_EUR}, \code{L2322.StubTechProd_Fert_EUR}, \code{L2322.StubTechCoef_Fert_EUR},
#'  \code{L2322.StubTechFixOut_Fert_imp_EUR}, \code{L2322.StubTechFixOut_Fert_exp_EUR}, \code{L2322.PerCapitaBased_Fert_EUR},
#'  \code{L2322.BaseService_Fert_EUR}. The corresponding file in the
#' original data system was \code{L2322.Fert.R} (energy level2).
#' @details This chunk provides supply sector information/keywords, subsector shareweights, global technology lifetime,
#' energy inputs and coefficients, global fertilizer manufacturing technologies, etc. for the fertilizer sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select
#' @importFrom tidyr complete nesting
#' @author LF September 2017
module_gcameurope_L2322.Fert <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A322.sector",
             FILE = "energy/A322.subsector_interp",
             FILE = "energy/A322.subsector_logit",
             FILE = "energy/A322.subsector_shrwt",
             FILE = "energy/A322.globaltech_coef",
             FILE = "energy/A322.globaltech_shrwt",
             FILE = "energy/A322.globaltech_co2capture",
             FILE = "energy/A322.globaltech_renew",
             FILE = "energy/A322.globaltech_retirement",
             "L1322.Fert_Prod_MtN_R_F_Y_EUR",
             "L1322.IO_R_Fert_F_Yh_EUR",
             "L1322.Fert_NEcost_75USDkgN_F_EUR",
             "L142.ag_Fert_NetExp_MtN_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2322.Supplysector_Fert_EUR",
             "L2322.FinalEnergyKeyword_Fert_EUR",
             "L2322.SubsectorLogit_Fert_EUR",
             "L2322.SubsectorShrwtFllt_Fert_EUR",
             "L2322.SubsectorInterp_Fert_EUR",
             "L2322.StubTech_Fert_EUR",
             "L2322.StubTechProd_Fert_EUR",
             "L2322.StubTechCoef_Fert_EUR",
             "L2322.StubTechFixOut_Fert_imp_EUR",
             "L2322.StubTechFixOut_Fert_exp_EUR",
             "L2322.PerCapitaBased_Fert_EUR",
             "L2322.BaseService_Fert_EUR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names") %>% filter_regions_europe()
    gcameurope.EUROSTAT_GCAMREGIONID <- GCAM_region_names %>%
      filter(region %in% gcameurope.EUROSTAT_COUNTRIES) %>%
      distinct()
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A322.sector <- get_data(all_data, "energy/A322.sector", strip_attributes = TRUE)
    A322.subsector_interp <- get_data(all_data, "energy/A322.subsector_interp", strip_attributes = TRUE)
    A322.subsector_logit <- get_data(all_data, "energy/A322.subsector_logit", strip_attributes = TRUE)
    A322.subsector_shrwt <- get_data(all_data, "energy/A322.subsector_shrwt", strip_attributes = TRUE)
    A322.globaltech_coef <- get_data(all_data, "energy/A322.globaltech_coef")
    A322.globaltech_shrwt <- get_data(all_data, "energy/A322.globaltech_shrwt", strip_attributes = TRUE)
    A322.globaltech_co2capture <- get_data(all_data, "energy/A322.globaltech_co2capture")
    A322.globaltech_renew <- get_data(all_data, "energy/A322.globaltech_renew")
    A322.globaltech_retirement <- get_data(all_data, "energy/A322.globaltech_retirement", strip_attributes = TRUE)
    L1322.Fert_Prod_MtN_R_F_Y_EUR <- get_data(all_data, "L1322.Fert_Prod_MtN_R_F_Y_EUR", strip_attributes = TRUE)
    L1322.IO_R_Fert_F_Yh_EUR <- get_data(all_data, "L1322.IO_R_Fert_F_Yh_EUR", strip_attributes = TRUE)
    L1322.Fert_NEcost_75USDkgN_F_EUR <- get_data(all_data, "L1322.Fert_NEcost_75USDkgN_F_EUR")
    L142.ag_Fert_NetExp_MtN_R_Y <- get_data(all_data, "L142.ag_Fert_NetExp_MtN_R_Y", strip_attributes = TRUE) %>%
      filter_regions_europe(region_ID_mapping = gcameurope.EUROSTAT_GCAMREGIONID)

    # ===================================================
    # 0. Give binding for variable names used in pipeline

    year.fillout <- to.value <- technology <- year <-
      share.weight <- supplysector <- subsector <- coefficient <- minicam.energy.input <-
      NEcost_75USDkgN <- input.cost <- remove.fraction <- half.life <- median.shutdown.point <-
      value <- calOutputValue <- sector <- fuel <- subs.share.weight <- region <- fixedOutput <- . <- NULL

    # ===================================================
    # 1. Perform computations
    # Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
    # 1a. Supplysector information
    # L2322.Supplysector_Fert_EUR: Supply sector information for fertilizer sector
    A322.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2322.Supplysector_Fert_EUR

    # L2322.FinalEnergyKeyword_Fert_EUR: Supply sector keywords for fertilizer sector
    A322.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L2322.FinalEnergyKeyword_Fert_EUR

    # 2b. Subsector information
    # L2322.SubsectorLogit_Fert_EUR: Subsector logit exponents of fertilizer sector
    A322.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2322.SubsectorLogit_Fert_EUR

    # L2322.SubsectorShrwtFllt_Fert_EUR: Subsector shareweights of fertilizer sector
    A322.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) ->
      L2322.SubsectorShrwtFllt_Fert_EUR

    # L2322.SubsectorInterp_Fert_EUR: Subsector shareweight interpolation of fertilizer sector
    A322.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) ->
      L2322.SubsectorInterp_Fert_EUR

    # 2c. Technology information
    # L2322.StubTech_Fert_EUR: Identification of stub technologies of fertilizer sector
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A322.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L2322.StubTech_Fert_EUR

    # Retirement information
    A322.globaltech_retirement %>%
      set_years %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      A322.globaltech_retirement_with_years

    # Copy the data in the last base year period through to the end year
    A322.globaltech_retirement_with_years %>%
      filter(year == max(MODEL_BASE_YEARS)) ->
      A322.globaltech_retirement_max_baseyear

    A322.globaltech_retirement_with_years %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(A322.globaltech_retirement_max_baseyear) ->
      L2322.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # All of these options have different headers, and all are allowed

    # Calibration and region-specific data
    # L2322.StubTechProd_Fert_EUR: calibrated output of fertilizer technologies
    L1322.Fert_Prod_MtN_R_F_Y_EUR %>%
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
      L2322.StubTechProd_Fert_EUR

    # L2322.StubTechCoef_Fert_EUR: calibrated base-year coefficients of fertilizer production technologies
    L1322.IO_R_Fert_F_Yh_EUR %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(coefficient = value) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      filter(coefficient != 0) %>% # Where 0, drop from this table (to revert to assumed defaults)
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector, technology, minicam.energy.input), by = c("sector", "fuel")) %>%
      mutate(stub.technology = technology, market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2322.StubTechCoef_Fert_EUR

    # L2322.StubTechFixOut_Fert_imp_EUR: fixed output of import technology (fixed imports)
    # Imports are negative net exports
    L142.ag_Fert_NetExp_MtN_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(fixedOutput = value) %>%
      left_join_error_no_match(GCAM_region_names, by = 'GCAM_region_ID') %>%
      mutate(supplysector = A322.globaltech_renew[["supplysector"]],
             subsector = A322.globaltech_renew[["subsector"]],
             stub.technology = A322.globaltech_renew[["technology"]],
             fixedOutput = round(pmax(0, -1 * fixedOutput), energy.DIGITS_CALOUTPUT),
             share.weight.year = year, subs.share.weight = 0, tech.share.weight = 0) %>%
      bind_rows(repeat_add_columns(select(filter(., year == max(MODEL_BASE_YEARS)), -year), tibble(year = MODEL_FUTURE_YEARS))) %>%
      select(LEVEL2_DATA_NAMES[["StubTechFixOut"]]) -> # Repeat final year to all future years and rbind
      L2322.StubTechFixOut_Fert_imp_EUR

    # L2322.StubTechFixOut_Fert_exp_EUR: fixed output of import technology (fixed imports)
    # Exports are positive net exports

    # Need to do this initial filter and extract outside of main pipeline (at least for dplyr 0.5)
    exports <- filter(A322.globaltech_shrwt, grepl("Exports", supplysector))
    sups <- exports[["supplysector"]]
    subs <- exports[["subsector"]]
    techs <- exports[["technology"]]

    L142.ag_Fert_NetExp_MtN_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(fixedOutput = value) %>%
      mutate(fixedOutput = round(fixedOutput, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(supplysector = sups,
             subsector = subs,
             stub.technology = techs,
             fixedOutput = pmax(0, fixedOutput),
             share.weight.year = year,
             subs.share.weight = 0,
             tech.share.weight = 0) ->
      L2322.StubTechFixOut_Fert_exp_EUR_base

    L2322.StubTechFixOut_Fert_exp_EUR_base %>%
      bind_rows(repeat_add_columns(select(filter(., year == max(MODEL_BASE_YEARS)), -year), tibble(year = MODEL_FUTURE_YEARS))) %>% # Repeat final year to all future years and rbind
      select(LEVEL2_DATA_NAMES[["StubTechFixOut"]]) ->
      L2322.StubTechFixOut_Fert_exp_EUR

    # L2322.PerCapitaBased_Fert_EUR: per-capita based flag for fertilizer exports final demand
    tibble(region = GCAM_region_names[["region"]]) %>%
      mutate(energy.final.demand = sups,
             perCapitaBased = 0) ->
      L2322.PerCapitaBased_Fert_EUR

    # L2322.BaseService_Fert_EUR: base-year service output of fertilizer exports final demand
    # Base service is equal to the output of the exports supplysector
    tibble(region = L2322.StubTechFixOut_Fert_exp_EUR_base[["region"]]) %>%
      mutate(energy.final.demand = L2322.StubTechFixOut_Fert_exp_EUR_base[["supplysector"]],
             year = L2322.StubTechFixOut_Fert_exp_EUR_base[["year"]],
             base.service = L2322.StubTechFixOut_Fert_exp_EUR_base[["fixedOutput"]]) ->
      L2322.BaseService_Fert_EUR

    # ===================================================
    # Produce outputs

    L2322.Supplysector_Fert_EUR %>%
      add_title("Supply sector information for fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A322.sector is expended into all GCAM regions") %>%
      add_legacy_name("L2322.Supplysector_Fert_EUR") %>%
      add_precursors("common/GCAM_region_names", "energy/A322.sector") ->
      L2322.Supplysector_Fert_EUR

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

    L2322.StubTechProd_Fert_EUR %>%
      add_title("calibrated output of fertilizer technologies") %>%
      add_units("Mt N") %>%
      add_comments("Values are calculated using L1322.Fert_Prod_MtN_R_F_Y_EUR then added GCAM region information") %>%
      add_legacy_name("L2322.StubTechProd_Fert_EUR") %>%
      add_precursors("L1322.Fert_Prod_MtN_R_F_Y_EUR", "common/GCAM_region_names", "energy/calibrated_techs") ->
      L2322.StubTechProd_Fert_EUR

    L2322.StubTechCoef_Fert_EUR %>%
      add_title("calibrated base-year coefficients of fertilizer production technologies") %>%
      add_units("Unitless") %>%
      add_comments("Coefficients are calculated using L1322.IO_R_Fert_F_Yh_EUR") %>%
      add_legacy_name("L2322.StubTechCoef_Fert_EUR") %>%
      add_precursors("L1322.IO_R_Fert_F_Yh_EUR", "common/GCAM_region_names", "energy/calibrated_techs") ->
      L2322.StubTechCoef_Fert_EUR

    L2322.StubTechFixOut_Fert_imp_EUR %>%
      add_title("fixed output of import technology (fixed imports)") %>%
      add_units("Mt N") %>%
      add_comments("The fixed import values are calculated using L142.ag_Fert_NetExp_MtN_R_Y then added GCAM region information") %>%
      add_legacy_name("L2322.StubTechFixOut_Fert_imp_EUR") %>%
      add_precursors("L142.ag_Fert_NetExp_MtN_R_Y", "common/GCAM_region_names", "energy/A322.globaltech_renew") ->
      L2322.StubTechFixOut_Fert_imp_EUR

    L2322.StubTechFixOut_Fert_exp_EUR %>%
      add_title("fixed output of import technology (fixed imports)") %>%
      add_units("Mt N") %>%
      add_comments("The fixed export values are alculated using L142.ag_Fert_NetExp_MtN_R_Y") %>%
      add_legacy_name("L2322.StubTechFixOut_Fert_exp_EUR") %>%
      add_precursors("L142.ag_Fert_NetExp_MtN_R_Y", "common/GCAM_region_names", "energy/A322.globaltech_shrwt") ->
      L2322.StubTechFixOut_Fert_exp_EUR

    L2322.PerCapitaBased_Fert_EUR %>%
      add_title("per-capita based flag for fertilizer exports final demand") %>%
      add_units("Unitless") %>%
      add_comments("Per Capita Based value are assigned as 0 for all GCAM regions for Exports_fertilizer of energy final demand") %>%
      add_legacy_name("L2322.PerCapitaBased_Fert_EUR") %>%
      add_precursors("common/GCAM_region_names", "energy/A322.globaltech_shrwt") ->
      L2322.PerCapitaBased_Fert_EUR

    L2322.BaseService_Fert_EUR %>%
      add_title("base-year service output of fertilizer exports final demand") %>%
      add_units("Mt N") %>%
      add_comments("Base service values are extracted from L2322.StubTechFixOut_Fert_exp_EUR_base") %>%
      add_legacy_name("L2322.BaseService_Fert_EUR") %>%
      add_precursors("L142.ag_Fert_NetExp_MtN_R_Y", "common/GCAM_region_names", "energy/A322.globaltech_shrwt") ->
      L2322.BaseService_Fert_EUR

    return_data(L2322.Supplysector_Fert_EUR, L2322.FinalEnergyKeyword_Fert_EUR, L2322.SubsectorLogit_Fert_EUR,
                L2322.SubsectorShrwtFllt_Fert_EUR, L2322.SubsectorInterp_Fert_EUR, L2322.BaseService_Fert_EUR,
                L2322.StubTech_Fert_EUR, L2322.StubTechProd_Fert_EUR, L2322.StubTechCoef_Fert_EUR,
                L2322.StubTechFixOut_Fert_imp_EUR, L2322.StubTechFixOut_Fert_exp_EUR, L2322.PerCapitaBased_Fert_EUR)
  } else {
    stop("Unknown command")
  }
}
