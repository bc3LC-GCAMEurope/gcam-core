# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L224.heat
#'
#' Write district heat sector outputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L224.SectorLogitTables[[ curr_table ]]$data}, \code{L224.Supplysector_heat}, \code{L224.SubsectorLogitTables[[ curr_table ]]$data}, \code{L224.SubsectorLogit_heat}, \code{L224.SubsectorShrwt_heat}, \code{L224.SubsectorShrwtFllt_heat}, \code{L224.SubsectorInterp_heat}, \code{L224.SubsectorInterpTo_heat}, \code{L224.StubTech_heat}, \code{L224.StubTechCalInput_heat_EUR}, \code{L224.StubTechSecOut_elec_EUR}, \code{L224.StubTechCost_elec_EUR}.
#' @details This chunk creates level 2 output files for district heat sector. It creates supply sector information,
#' subsector logit exponents, subsector shareweight and interpolation, and stubtech info by writing assumption file
#' information to all model periods and regions that have district heat. It creates global tech coef, costs, and shareweights
#' by interpolating assumptions. From the level 1 heat data, this chunk computes stub tech calibrated inputs, secondary
#' outputs from elec and modified costs.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate select
#' @author RH February 2024
module_gcameurope_L224.heat <- function(command, ...) {
  OUTPUTS_TO_COPY_FILTER <- c("L224.Supplysector_heat",
                              "L224.SubsectorLogit_heat",
                              "L224.SubsectorShrwt_heat",
                              "L224.SubsectorShrwtFllt_heat",
                              "L224.SubsectorInterp_heat",
                              "L224.SubsectorInterpTo_heat",
                              "L224.StubTech_heat")

  MODULE_INPUTS <- c("L101.GCAM_EUR_regions",
                     FILE = "energy/calibrated_techs",
                     FILE = "energy/A_regions",
                     FILE = "energy/A24.sector",
                     FILE = "gcam-europe/A23.elecS_naming",
                     "L1231.eff_R_elec_F_tech_Yh_EUR",
                     "L124.in_EJ_R_heat_F_Yh_EUR",
                     "L124.out_EJ_R_heat_F_Yh_EUR",
                     "L124.out_EJ_R_heat_F_Yh",
                     "L124.coef_R_heat_F_Yh_EUR",
                     "L124.heatoutratio_R_elec_F_tech_Yh_EUR",
                     "L1231.eff_R_elec_F_tech_Yh",
                     "L124.in_EJ_R_heat_F_Yh",
                     "L124.heatoutratio_R_elec_F_tech_Yh",
                     "L124.out_EJ_R_heatfromelec_F_Yh_EUR",
                     "L124.out_EJ_R_heatfromelec_F_Yh",
                     "L2235.StubTech_elecS_cool_EUR",
                     OUTPUTS_TO_COPY_FILTER)

  MODULE_OUTPUTS <- c("L224.StubTechCalInput_heat_EUR",
                      "L224.StubTechCoef_heat_EUR",
                      "L224.StubTechCalOutput_heat_EUR",
                      "L224.StubTechFixOutput_heat_EUR",
                      "L224.GlobalTechShrwt_heat_EUR",
                      paste0(OUTPUTS_TO_COPY_FILTER, "_EUR"))
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # Silence package checks
    has_district_heat <- region <- year.fillout <- to.value <- efficiency <-
      technology <- coef <- subsector <- supplysector <- minicam.energy.input <-
      input.cost <- minicam.non.energy.input <- share.weight <- sector <- fuel <-
      value <- subs.share.weight <- calibrated.value <- secondary.output <-
      stub.technology <- cost_modifier <- year <- output.ratio <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # regional adjustments -------------------
    GCAM_region_names_Eurostat <- L101.GCAM_EUR_regions %>%
      distinct(GCAM_region_ID, region = GCAMEU_region)

    # add any regions in segments, but not in Eurostat (Switzerland)
    GCAM_region_names <- L2235.StubTech_elecS_cool_EUR %>%
      anti_join(GCAM_region_names_Eurostat, by = "region") %>%
      distinct(region) %>%
      left_join_error_no_match(A_regions %>% distinct(GCAM_region_ID, region), by = "region") %>%
      bind_rows(GCAM_region_names_Eurostat)

    # Create outputs that are simply copied from main scripts and filtered to Eurostat regions
    copy_filter_europe(all_data, OUTPUTS_TO_COPY_FILTER, regions_to_keep = GCAM_region_names$region)

    # Some regions missing, add them in here
    for (df_nm in paste0(OUTPUTS_TO_COPY_FILTER, "_EUR")){
      df <- get(df_nm)
      if ("region" %in% names(df)){
        missing_regions <- GCAM_region_names %>%
          distinct(region) %>%
          anti_join(df, by = "region")

        df_no_region <- df %>%
          select(-region) %>%
          distinct()

        assign(df_nm,
               missing_regions %>% repeat_add_columns(df_no_region) %>% bind_rows(df))
      }
    }

    # Add in segment regions not in Eurostat
    L1231.eff_R_elec_F_tech_Yh_EUR <- replace_with_eurostat(L1231.eff_R_elec_F_tech_Yh, L1231.eff_R_elec_F_tech_Yh_EUR) %>%
      filter_regions_europe(regions_to_keep_name = GCAM_region_names$region, region_ID_mapping = GCAM_region_names)

    L124.in_EJ_R_heat_F_Yh_EUR <- replace_with_eurostat(L124.in_EJ_R_heat_F_Yh, L124.in_EJ_R_heat_F_Yh_EUR) %>%
      filter_regions_europe(regions_to_keep_name = GCAM_region_names$region, region_ID_mapping = GCAM_region_names)

    L124.out_EJ_R_heat_F_Yh_EUR  <- replace_with_eurostat(L124.out_EJ_R_heat_F_Yh, L124.out_EJ_R_heat_F_Yh_EUR) %>%
      filter_regions_europe(regions_to_keep_name = GCAM_region_names$region, region_ID_mapping = GCAM_region_names)

    L124.heatoutratio_R_elec_F_tech_Yh_EUR <- replace_with_eurostat(L124.heatoutratio_R_elec_F_tech_Yh, L124.heatoutratio_R_elec_F_tech_Yh_EUR) %>%
      filter_regions_europe(regions_to_keep_name = GCAM_region_names$region, region_ID_mapping = GCAM_region_names)

    # L224.StubTechCalInput_heat_EUR ------------
    L124.in_EJ_R_heat_F_Yh_EUR %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(calibrated_techs %>%
                  select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
                  distinct, by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], "minicam.energy.input", "value") %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             year.share.weight = year,
             subs.share.weight = if_else(calibrated.value == 0, 0, 1),
             share.weight = subs.share.weight) %>%
      select(-value) -> L224.StubTechCalInput_heat_EUR

    # L224.StubTechCoef_heat_EUR ------------
    L224.StubTechCoef_heat_EUR <- L124.coef_R_heat_F_Yh_EUR %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(calibrated_techs %>%
                  select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
                  distinct, by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(market.name = region) %>%
      select(LEVEL2_DATA_NAMES$StubTechCoef) %>%
      complete(year = MODEL_YEARS, nesting(region, supplysector, subsector,
                                           stub.technology, minicam.energy.input, market.name)) %>%
      group_by(region, supplysector, subsector, stub.technology, minicam.energy.input, market.name) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 2)) %>%
      ungroup

    # L224.StubTechCalOutput_heat_EUR -----------------------
    L224.StubTechCalOutput_heat_EUR <- L124.out_EJ_R_heat_F_Yh_EUR %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(calibrated_techs %>%
                  select(sector, fuel, supplysector, subsector, technology) %>%
                  distinct, by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue == 0, 0, 1),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]])

    # L224.StubTechFixOutput_heat_EUR -----------------------
    # Rather than use secondary output for electricity, we are going to use a fixed output
    # In the core, there is a secondary output from electricity in historical years, but not in future years
    # Then there is a huge increase in district heat output in future years, which is not realistic.
    # However we can't even do this because the electricity structure doesn't allow us to have a sec-output only in historical years for the last vintage
    # So we are going to use a fixed output, which stays in the future, also preventing massive increases in district heat output in the first modelled year
    L224.StubTechFixOutput_heat_EUR <- replace_with_eurostat(L124.out_EJ_R_heatfromelec_F_Yh, L124.out_EJ_R_heatfromelec_F_Yh_EUR) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(region %in% L224.StubTech_heat_EUR$region) %>%
      # First calculate total heat output from electricity
      group_by(region, year) %>%
      summarise(fixedOutput = sum(value)) %>%
      ungroup %>%
      # assign to a new electricity subsector
      mutate(supplysector = "district heat",
             subsector = "electricity",
             stub.technology = "electricity") %>%
      # need to repeat last year in future
      complete(year = MODEL_YEARS, nesting(region, supplysector, subsector, stub.technology)) %>%
      group_by(region, supplysector, subsector, stub.technology) %>%
      mutate(fixedOutput = if_else(year > MODEL_FINAL_BASE_YEAR, fixedOutput[year == MODEL_FINAL_BASE_YEAR], fixedOutput)) %>%
      ungroup %>%
      mutate(share.weight.year = year,
             subs.share.weight = 0,
             tech.share.weight = 0)

    # Adjust L224.SubsectorLogit_heat_EUR  to include new subsectors
    L224.SubsectorLogit_heat_EUR <- L224.StubTechFixOutput_heat_EUR %>%
      distinct(region, supplysector, subsector) %>%
      mutate(logit.year.fillout = 1975,
             logit.exponent = -6) %>%
      bind_rows(L224.SubsectorLogit_heat_EUR)

    # also need global tech entry for new tech
    L224.GlobalTechShrwt_heat_EUR <- L224.StubTechFixOutput_heat_EUR %>%
      distinct(sector.name = supplysector, subsector.name = subsector, technology = stub.technology, year) %>%
      mutate(share.weight = 0)

    # Produce outputs ===================================================
    L224.StubTechCalInput_heat_EUR %>%
      add_title("Calibrated input to district heat") %>%
      add_units("EJ/yr") %>%
      add_comments("L124.in_EJ_R_heat_F_Yh_EUR and calibrated_techs are joined, shareweights assigned") %>%
      add_comments("as 0 if the calibrated value is 0 and 1 if it is not 0") %>%
      add_precursors("L124.in_EJ_R_heat_F_Yh_EUR", "energy/calibrated_techs", "energy/A_regions", "common/GCAM_region_names") ->
      L224.StubTechCalInput_heat_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
