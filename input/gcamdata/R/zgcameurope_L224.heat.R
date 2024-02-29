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
                     "L1231.eff_R_elec_F_tech_Yh_EUR",
                     "L124.in_EJ_R_heat_F_Yh_EUR",
                     "L124.heatoutratio_R_elec_F_tech_Yh_EUR",
                     OUTPUTS_TO_COPY_FILTER)

  MODULE_OUTPUTS <- c("L224.StubTechCalInput_heat_EUR",
                      "L224.StubTechSecOut_elec_EUR",
                      "L224.StubTechCost_elec_EUR",
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
    GCAM_region_names <- L101.GCAM_EUR_regions %>%
      distinct(GCAM_region_ID, region = GCAMEU_region)

    # Create outputs that are simply copied from main scripts and filtered to Eurostat regions
    copy_filter_europe(all_data, OUTPUTS_TO_COPY_FILTER)


    # L224.StubTechCalInput_heat_EUR ------------
    # Create list of regions with district heat modeled
    A_regions %>%
      filter(has_district_heat == 1) %>%
      select(region) -> heat_region

    L124.in_EJ_R_heat_F_Yh_EUR %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(calibrated_techs %>%
                  select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
                  distinct, by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) %>%
      filter(year %in% MODEL_BASE_YEARS,
             region %in% heat_region$region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], "minicam.energy.input", "value") %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             year.share.weight = year,
             subs.share.weight = if_else(calibrated.value == 0, 0, 1),
             share.weight = subs.share.weight) %>%
      select(-value) -> L224.StubTechCalInput_heat_EUR

    # L224.StubTechSecOut_elec_EUR ------------
    # Secondary output of heat, applied to electricity generation technologies
    # NOTE: This is complicated. Initially tried using historical information for all model periods that fall within historical time
    # (i.e. not just the model base years). However for regions like the FSU where historical periods often have very low output of heat
    # from the district heat sector, and most heat as a secondary output from the electricity sector, the secondary output heat can easily
    # exceed the demands from the end-use sectors, causing model solution failure. For this reason, the convention applied here is to
    # use the secondary output of heat from the power sector only in the model base years.
    L124.heatoutratio_R_elec_F_tech_Yh_EUR %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(calibrated_techs %>%
                  select(sector, fuel, supplysector, subsector, technology) %>%
                  distinct, by = c("sector", "fuel", "technology")) %>%
      mutate(stub.technology = technology,
             secondary.output = A24.sector[["supplysector"]],
             output.ratio = round(value, energy.DIGITS_CALOUTPUT)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechSecOut"]]) -> L224.StubTechSecOut_elec_EUR

    # L224.StubTechCost_elec_EUR -----------------------
    # Calculate cost adjustment, equal to the output of heat multiplied by the heat price (to minimize the distortion of including the secondary output)
    L224.StubTechSecOut_elec_EUR %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], "output.ratio") %>%
      mutate(minicam.non.energy.input = "heat plant",
             input.cost = round(output.ratio * energy.HEAT_PRICE, energy.DIGITS_COST))-> L224.StubTechCost_elec_EUR

    # The secondary output of heat from CHP in the electric sector can cause the price of the technologies
    # to go very low or negative if the technology cost is not modified to reflect the additional costs of
    # CHP systems (as compared with electricity-only systems). Low technology costs can cause unrealistically
    # low electricity prices in the calibration year, distorting behavior in future years. In this method,
    # costs related to heat production and distribution are backed out from exogenous heat prices and data-derived heat:power ratios.
    L1231.eff_R_elec_F_tech_Yh_EUR %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(efficiency = value) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(region %in% heat_region$region) %>%
      filter(fuel == "gas") %>%
      filter(efficiency < energy.DEFAULT_ELECTRIC_EFFICIENCY) %>%
      mutate(cost_modifier = energy.GAS_PRICE * (1 / energy.DEFAULT_ELECTRIC_EFFICIENCY - 1 / efficiency)) -> L224.eff_cost_adj_Rh_elec_gas_sc_Y

    # Modify the costs
    L224.StubTechCost_elec_EUR %>%
      left_join(L224.eff_cost_adj_Rh_elec_gas_sc_Y %>%
                  rename(subsector = fuel, stub.technology = technology) %>%
                  select(region, subsector, stub.technology, year, cost_modifier),
                by = c("region", "subsector", "stub.technology", "year")) %>%
      mutate(input.cost = if_else(!is.na(cost_modifier), round(pmax(0, input.cost + cost_modifier), energy.DIGITS_COST), input.cost)) %>%
      select(-cost_modifier, -output.ratio) -> L224.StubTechCost_elec_EUR

    # Need to fill out object names for all model time periods
    L224.StubTechCost_elec_EUR %>%
      filter(year == max(year)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      mutate(input.cost = 0) -> L224.StubTechCost_elec_EUR_fut

    L224.StubTechCost_elec_EUR %>%
      bind_rows(L224.StubTechCost_elec_EUR_fut) -> L224.StubTechCost_elec_EUR

    # Produce outputs ===================================================
    L224.StubTechCalInput_heat_EUR %>%
      add_title("Calibrated input to district heat") %>%
      add_units("EJ/yr") %>%
      add_comments("L124.in_EJ_R_heat_F_Yh_EUR and calibrated_techs are joined, shareweights assigned") %>%
      add_comments("as 0 if the calibrated value is 0 and 1 if it is not 0") %>%
      add_precursors("L124.in_EJ_R_heat_F_Yh_EUR", "energy/calibrated_techs", "energy/A_regions", "common/GCAM_region_names") ->
      L224.StubTechCalInput_heat_EUR

    L224.StubTechSecOut_elec_EUR %>%
      add_title("Secondary output of district heat from electricity technologies") %>%
      add_units("EJ") %>%
      add_comments("L124.heatoutratio_R_elec_F_tech_Yh_EUR used to determine secondary output heat from elec, ") %>%
      add_comments("filtering for only model base years") %>%
      add_precursors("L124.heatoutratio_R_elec_F_tech_Yh_EUR", "energy/calibrated_techs", "energy/A24.sector", "common/GCAM_region_names") ->
      L224.StubTechSecOut_elec_EUR

    L224.StubTechCost_elec_EUR %>%
      add_title("Stubtech costs with secondary output heat") %>%
      add_units("1975$/GJ") %>%
      add_comments("From L224.StubTechSecOut_elec_EUR calculate cost adjustment, equal to the output of heat multiplied by the heat price") %>%
      add_comments("modify costs for technologies with efficiencies below default, apply to all model periods") %>%
      add_precursors("L124.heatoutratio_R_elec_F_tech_Yh_EUR", "energy/calibrated_techs", "energy/A24.sector", "energy/A_regions", "L1231.eff_R_elec_F_tech_Yh_EUR", "common/GCAM_region_names") ->
      L224.StubTechCost_elec_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
