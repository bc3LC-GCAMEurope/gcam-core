# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L226.en_distribution
#'
#' Generate the level 2 data tables for the energy distribution sector,
#' including capital costs, shareweights, logits, and interpolations as well as energy use coefficients for electricity and gas pipeline
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L226.SectorLogitTables[[ curr_table ]]$data}, \code{L226.Supplysector_en}, \code{L226.SubsectorLogitTables[[ curr_table ]]$data}, \code{L226.SubsectorLogit_en}, \code{L226.SubsectorShrwt_en}, \code{L226.SubsectorShrwtFllt_en}, \code{L226.SubsectorInterp_en}, \code{L226.SubsectorInterpTo_en}, \code{L226.StubTech_en}, \code{L226.StubTechCoef_elecownuse_EUR}, \code{L226.StubTechCoef_electd_EUR}, \code{L226.StubTechCoef_gaspipe_EUR}.
#' @details Prepares Level 2 data on energy distribution sector for the generation of en_distribution.xml.
#' Creates global technology database info--cost, shareweight, logit, efficiencies, and interpolations--and regional values where applicable for electricity net ownuse, gas pipelines, and transmission and distribution.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select
#' @importFrom tidyr complete nesting
#' @author RH Feb 2024
module_gcameurope_L226.en_distribution <- function(command, ...) {
  OUTPUTS_TO_COPY_FILTER <- c("L226.Supplysector_en",
                              "L226.SubsectorLogit_en",
                              "L226.SubsectorShrwt_en",
                              "L226.SubsectorShrwtFllt_en",
                              "L226.SubsectorInterp_en",
                              "L226.SubsectorInterpTo_en",
                              "L226.StubTech_en")

  MODULE_INPUTS <- c("L101.GCAM_EUR_regions",
                     FILE = "energy/calibrated_techs",
                     FILE = "energy/A_regions",
                     "L126.IO_R_elecownuse_F_Yh_EUR",
                     "L126.IO_R_electd_F_Yh_EUR",
                     "L126.IO_R_gaspipe_F_Yh_EUR",
                     OUTPUTS_TO_COPY_FILTER)

  MODULE_OUTPUTS <- c("L226.StubTechCoef_elecownuse_EUR",
                      "L226.StubTechCoef_electd_EUR",
                      "L226.StubTechCoef_gaspipe_EUR",
                      paste0(OUTPUTS_TO_COPY_FILTER, "_EUR"))
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    year <- year.fillout <- to.value <- technology <- efficiency <- supplysector <- subsector <-
      minicam.energy.input <- input.cost <- share.weight <- calibration <-
      secondary.output <- year.x <- year.y <- . <- value <- region <- coefficient <- GCAM_region_ID <-
      sector <- fuel <- minicam.non.energy.input <- elect_td_techchange <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    GCAM_region_names <- L101.GCAM_EUR_regions %>%
      distinct(GCAM_region_ID, region = GCAMEU_region)

    # Create outputs that are simply copied from main scripts and filtered to Eurostat regions
    copy_filter_europe(all_data, OUTPUTS_TO_COPY_FILTER)

    # L226.StubTechCoef_elecownuse_EUR-------------
    # Electricity ownuse IO coefs - filter down to the base years and append region IDs
    L126.IO_R_elecownuse_F_Yh_EUR %>%
      filter(year %in% c(MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(calibrated_techs, by = c("sector", "fuel")) %>%
      select(-calibration, -secondary.output) ->
      L226.IO_R_elecownuse_F_Yh_EUR

    # repeat final year's ownuse ratio into future years and append future years to base years (could perhaps be tied to industrial CHP...but also AUTOELEC)
    L226.IO_R_elecownuse_F_Yh_EUR %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      repeat_add_columns(tibble("year" = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) %>%
      bind_rows(L226.IO_R_elecownuse_F_Yh_EUR) ->
      L226.IO_R_elecownuse_F_Y

    # rename columns and round coefficients - L226.StubTechCoef_elecownuse_EUR: calibrated coefficients on electricity net ownuse
    L226.IO_R_elecownuse_F_Y %>%
      rename(coefficient = value, stub.technology = technology) %>%
      mutate(market.name = region, coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      select(-GCAM_region_ID, -sector, -fuel) ->
      L226.StubTechCoef_elecownuse_EUR

    # L226.StubTechCoef_electd_EUR ----------------
    # Filter electricity transmission and distribution input-output ratio historical data down to base years (this works now but may need optional interpolation if the assumptions file changes not to include base model years)
    L126.IO_R_electd_F_Yh_EUR %>%
      filter(year %in% MODEL_BASE_YEARS) ->
      L226.IO_R_electd_F_Yh_EUR

    # Copy final base year value to future periods
    L226.IO_R_electd_F_Yh_EUR %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      repeat_add_columns(tibble("year" = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L226.IO_R_electd_F_Yfut

    # append assumed techchange value and calculate decrease in future energy use
    L226.IO_R_electd_F_Yfut %>%
      left_join(A_regions, by = "GCAM_region_ID") %>%
      mutate(value = value * ((1 - elect_td_techchange) ^ (year - max(MODEL_BASE_YEARS)))) %>%
      select(GCAM_region_ID, sector, fuel, value, year) %>%
      bind_rows(L226.IO_R_electd_F_Yh_EUR, .) ->
      L226.IO_R_electd_F_Y

    # Use base and future IO ratios to generate L226.StubTechCoef_electd_EUR: calibrated coefficients on electricity transmission and distribution
    # Electricity T&D: Because this is written out to multiple sectors, need to start with the list in calibrated_techs
    calibrated_techs %>%
      filter(paste(sector, fuel) %in% paste(L226.IO_R_electd_F_Y$sector, L226.IO_R_electd_F_Y$fuel)) %>%
      # append region names and calibrated tech info
      repeat_add_columns(tibble("GCAM_region_ID" = unique(L226.IO_R_electd_F_Y$GCAM_region_ID))) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      repeat_add_columns(tibble("year" = unique(L226.IO_R_electd_F_Y$year))) %>%
      left_join_error_no_match(L226.IO_R_electd_F_Y, by = c("sector", "fuel", "GCAM_region_ID", "year")) %>%
      # adjust column names and round values to clean up final table
      rename(stub.technology = technology, coefficient = value) %>%
      mutate(market.name = region) %>%
      select(-sector, -fuel, -calibration, -secondary.output, -GCAM_region_ID) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT))->
      L226.StubTechCoef_electd_EUR

    # L226.StubTechCoef_gaspipe_EUR ----------------
    # Interpolate regional gas pipeline IO coefs to generate future year values, add region names, and calibrated tech information
    L126.IO_R_gaspipe_F_Yh_EUR %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, MODEL_BASE_YEARS)) %>%
      arrange(GCAM_region_ID, sector, year) %>%
      group_by(GCAM_region_ID, sector) %>%
      mutate (value = approx_fun(as.numeric(year), value)) %>%
      ungroup() %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(calibrated_techs, by = c("sector", "fuel")) %>%
      select(-calibration, -secondary.output, -GCAM_region_ID, -sector, -fuel) ->
      L226.IO_R_gaspipe_F_Yh_EUR

    # Generate future year gas pipeline energy use ratios by holding final base year value constant
    L226.IO_R_gaspipe_F_Yh_EUR %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      repeat_add_columns(tibble("year" = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L226.IO_R_gaspipe_F_Yfut

    # L226.StubTechCoef_gaspipe_EUR: calibrated coefficients on gas pipeline energy use
    # Append future years to base years, add a market.name column, and round the values for the xml
    L226.IO_R_gaspipe_F_Yfut %>%
      bind_rows(L226.IO_R_gaspipe_F_Yh_EUR, .) %>%
      mutate(market.name = region, value = round(value, energy.DIGITS_COEFFICIENT)) %>%
      rename(coefficient = value, stub.technology = technology) ->
      L226.StubTechCoef_gaspipe_EUR

    # Produce outputs ===================================================
    L226.StubTechCoef_elecownuse_EUR %>%
      add_title("Input-Output Coefficients for electricity sectors' own electricity use") %>%
      add_units("IO coefficient - Unitless") %>%
      add_comments("future values generated from final base year as a fixed constant") %>%
      add_comments("historical values from L126.IO_R_elecownuse_F_Yh_EUR filtered to base model years") %>%
      add_precursors("L126.IO_R_elecownuse_F_Yh_EUR", "common/GCAM_region_names", "energy/calibrated_techs") ->
      L226.StubTechCoef_elecownuse_EUR

    L226.StubTechCoef_electd_EUR %>%
      add_title("coefficients of electricity use for the transmission and distribution of electricity") %>%
      add_units("IO coefficient - Unitless") %>%
      add_comments("future values generated from historical values from final base year of L126.IO_R_electd_F_Yh_EUR, ") %>%
      add_comments("applying an assumed tech change from A_regions (new IO = base IO * 1-techchange ^ year-baseyear)") %>%
      add_precursors("L126.IO_R_electd_F_Yh_EUR", "common/GCAM_region_names", "energy/calibrated_techs", "energy/A_regions") ->
      L226.StubTechCoef_electd_EUR

    L226.StubTechCoef_gaspipe_EUR %>%
      add_title("Regional Gas pipeline I:O coefficients") %>%
      add_units("IO coefficient - Unitless") %>%
      add_comments("input output coefficients from historical data and future values fixed constant from final base year") %>%
      add_precursors("L126.IO_R_gaspipe_F_Yh_EUR") ->
      L226.StubTechCoef_gaspipe_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
