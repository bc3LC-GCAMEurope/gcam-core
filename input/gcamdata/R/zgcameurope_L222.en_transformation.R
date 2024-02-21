# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L222.en_transformation
#'
#' Prepare the calibrated outputs for energy transformation supplysectors, subsectors, and technologies.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L222.StubTechProd_gasproc_EUR}, \code{L222.StubTechProd_refining_EUR}, \code{L222.StubTechCoef_refining_EUR},
#' @details Calibrated outputs for GCAM-Europe gas processing and oil refining as well as I:O coefficients are interpolated from historical values to base model years.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select semi_join
#' @importFrom tidyr complete nesting
#' @author RH Feb 2024
module_gcameurope_L222.en_transformation <- function(command, ...) {
  OUTPUTS_TO_COPY_FILTER <- c("L222.Supplysector_en",
                              "L222.SectorUseTrialMarket_en",
                              "L222.SubsectorLogit_en",
                              "L222.SubsectorShrwt_en",
                              "L222.SubsectorShrwtFllt_en",
                              "L222.SubsectorInterp_en",
                              "L222.SubsectorInterpTo_en",
                              "L222.StubTech_en"
                              )

  MODULE_INPUTS <- c("L101.GCAM_EUR_regions",
                     FILE = "energy/calibrated_techs",
                     "L122.out_EJ_R_gasproc_F_Yh_EUR",
                     "L122.out_EJ_R_refining_F_Yh_EUR",
                     "L122.IO_R_oilrefining_F_Yh_EUR",
                     "L222.GlobalTechCoef_en",
                     OUTPUTS_TO_COPY_FILTER
  )

  MODULE_OUTPUTS <- c("L222.StubTechProd_gasproc_EUR",
                      "L222.StubTechProd_refining_EUR",
                      "L222.StubTechCoef_refining_EUR",
                      paste0(OUTPUTS_TO_COPY_FILTER, "_EUR"))
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silencing global variable package checks
    GCAM_region_ID <- calOutputValue <- coefficient <- fuel <- half.life <- input.cost <- lifetime <-
      median.shutdown.point <- minicam.energy.input <- minicam.non.energy.input <- object <-
      profit.shutdown.steepness <- region <- remove.fraction <- sector <- sector.name <- share.weight <-
      shutdown.rate <- steepness <- stub.technology <- subsector <- subsector.name <- supplysector <-
      technology <- to.value <- value <- year <- year.fillout <- year.share.weight <- year.x <- year.y <-
      primary.consumption <- NULL

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    GCAM_region_names <- L101.GCAM_EUR_regions %>%
      distinct(GCAM_region_ID, region = GCAMEU_region)

    # Create outputs that are simply copied from main scripts and filtered to Eurostat regions
    copy_filter_europe(all_data, OUTPUTS_TO_COPY_FILTER)

    # Gas production ===================================================
    #  generate base year calibrated outputs of gas processing by interpolating from historical values
    L122.out_EJ_R_gasproc_F_Yh_EUR %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, MODEL_BASE_YEARS)) %>%
      tidyr::replace_na(list(value = 0)) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L222.out_EJ_R_gasproc_F_Yh_base

    # append matching calibrated technology sector/subsector/technology to calibrated outputs of gas processing
    calibrated_techs %>%
      semi_join(L222.out_EJ_R_gasproc_F_Yh_base, by = c("sector", "fuel")) %>%
      select(sector, fuel, supplysector, subsector, technology) %>%
      # left_join because the join changes the number of rows, multiple matches in gasproc for every calibrated tech.
      left_join(L222.out_EJ_R_gasproc_F_Yh_base, by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) ->
      L222.out_EJ_R_gasproc_F_Yh

    # L222.StubTechProd_gasproc_EUR: calibrated output of gas processing technologies -- writes to all regions, adds calibrated tech info
    L222.GlobalTechCoef_en %>%
      distinct(supplysector = sector.name, subsector = subsector.name, technology, minicam.energy.input) %>%
      filter(supplysector == "gas processing") %>%
      write_to_all_regions(c("region", "supplysector", "subsector", "technology", "minicam.energy.input"), GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L222.out_EJ_R_gasproc_F_Yh, by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      # rounds outputs and adds year column for shareweights
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT), year.share.weight = year) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue, year.share.weight) %>%
      # sets shareweight to 1 if output exists, otherwise 0
      mutate(share.weight = if_else(calOutputValue > 0, 1, 0), subs.share.weight = share.weight) ->
      L222.StubTechProd_gasproc_EUR
    # reorders columns to match expected model interface input
    L222.StubTechProd_gasproc_EUR <- L222.StubTechProd_gasproc_EUR[c(LEVEL2_DATA_NAMES[["StubTechYr"]], "calOutputValue", "year.share.weight", "subs.share.weight", "share.weight")]

    # Oil refining calibrated output by technology ===================================================
    # interpolates values of IO coefficients for base years from historical values
    L122.out_EJ_R_refining_F_Yh_EUR %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, MODEL_BASE_YEARS)) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L222.out_EJ_R_refining_F_Yh

    # L222.StubTechProd_refining_EUR: calibrated output of refining technologies
    # matches calibrated tech info (sector, subsector, stub.technology) to refining outputs for base years and adds to output file
    calibrated_techs %>%
      semi_join(L222.out_EJ_R_refining_F_Yh, by = c("sector", "fuel")) %>%
      select(sector, fuel, supplysector, subsector, technology) %>%
      rename(stub.technology = technology) %>%
      left_join(L222.out_EJ_R_refining_F_Yh, by = c("sector", "fuel")) %>%
      # rounds and renames outputs and adds year column for shareweights
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT), year.share.weight = year) %>%
      select(-sector, -GCAM_region_ID, -fuel, -value) %>%
      # sets shareweight to 1 if output exists, otherwise 0
      mutate(share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      set_subsector_shrwt() ->
      L222.StubTechProd_refining_EUR
    # reorders columns to match expected model interface input
    L222.StubTechProd_refining_EUR <- L222.StubTechProd_refining_EUR[c(LEVEL2_DATA_NAMES[["StubTechYr"]], "calOutputValue", "year.share.weight", "subs.share.weight", "share.weight")]

    # L222.StubTechCoef_refining_EUR: calibrated input-output coefficients of oil refining by region and input
    # interpolates values of IO coefficients for base years from historical values
    L122.IO_R_oilrefining_F_Yh_EUR %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, MODEL_BASE_YEARS)) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L222.IO_R_oilrefining_F_Yh

    # matches calibrated tech info (sector, subsector, stub.technology) to input-output coefficients for base years
    calibrated_techs %>%
      semi_join(L222.IO_R_oilrefining_F_Yh, by = c("sector", "fuel")) %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      left_join(L222.IO_R_oilrefining_F_Yh, by = c("sector", "fuel")) %>%
      rename(stub.technology = technology)  %>%
      # rounds and renames outputs and adds market name
      mutate(coefficient = round(value, energy.DIGITS_COEFFICIENT), market.name = region) %>%
      select(-sector, -GCAM_region_ID, -fuel, -value) ->
      L222.StubTechCoef_refining_EUR
    # reorders columns to match expected model interface input
    L222.StubTechCoef_refining_EUR <- L222.StubTechCoef_refining_EUR[c(LEVEL2_DATA_NAMES[["StubTechYr"]], "minicam.energy.input", "coefficient", "market.name")]

    # Produce outputs ===================================================
    L222.StubTechProd_gasproc_EUR %>%
      add_title("Historical calibrated output of gas processing technologies", overwrite = T) %>%
      add_units("EJ") %>%
      add_comments("Historical values of output of gas processing for base model years by region") %>%
      add_precursors("L222.GlobalTechCoef_en", "L122.out_EJ_R_gasproc_F_Yh_EUR",
                     "energy/calibrated_techs", "L101.GCAM_EUR_regions") ->
      L222.StubTechProd_gasproc_EUR

    L222.StubTechProd_refining_EUR %>%
      add_title("Historical calibrated output of refining technologies", overwrite = T) %>%
      add_units("EJ") %>%
      add_comments("Historical values of output for liquid refining for base model years by region") %>%
      add_precursors("L122.out_EJ_R_refining_F_Yh_EUR", "energy/calibrated_techs", "L101.GCAM_EUR_regions") ->
      L222.StubTechProd_refining_EUR

    L222.StubTechCoef_refining_EUR %>%
      add_title("Calibrated input-output coefficients of oil refining", overwrite = T) %>%
      add_units("unitless ratio") %>%
      add_comments("Historical values of ratio of inputs to outputs in the oil refining sector by region") %>%
      add_precursors("L122.IO_R_oilrefining_F_Yh_EUR", "energy/calibrated_techs", "L101.GCAM_EUR_regions") ->
      L222.StubTechCoef_refining_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
