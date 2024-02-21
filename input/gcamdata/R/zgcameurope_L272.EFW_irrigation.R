# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L272.EFW_irrigation
#'
#' Generate irrigation energy-for-water input file
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L272.Supplysector_irr_EUR},
#'   \code{L272.FinalEnergyKeyword_irr_EUR}, \code{L272.SubsectorLogit_irr_EUR}, \code{L272.SubsectorShrwtFllt_irr_EUR},
#'   \code{L272.SubsectorInterpTo_irr_EUR}, \code{L272.StubTech_irr_EUR}, \code{L272.StubTechCoef_irr_EUR}.
#' @details This chunk translates input assumptions from CSV to XML, and calibrates the input-output coefficients by
#'   technology in each region and model base year
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by mutate rename select ungroup
#' @importFrom tidyr complete nesting
#' @author RH Feb 2024
module_gcameurope_L272.EFW_irrigation <- function(command, ...) {
  OUTPUTS_TO_COPY_FILTER <- c("L272.Supplysector_irr",
                              "L272.FinalEnergyKeyword_irr",
                              "L272.SubsectorLogit_irr",
                              "L272.SubsectorShrwt_irr",
                              "L272.SubsectorShrwtFllt_irr",
                              "L272.SubsectorInterp_irr",
                              "L272.SubsectorInterpTo_irr",
                              "L272.StubTech_irr")
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     FILE = "water/EFW_mapping",
                     "L172.Coef_GJm3_IrrEnergy_R_EUR",
                     OUTPUTS_TO_COPY_FILTER)
  MODULE_OUTPUTS <- c("L272.StubTechCoef_irr_EUR",
                      paste0(OUTPUTS_TO_COPY_FILTER, "_EUR"))
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- year.fillout <- to.value <- technology <- supplysector <- subsector <-
      minicam.energy.input <- coefficient <- share.weight <- sector <-
      fuel <- region <- NULL  # silence package check notes

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Create outputs that are simply copied from main scripts and filtered to Eurostat regions
    copy_filter_europe(all_data, OUTPUTS_TO_COPY_FILTER)

    # ===================================================
    # Calibrated coefficients of irrigation water abstraction by technology, carried forward to all future years
    L272.StubTechCoef_irr_EUR <- filter(L172.Coef_GJm3_IrrEnergy_R_EUR, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names,
                               by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(EFW_mapping, supplysector, subsector, technology, minicam.energy.input, sector, fuel),
                               by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT),
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])

    # Copy the coefficients forward to future years
    L272.StubTechCoef_irr_EUR_future <- filter(L272.StubTechCoef_irr_EUR, year == max(year)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))

    L272.StubTechCoef_irr_EUR <- bind_rows(L272.StubTechCoef_irr_EUR, L272.StubTechCoef_irr_EUR_future)


    #==== OUTPUT ===========
    L272.StubTechCoef_irr_EUR %>%
      add_title("Calibrated energy-water input-output coefficients of irrigation water abstraction") %>%
      add_units("GJ/m^3") %>%
      add_comments("Region-specific data reflect surface:ground water ratios and available energy") %>%
      add_precursors("L172.Coef_GJm3_IrrEnergy_R_EUR", "water/EFW_mapping", "common/GCAM_region_names") ->
      L272.StubTechCoef_irr_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
