# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L274.EFW_municipal
#'
#' Generate municipal water sector energy-for-water input file
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L274.Supplysector_muni},
#'   \code{L274.FinalEnergyKeyword_muni}, \code{L274.SubsectorLogit_muni}, \code{L274.SubsectorShrwtFllt_muni},
#'   \code{L274.SubsectorInterpTo_muni}, \code{L274.StubTech_muni}, \code{L274.StubTechCoef_muni_EUR}.
#' @details This chunk translates input assumptions from CSV to XML, and calibrates the input-output coefficients by
#'   technology in each region and model base year
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by mutate rename select ungroup
#' @importFrom tidyr complete nesting
#' @author RH Feb 2024
module_gcameurope_L274.EFW_municipal <- function(command, ...) {
  OUTPUTS_TO_COPY_FILTER <- c("L274.Supplysector_muni",
                              "L274.FinalEnergyKeyword_muni",
                              "L274.SubsectorLogit_muni",
                              "L274.SubsectorShrwt_muni",
                              "L274.SubsectorShrwtFllt_muni",
                              "L274.SubsectorInterp_muni",
                              "L274.SubsectorInterpTo_muni",
                              "L274.StubTech_muni")
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     FILE = "water/EFW_mapping",
                     "L274.GlobalTechCoef_muni",
                     "L174.IO_GJkm3_R_muniEFW_F_Yh_EUR",
                     OUTPUTS_TO_COPY_FILTER)
  MODULE_OUTPUTS <- c("L274.StubTechCoef_muni_EUR",
                      paste0(OUTPUTS_TO_COPY_FILTER, "_EUR"))
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- year.fillout <- to.value <- technology <- supplysector <- subsector <-
      minicam.energy.input <- coefficient <- share.weight <- sector <-
      fuel <- region <- coefficient.default <- NULL  # silence package check notes

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Create outputs that are simply copied from main scripts and filtered to Eurostat regions
    copy_filter_europe(all_data, OUTPUTS_TO_COPY_FILTER)
    # ===================================================
    # Calibrated output of municipal water sector energy-for-water by technology
    L274.StubTechCoef_muni_EUR <- filter(L174.IO_GJkm3_R_muniEFW_F_Yh_EUR, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names,
                               by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(EFW_mapping, supplysector, subsector, technology, minicam.energy.input, sector, fuel),
                               by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT),
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])

    # If any of the stub technology coefficients are greater than the global technology defaults, copy them forward to all future years
    # (Coefs less than the global tech defaults are only that way because of base-year energy balancing, not something to carry forward)
    L274.StubTechCoef_muni_EUR_future <- filter(L274.StubTechCoef_muni_EUR, year == max(MODEL_BASE_YEARS)) %>%
      left_join(L274.GlobalTechCoef_muni, by = c(supplysector = "sector.name", subsector = "subsector.name", stub.technology = "technology", "year"),
                suffix = c("", ".default")) %>%
      filter(coefficient > coefficient.default) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])

    L274.StubTechCoef_muni_EUR <- bind_rows(L274.StubTechCoef_muni_EUR, L274.StubTechCoef_muni_EUR_future)


    #==== OUTPUT ===========
    L274.StubTechCoef_muni_EUR %>%
      add_title("Calibrated energy-water input-output coefficients of municipal water energy-for-water") %>%
      add_units("GJ/m^3") %>%
      add_comments("Region-specific data reflect water source for abstraction, and available energy") %>%
      add_precursors("L174.IO_GJkm3_R_muniEFW_F_Yh_EUR", "water/EFW_mapping", "common/GCAM_region_names") ->
      L274.StubTechCoef_muni_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
