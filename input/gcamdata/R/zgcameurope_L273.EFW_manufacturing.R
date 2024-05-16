# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L273.EFW_manufacturing
#'
#' Generate manufacturing energy-for-water input file
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L273.Supplysector_ind},
#'   \code{L273.FinalEnergyKeyword_ind}, \code{L273.SubsectorLogit_ind}, \code{L273.SubsectorShrwtFllt_ind},
#'   \code{L273.SubsectorInterpTo_ind}, \code{L273.StubTech_ind}, \code{L273.GlobalTechCoef_ind},
#'   \code{L273.GlobalTechShrwt_ind}, \code{L273.StubTechCoef_ind_EUR}. This did not
#'   exist in the original data system.
#' @details This chunk translates input assumptions from CSV to XML, and calibrates the input-output coefficients by
#'   technology in each region and model base year
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by mutate rename select ungroup
#' @importFrom tidyr complete nesting
#' @author GPK January 2019
module_gcameurope_L273.EFW_manufacturing <- function(command, ...) {
  OUTPUTS_TO_COPY_FILTER <-c("L273.Supplysector_ind",
                             "L273.FinalEnergyKeyword_ind",
                             "L273.SubsectorLogit_ind",
                             "L273.SubsectorShrwt_ind",
                             "L273.SubsectorShrwtFllt_ind",
                             "L273.SubsectorInterp_ind",
                             "L273.SubsectorInterpTo_ind",
                             "L273.StubTech_ind")
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     FILE = "water/EFW_mapping",
                     FILE = "water/A73.sector",
                     FILE = "water/A73.subsector_interp",
                     FILE = "water/A73.subsector_logit",
                     FILE = "water/A73.subsector_shrwt",
                     FILE = "water/A73.globaltech_coef",
                     FILE = "water/A73.globaltech_shrwt",
                     "L173.IO_GJkm3_R_indEFW_F_Yh_EUR",
                     "L273.GlobalTechCoef_ind",
                     OUTPUTS_TO_COPY_FILTER)
  MODULE_OUTPUTS <- c("L273.StubTechCoef_ind_EUR",
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

    # L273.StubTechCoef_ind_EUR ===================================================
    # Calibrated output of manufacturing sector energy-for-water by technology
    L273.StubTechCoef_ind_EUR <- filter(L173.IO_GJkm3_R_indEFW_F_Yh_EUR, year %in% MODEL_BASE_YEARS) %>%
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
    L273.StubTechCoef_ind_EUR_future <- filter(L273.StubTechCoef_ind_EUR, year == max(MODEL_BASE_YEARS)) %>%
      left_join(L273.GlobalTechCoef_ind, by = c(supplysector = "sector.name", subsector = "subsector.name", stub.technology = "technology", "year"),
                suffix = c("", ".default")) %>%
      filter(coefficient > coefficient.default) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])

    L273.StubTechCoef_ind_EUR <- bind_rows(L273.StubTechCoef_ind_EUR, L273.StubTechCoef_ind_EUR_future)

    #==== OUTPUT ===========
    L273.StubTechCoef_ind_EUR %>%
      add_title("GCAM-Europe Calibrated energy-water input-output coefficients of manufacturing energy-for-water") %>%
      add_units("GJ/m^3") %>%
      add_comments("Region-specific data reflect water source for abstraction, and available energy") %>%
      add_precursors("L173.IO_GJkm3_R_indEFW_F_Yh_EUR", "water/EFW_mapping",
                     "common/GCAM_region_names", "L273.GlobalTechCoef_ind") ->
      L273.StubTechCoef_ind_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
