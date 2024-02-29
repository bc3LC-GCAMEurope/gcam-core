# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L271.desalination
#'
#' Generate sector and technologies for desalination
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L271.Supplysector_desal},
#'   \code{L271.FinalEnergyKeyword_desal}, \code{L271.SubsectorLogit_desal}, \code{L271.SubsectorShrwtFllt_desal},
#'   \code{L271.SubsectorInterpTo_desal}, \code{L271.StubTech_desal}, \code{L271.StubTechProd_desal_EUR}.
#' @details This chunk translates input assumptions from CSV to XML, and calibrates the production by technology in each
#'   region and model base year
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by left_join mutate rename select ungroup
#' @importFrom tidyr complete nesting
#' @author RH Feb 2024
module_gcameurope_L271.desalination <- function(command, ...) {
  OUTPUTS_TO_COPY_FILTER <- c("L271.Supplysector_desal",
                              "L271.FinalEnergyKeyword_desal",
                              "L271.SubsectorLogit_desal",
                              "L271.SubsectorShrwt_desal",
                              "L271.SubsectorShrwtFllt_desal",
                              "L271.SubsectorInterp_desal",
                              "L271.SubsectorInterpTo_desal",
                              "L271.StubTech_desal")
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     FILE = "water/EFW_mapping",
                     "L171.out_km3_R_desal_F_tech_Yh_EUR",
                     OUTPUTS_TO_COPY_FILTER)
  MODULE_OUTPUTS <- c("L271.StubTechProd_desal_EUR",
                      paste0(OUTPUTS_TO_COPY_FILTER, "_EUR"))
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- supplysector <- year.fillout <- to.value <- technology <- subsector <-
      minicam.energy.input <- coefficient <- share.weight <- minicam.non.energy.input <-
      input.cost <- sector <- fuel <- desal_km3 <- calOutputValue <- subs.share.weight <-
      NULL  # silence package check notes

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Create outputs that are simply copied from main scripts and filtered to Eurostat regions
    copy_filter_europe(all_data, OUTPUTS_TO_COPY_FILTER)

    # Calibrated output of desalinated water by technology
    L271.StubTechProd_desal_EUR <- filter(L171.out_km3_R_desal_F_tech_Yh_EUR, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names,
                               by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(EFW_mapping, supplysector, subsector, technology, sector, fuel),
                               by = c("sector", "fuel", "technology")) %>%
      rename(stub.technology = technology) %>%
      mutate(calOutputValue = round(desal_km3, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]])


    #==== OUTPUT ===========
    L271.StubTechProd_desal_EUR %>%
      add_title("Calibrated production of desalinated water by technology") %>%
      add_units("km^3/yr") %>%
      add_comments("Desalination-only plants (excludes combined electric + desal plants)") %>%
      add_precursors("L171.out_km3_R_desal_F_tech_Yh_EUR", "water/EFW_mapping", "common/GCAM_region_names") ->
      L271.StubTechProd_desal_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
