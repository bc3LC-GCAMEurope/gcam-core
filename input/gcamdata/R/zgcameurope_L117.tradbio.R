# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L117.tradbio
#'
#' Creates regional traditional biomass supply curves
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L117.RsrcCurves_EJ_R_tradbio}. The corresponding file in the
#' original data system was \code{LA117.tradbio.R} (energy level1).
#' @details Creates regional traditional biomass supply curves by multiplying the max historical amount
#' used in each region by the supply curve amount in the global assumption supply curve
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by mutate select summarise
#' @author RH March 2024
module_gcameurope_L117.tradbio <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L1012.en_bal_EJ_R_Si_Fi_Yh_EUR",
             FILE = "energy/A17.tradbio_curves"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L117.RsrcCurves_EJ_R_tradbio_EUR"))
  } else if(command == driver.MAKE) {

    ## silence package check.
    year <- value <- GCAM_region_ID <- sector <- fuel <- available <-
      resource <- subresource <- grade <- extractioncost <- NULL


    all_data <- list(...)[[1]]

    # Load required inputs
    L1012.en_bal_EJ_R_Si_Fi_Yh_EUR <- get_data(all_data, "L1012.en_bal_EJ_R_Si_Fi_Yh_EUR")
    A17.tradbio_curves <- get_data(all_data, "energy/A17.tradbio_curves", strip_attributes = TRUE)

    # ===================================================

    # Calculate the max resource of tradbio as the maximum during the historical years
    L1012.en_bal_EJ_R_Si_Fi_Yh_EUR %>%
      filter(fuel == "biomass_tradbio", sector == "TPES") %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      summarise(value = max(value)) %>%
      ungroup ->
      L117.maxSubResource_tradbio

    # Add 0 for missing countries
    L117.maxSubResource_tradbio <- L117.maxSubResource_tradbio %>%
      complete(nesting(sector, fuel), GCAM_region_ID = unique(L1012.en_bal_EJ_R_Si_Fi_Yh_EUR$GCAM_region_ID)) %>%
      tidyr::replace_na(list(value = 0))

    # Writing the supply curves to all regions, multiplying maxSubResouce by quantity available at each grade
    repeat_add_columns(A17.tradbio_curves, L117.maxSubResource_tradbio) %>%
      arrange(GCAM_region_ID) %>%
      mutate(available = available * value) %>%
      select(GCAM_region_ID, resource, subresource, grade, available, extractioncost) %>%

      # ===================================================

    # Produce outputs

    add_title("Traditional biomass resources by GCAM region ") %>%
      add_units("available: EJ; extractioncost: 1975$/GJ") %>%
      add_comments("Multiply the max historical amount used in each region by the supply curve amount") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh_EUR", "energy/A17.tradbio_curves") ->
      L117.RsrcCurves_EJ_R_tradbio_EUR

    return_data(L117.RsrcCurves_EJ_R_tradbio_EUR)
  } else {
    stop("Unknown command")
  }
}
