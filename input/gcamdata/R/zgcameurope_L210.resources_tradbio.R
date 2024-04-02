# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L210.resources_tradbio
#'
#' Resource market information, prices, and supply curves for traditional biomass in Europe.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L210.RenewRsrc_tradbio_EUR}, \code{L210.RenewRsrcPrice_tradbio_EUR},
#' \code{L210.GrdRenewRsrcCurves_tradbio_EUR}, \code{L210.GrdRenewRsrcMax_tradbio_EUR}, \code{L210.ResTechShrwt_tradbio_EUR}
#' @details Resource market information, prices, and supply curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else mutate select semi_join
#' @author RLH March 2024
module_gcameurope_L210.resources_tradbio <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     FILE = "energy/A10.rsrc_info",
                     "L117.RsrcCurves_EJ_R_tradbio_EUR")
  MODULE_OUTPUTS <- c("L210.RenewRsrc_tradbio_EUR",
                      "L210.RenewRsrcPrice_tradbio_EUR",
                      "L210.GrdRenewRsrcCurves_tradbio_EUR",
                      "L210.GrdRenewRsrcMax_tradbio_EUR",
                      "L210.ResTechShrwt_tradbio_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {


    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)
    A10.rsrc_info <- A10.rsrc_info %>% gather_years()

    # A. Output unit, price unit, market -------------
    L210.rsrc_info_tradbio_EUR <- A10.rsrc_info %>%
      filter(resource == "traditional biomass") %>%
      repeat_add_columns(tibble(region = gcameurope.EUROSTAT_COUNTRIES)) %>%
      # Reset regional markets to the names of the specific regions
      mutate(market = if_else(market == "regional", region, market))

    # L210.RenewRsrc: output unit, price unit, and market for renewable resources
    L210.RenewRsrc_tradbio_EUR <- L210.rsrc_info_tradbio_EUR %>%
      filter(resource_type == "renewresource") %>%
      select(region, renewresource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market) %>%
      distinct()

    # L210.RenewRsrcPrice: historical prices for renewable resources
    L210.RenewRsrcPrice_tradbio_EUR <- L210.rsrc_info_tradbio_EUR %>%
      filter(resource_type == "renewresource",
             year %in% MODEL_BASE_YEARS) %>%
      select(region, renewresource = resource, year, price = value)

    # B. Resource supply curves -----------
    # L210.GrdRenewRsrcCurves_tradbio: graded supply curves of traditional biomass resources
    L210.GrdRenewRsrcCurves_tradbio_EUR <- L117.RsrcCurves_EJ_R_tradbio_EUR %>%
      # Add region name
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(available = round(available, energy.DIGITS_MAX_SUB_RESOURCE)) %>%
      select(region, renewresource = resource, sub.renewable.resource = subresource, grade, available, extractioncost)

    # L210.GrdRenewRsrcMax_tradbio: default max sub resource of tradbio resources
    L210.GrdRenewRsrcMax_tradbio_EUR <- L210.GrdRenewRsrcCurves_tradbio_EUR %>%
      filter(grade == "grade 1") %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             maxSubResource = 1) %>%
      select(LEVEL2_DATA_NAMES[["maxSubResource"]])

    # C. Shareweights ------------------
    # We need to make sure we have at least a shell technology for ALL resources
    # and so we will just use the share weight table to facilatate doing that.
    A10.rsrc_info %>%
      select(resource) %>%
      filter(resource == "traditional biomass") %>%
      repeat_add_columns(tibble(region = gcameurope.EUROSTAT_COUNTRIES)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(subresource = resource,
             technology = subresource,
             share.weight = if_else(year > MODEL_FINAL_BASE_YEAR, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      L210.ResTechShrwt_tradbio_EUR

    # Produce outputs -------------------------------------
    L210.RenewRsrc_tradbio_EUR %>%
      add_title("Market information for trad bio resources") %>%
      add_units("NA") %>%
      add_comments("A10.rsrc_info written to all regions") %>%
      add_precursors("energy/A10.rsrc_info") ->
      L210.RenewRsrc_tradbio_EUR

    L210.RenewRsrcPrice_tradbio_EUR %>%
      add_title("Historical prices for trad bio") %>%
      add_units("1975$/GJ") %>%
      add_comments("A10.rsrc_info written to all regions") %>%
      same_precursors_as(L210.RenewRsrc_tradbio_EUR) ->
      L210.RenewRsrcPrice_tradbio_EUR

    L210.GrdRenewRsrcCurves_tradbio_EUR %>%
      add_title("Graded supply curves of traditional biomass resources") %>%
      add_units("available: EJ; extractioncost: 1975$/GJ") %>%
      add_comments("Data from L117.RsrcCurves_EJ_R_tradbio") %>%
      add_precursors("L117.RsrcCurves_EJ_R_tradbio_EUR", "common/GCAM_region_names") ->
      L210.GrdRenewRsrcCurves_tradbio_EUR

    L210.GrdRenewRsrcMax_tradbio_EUR %>%
      add_title("Default max sub resource of traditional biomass resources") %>%
      add_units("Unitless") %>%
      add_comments("maxSubResource assumed to be 1 for all regions") %>%
      same_precursors_as(L210.GrdRenewRsrcCurves_tradbio_EUR) ->
      L210.GrdRenewRsrcMax_tradbio_EUR

    L210.ResTechShrwt_tradbio_EUR %>%
      add_title("Share weights for technologies in resources") %>%
      add_units("NA") %>%
      add_comments("Share weights won't matter for resource technologies as there") %>%
      add_comments("is no competetion between technologies.") %>%
      add_precursors("energy/A10.rsrc_info") ->
      L210.ResTechShrwt_tradbio_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
