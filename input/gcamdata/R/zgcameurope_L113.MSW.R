# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L113.MSW
#'
#' Europe-specific version of zenergy_L113.MSW. Builds the municipal solid waste (MSW) supply curves of the
#' Eurostat regions (\code{gcameurope.EUROSTAT_COUNTRIES}) from country-level data
#' (gcam-europe/A13.MSW_curves_EUR), instead of downscaling the GCAM 3.0 Western Europe / Eastern Europe curves by GDP.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L113.RsrcCurves_EJ_R_MSW_EUR}.
#' @details Each row of gcam-europe/A13.MSW_curves_EUR is one GCAM-Europe region (matched by name in
#' common/GCAM_region_names) and is used directly, with no GDP downscaling. The table covers exactly
#' \code{gcameurope.EUROSTAT_COUNTRIES}; Belarus, Switzerland and the non-European regions keep the core
#' curves of zenergy_L113.MSW (written in resources.xml). As in the core chunk, all regions must share the
#' same curve-shape parameters.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange select
#' @author GCAM-Europe 2026
module_gcameurope_L113.MSW <- function(command, ...) {

  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     FILE = "gcam-europe/A13.MSW_curves_EUR")

  MODULE_OUTPUTS <- c("L113.RsrcCurves_EJ_R_MSW_EUR")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    region <- region_GCAM3 <- resource <- subresource <- mid.price <- curve.exponent <-
      gdpSupplyElast <- maxSubResource <- GCAM_region_ID <- NULL  # silence package check notes

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Checks on the input table ----------------------------------------------
    # This method assumes that all regions have the same curve shape parameters (as zenergy_L113.MSW)
    if(length(unique(A13.MSW_curves_EUR$resource)) > 1 |
       length(unique(A13.MSW_curves_EUR$subresource)) > 1 |
       length(unique(A13.MSW_curves_EUR$`mid-price`)) > 1 |
       length(unique(A13.MSW_curves_EUR$`curve-exponent`)) > 1 |
       length(unique(A13.MSW_curves_EUR$gdpSupplyElast)) > 1) {
      stop("The A13.MSW_curves_EUR data have different curve shape parameters!")
    }

    # Column region_GCAM3 holds GCAM-Europe region names (GCAM_region_names$region)
    unknown_regions <- setdiff(A13.MSW_curves_EUR$region_GCAM3, GCAM_region_names$region)
    if(length(unknown_regions) > 0) {
      stop("A13.MSW_curves_EUR has names that are not GCAM regions: ", paste(unknown_regions, collapse = ", "))
    }

    duplicated_regions <- unique(A13.MSW_curves_EUR$region_GCAM3[duplicated(A13.MSW_curves_EUR$region_GCAM3)])
    if(length(duplicated_regions) > 0) {
      stop("A13.MSW_curves_EUR has duplicated regions: ", paste(duplicated_regions, collapse = ", "))
    }

    missing_regions <- setdiff(gcameurope.EUROSTAT_COUNTRIES, A13.MSW_curves_EUR$region_GCAM3)
    extra_regions <- setdiff(A13.MSW_curves_EUR$region_GCAM3, gcameurope.EUROSTAT_COUNTRIES)
    if(length(missing_regions) > 0 | length(extra_regions) > 0) {
      stop("A13.MSW_curves_EUR regions must equal gcameurope.EUROSTAT_COUNTRIES. Missing: ",
           paste(missing_regions, collapse = ", "), "; not in gcameurope.EUROSTAT_COUNTRIES: ",
           paste(extra_regions, collapse = ", "))
    }

    if(any(is.na(A13.MSW_curves_EUR$maxSubResource)) | any(A13.MSW_curves_EUR$maxSubResource < 0)) {
      stop("A13.MSW_curves_EUR has NA or negative maxSubResource")
    }

    # Supply curves by region: country rows used directly ---------------------
    # Same columns as L113.RsrcCurves_EJ_R_MSW (zenergy_L113.MSW)
    A13.MSW_curves_EUR %>%
      left_join_error_no_match(GCAM_region_names, by = c("region_GCAM3" = "region")) %>%
      select(GCAM_region_ID, resource, subresource, mid.price = `mid-price`, curve.exponent = `curve-exponent`,
             gdpSupplyElast, maxSubResource) %>%
      arrange(GCAM_region_ID) %>%

      # Documentation
      add_title("Municipal solid waste resource curves by GCAM-Europe region") %>%
      add_units("maxSubResource: EJ/yr; mid.price: 1975$/GJ") %>%
      add_comments("Country-level MSW supply curves for the Eurostat regions, used directly (no GDP downscaling)") %>%
      add_comments("Belarus, Switzerland and non-European regions keep the core L113.RsrcCurves_EJ_R_MSW") %>%
      add_precursors("gcam-europe/A13.MSW_curves_EUR",
                     "common/GCAM_region_names") ->
      L113.RsrcCurves_EJ_R_MSW_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
