# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L120.offshore_wind
#'
#' Europe-specific version of zenergy_L120.offshore_wind. Takes in data on country-level offshore wind
#' energy potential and builds, for the Eurostat regions, the resource curves, technological change,
#' grid-connection cost adders and region-specific capacity factors SEPARATELY for the two offshore
#' wind resource types: "fixed offshore wind resource" (shallow and transitional depths) and
#' "floating offshore wind resource" (deep waters). The mapping depth class -> resource type (and the technology that consumes it, key of the cost tables of
#' zgcameurope_L1233.elec_cost_data, which provide FCR, fixed O&M and the capital trajectory of each resource) is read from
#' gcam-europe/A20.offshore_wind_types_EUR.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L120.RsrcCurves_EJ_R_offshore_wind_EUR}, \code{L120.TechChange_offshore_wind_EUR},
#' \code{L120.GridCost_offshore_wind_EUR}, \code{L120.RegCapFactor_offshore_wind_EUR}.
#' @details Only region x resource pairs with an offshore potential that allows fitting a smooth renewable
#' resource curve are written (no dummy zero-resource rows). Downstream chunks (zgcameurope_L210.resources)
#' use the pairs present in \code{L120.RsrcCurves_EJ_R_offshore_wind_EUR} as the list of resources that exist
#' in each region.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select group_by summarise distinct arrange bind_rows rename
#' @importFrom tidyr gather
#' @importFrom stats optimize
#' @author MB GI AJS March 2019 (core chunk); GCAM-Europe adaptation 2026
module_gcameurope_L120.offshore_wind <- function(command, ...) {

  MODULE_INPUTS <- c(FILE = "common/iso_GCAM_regID",
                     FILE = "common/GCAM_region_names",
                     FILE = "energy/A20.wind_class_CFs",
                     FILE = "energy/A20.offshore_wind_depth_cap_cost",
                     FILE = "energy/mappings/NREL_wind_ctry",
                     FILE = "energy/NREL_offshore_energy",
                     FILE = "energy/offshore_wind_grid_cost",
                     FILE = "gcam-europe/A20.offshore_wind_types_EUR",
                     "L1233.globaltech_capital_ATB_offshore_EUR",
                     "L1233.globaltech_OMfixed_ATB_offshore_EUR")

  MODULE_OUTPUTS <- c("L120.RsrcCurves_EJ_R_offshore_wind_EUR",
                      "L120.TechChange_offshore_wind_EUR",
                      "L120.GridCost_offshore_wind_EUR",
                      "L120.RegCapFactor_offshore_wind_EUR")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # Silence package checks
    country_name <- GCAM_region_ID <- wind_class <- resource.potential.PWh <- IAM_country <- depth_class <-
      distance_to_shore <- resource.potential.EJ <- fixed.charge.rate <- technology <- fcr <- OM.fixed <-
      price <- CFmax <- supply <- curve.exponent <- mid.price <- maxSubResource <- year <- capital.overnight <-
      tech.change <- cost <- grid.cost <- total <- CF <- resource <- subresource <- k1 <- k2 <- region <-
      rep_dist_for_bin <- share <- cost_per_kW_km <- capital.tech.change.period <- tech.change.period <-
      time.change <- iso <- distance_bin <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    A20.wind_class_CFs <- get_data(all_data, "energy/A20.wind_class_CFs")
    A20.offshore_wind_depth_cap_cost <- get_data(all_data, "energy/A20.offshore_wind_depth_cap_cost")
    NREL_wind_ctry <- get_data(all_data, "energy/mappings/NREL_wind_ctry") %>%
      select(IAM_country, iso) %>%
      # Former Yugoslavia is assigned to Montenegro (the individual Balkan countries are separate GCAM-Europe regions)
      mutate(iso = if_else(iso == "yug", "mne", iso))
    NREL_offshore_energy <- get_data(all_data, "energy/NREL_offshore_energy")
    offshore_wind_grid_cost <- get_data(all_data, "energy/offshore_wind_grid_cost")
    A20.offshore_wind_types_EUR <- get_data(all_data, "gcam-europe/A20.offshore_wind_types_EUR")
    L1233.globaltech_capital_ATB_offshore_EUR <- get_data(all_data, "L1233.globaltech_capital_ATB_offshore_EUR", strip_attributes = TRUE)
    L1233.globaltech_OMfixed_ATB_offshore_EUR <- get_data(all_data, "L1233.globaltech_OMfixed_ATB_offshore_EUR", strip_attributes = TRUE)

    # Exponent search interval for the smooth renewable resource curve (same as the core chunk)
    OFFSHORE_EXPONENT_INTERVAL <- c(1.0, 15.0)

    # -----------------------------------------------------------------------------
    # Perform computations

    # Europe regions: looked up by name, no hard-coded IDs
    europe_regions <- GCAM_region_names %>%
      filter_regions_europe() %>%
      select(GCAM_region_ID, region)

    # Each depth class must feed exactly one resource, and each resource must be consumed by exactly one technology
    assert_that(!anyDuplicated(A20.offshore_wind_types_EUR$depth_class),
                msg = "A20.offshore_wind_types_EUR: each depth_class must appear once")
    assert_that(all(NREL_offshore_energy$depth_class %in% A20.offshore_wind_types_EUR$depth_class),
                msg = "A20.offshore_wind_types_EUR: some NREL depth classes are not mapped to a resource")
    L120.offshore_wind_params <- A20.offshore_wind_types_EUR %>%
      distinct(resource, technology)
    assert_that(!anyDuplicated(L120.offshore_wind_params$resource),
                msg = "A20.offshore_wind_types_EUR: each resource must use a single technology")

    # FCR and fixed O&M of the technology that consumes each resource (final base year values, as in the core chunk)
    L120.offshore_wind_params <- L120.offshore_wind_params %>%
      left_join_error_no_match(L1233.globaltech_capital_ATB_offshore_EUR %>%
                                 filter(year == MODEL_FINAL_BASE_YEAR) %>%
                                 select(technology, fcr = fixed.charge.rate),
                               by = "technology") %>%
      left_join_error_no_match(L1233.globaltech_OMfixed_ATB_offshore_EUR %>%
                                 filter(year == MODEL_FINAL_BASE_YEAR) %>%
                                 select(technology, OM.fixed),
                               by = "technology")

    # Helper: NREL country data -> GCAM region ID (Europe only)
    nrel_to_europe_region <- function(df) {
      df %>%
        left_join_error_no_match(NREL_wind_ctry, by = "IAM_country") %>%
        left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
        filter(GCAM_region_ID %in% europe_regions$GCAM_region_ID) %>%
        select(-iso)
    }

    # Potential by region / wind class / depth class (PWh -> EJ); zero-potential rows are dropped
    NREL_offshore_energy %>%
      select(-total) %>%
      nrel_to_europe_region() %>%
      gather(wind_class, resource.potential.PWh, -IAM_country, -GCAM_region_ID, -depth_class, -distance_to_shore) %>%
      mutate(resource.potential.EJ = resource.potential.PWh * 1000 * CONV_TWH_EJ) %>%
      group_by(GCAM_region_ID, wind_class, depth_class) %>%
      summarise(resource.potential.EJ = sum(resource.potential.EJ), .groups = "drop") %>%
      filter(resource.potential.EJ != 0) -> L120.offshore_wind_potential_EJ

    # Price of each class (fixed charge on capital + fixed O&M, levelized), then, for each region AND resource type,
    # arrange by price and accumulate supply. See the comment in the core chunk for why offshore wind is sorted by price.
    L120.offshore_wind_potential_EJ %>%
      left_join_error_no_match(A20.wind_class_CFs, by = "wind_class") %>%
      left_join_error_no_match(A20.offshore_wind_depth_cap_cost, by = "depth_class") %>%
      left_join_error_no_match(A20.offshore_wind_types_EUR %>% select(depth_class, resource), by = "depth_class") %>%
      left_join_error_no_match(L120.offshore_wind_params %>% select(resource, fcr, OM.fixed), by = "resource") %>%
      mutate(price = fcr * capital.overnight / CF / CONV_YEAR_HOURS / CONV_KWH_GJ +
               OM.fixed / CF / CONV_YEAR_HOURS / CONV_KWH_GJ) %>%
      group_by(GCAM_region_ID, resource) %>%
      # maximum capacity factor among the classes that actually have potential in this region and resource
      mutate(CFmax = max(CF)) %>%
      arrange(GCAM_region_ID, resource, price) %>%
      mutate(supply = cumsum(resource.potential.EJ)) %>%
      ungroup() %>%
      select(GCAM_region_ID, resource, price, supply, CFmax) -> L120.offshore_wind_matrix

    # Fit of the smooth renewable resource curve for ONE region x resource:
    #   base.price = minimum price; Pvar = price - base.price
    #   mid.price  = price (over base.price) at which 50% of maxSubResource is supplied, estimated by linear
    #                interpolation between the points that surround the 50% point
    #   curve.exponent = exponent that minimises the error of the smooth curve against the supply points
    # If all points lie above the 50% point, the curve is extrapolated linearly (using the first two points) down to a
    # point just below 50% of maxSubResource, so that the mid-price can be computed (price floored at zero).
    # Region x resource pairs that cannot be fitted (a single supply point, or maxSubResource rounding to zero) are
    # dropped: the resource does not exist in that region. The message() below is only shown when the data system runs.
    empty_fit <- tibble::tibble(maxSubResource = numeric(), mid.price = numeric(),
                                curve.exponent = numeric(), CFmax = numeric())
    fit_offshore_curve <- function(pts, key) {
      label <- paste0(europe_regions$region[europe_regions$GCAM_region_ID == key$GCAM_region_ID], " / ", key$resource)
      cf_max <- pts$CFmax[1]
      pts <- pts %>% select(price, supply) %>% arrange(price)

      if(nrow(pts) < 2) {
        message("L120.offshore_wind (EUR): ", label, " has a single supply point; no curve can be fitted, resource dropped")
        return(empty_fit)
      }
      max_sub <- round(max(pts$supply), energy.DIGITS_MAX_SUB_RESOURCE)
      if(max_sub <= 0) {
        message("L120.offshore_wind (EUR): ", label, " has maxSubResource = 0 after rounding, resource dropped")
        return(empty_fit)
      }

      if(!any(pts$supply / max_sub <= energy.WIND_CURVE_MIDPOINT)) {
        slope <- (pts$price[2] - pts$price[1]) / (pts$supply[2] - pts$supply[1])
        new_supply <- max_sub * energy.WIND_CURVE_MIDPOINT * 0.99
        new_price <- max(pts$price[1] + slope * (new_supply - pts$supply[1]), 0)
        pts <- bind_rows(tibble::tibble(price = new_price, supply = new_supply), pts) %>% arrange(price)
      }

      base_price <- min(pts$price)
      pvar <- pts$price - base_price
      pct <- pts$supply / max_sub
      i1 <- max(which(pct <= energy.WIND_CURVE_MIDPOINT))
      i2 <- min(which(pct >= energy.WIND_CURVE_MIDPOINT))
      P1 <- pvar[i1]; Q1 <- pts$supply[i1]; P2 <- pvar[i2]; Q2 <- pts$supply[i2]
      mid <- if(Q2 == Q1) P1 else ((P2 - P1) * max_sub + 2 * Q2 * P1 - 2 * Q1 * P2) / (2 * (Q2 - Q1))
      mid <- round(mid, energy.DIGITS_MAX_SUB_RESOURCE)
      if(!is.finite(mid) || mid <= 0) {
        message("L120.offshore_wind (EUR): ", label, " has a non-positive mid.price; resource dropped")
        return(empty_fit)
      }

      opt <- optimize(f = smooth_res_curve_approx_error, interval = OFFSHORE_EXPONENT_INTERVAL,
                      mid, base_price, max_sub, pts)
      expo <- round(opt$minimum, energy.DIGITS_MAX_SUB_RESOURCE)

      tibble::tibble(maxSubResource = max_sub, mid.price = mid, curve.exponent = expo, CFmax = cf_max)
    }

    # (explicit loop over region x resource pairs: avoids depending on dplyr::group_modify, which is not imported by gcamdata)
    L120.offshore_wind_keys <- L120.offshore_wind_matrix %>% distinct(GCAM_region_ID, resource)
    L120.offshore_wind_fit <- bind_rows(lapply(seq_len(nrow(L120.offshore_wind_keys)), function(i) {
      key <- L120.offshore_wind_keys[i, ]
      pts <- L120.offshore_wind_matrix %>%
        filter(GCAM_region_ID == key$GCAM_region_ID, resource == key$resource)
      fit_offshore_curve(pts, key) %>%
        mutate(GCAM_region_ID = key$GCAM_region_ID, resource = key$resource)
    })) %>%
      select(GCAM_region_ID, resource, maxSubResource, mid.price, curve.exponent, CFmax)

    # Resource curves: only the region x resource pairs that exist
    L120.offshore_wind_fit %>%
      mutate(subresource = resource) %>%
      select(GCAM_region_ID, resource, subresource, maxSubResource, mid.price, curve.exponent) %>%
      arrange(GCAM_region_ID, resource) -> L120.RsrcCurves_EJ_R_offshore_wind_EUR

    # Region-specific (maximum) capacity factor, by resource: used to levelize the grid-connection cost and
    # as the capacity factor of the offshore wind technologies
    L120.offshore_wind_fit %>%
      select(GCAM_region_ID, resource, CFmax) %>%
      arrange(GCAM_region_ID, resource) -> L120.RegCapFactor_offshore_wind_EUR

    # Technological change in the supply curve is related to assumed improvements in capital cost.
    # If capital cost changes from CC to a.CC, every price point of the curve scales by a factor a' given by
    # a' = (k1.a.CC + k2.OM-fixed) / (k1.CC + k2.OM-fixed), k1 = FCR/(CONV_YEAR_HOURS*kWh_GJ), k2 = 1/(CONV_YEAR_HOURS*kWh_GJ)
    # and techChange (the yearly reduction) is 1 - a'^(1/years). Capital trajectory of each resource = that of the offshore wind technology that consumes it.
    # (one resource at a time: several resources may share the same technology)
    offshore_tech_change <- function(offshore_tech, fcr, OM_fixed) {
      L1233.globaltech_capital_ATB_offshore_EUR %>%
        filter(technology == offshore_tech) %>%
        select(year, capital.overnight) %>%
        arrange(year) %>%
        mutate(capital.tech.change.period = dplyr::lag(capital.overnight, 1) / capital.overnight,
               time.change = year - dplyr::lag(year),
               k1 = fcr / (CONV_YEAR_HOURS * CONV_KWH_GJ),
               k2 = 1 / (CONV_YEAR_HOURS * CONV_KWH_GJ),
               tech.change.period = (k1 * capital.tech.change.period * capital.overnight + k2 * OM_fixed) /
                 (k1 * capital.overnight + k2 * OM_fixed),
               tech.change = round(abs(1 - (tech.change.period) ^ (1 / time.change)), energy.DIGITS_TECHCHANGE)) %>%
        filter(!is.na(tech.change), year > MODEL_FINAL_BASE_YEAR) %>%
        select(year, tech.change)
    }
    L120.TechChange_offshore_wind_EUR <- bind_rows(lapply(seq_len(nrow(L120.offshore_wind_params)), function(i) {
      offshore_tech_change(L120.offshore_wind_params$technology[i],
                           L120.offshore_wind_params$fcr[i],
                           L120.offshore_wind_params$OM.fixed[i]) %>%
        mutate(resource = L120.offshore_wind_params$resource[i])
    })) %>%
      select(resource, year, tech.change)

    # Grid connection costs ($1975/GJ), by region and resource. See the comment in the core chunk:
    # share of potential by distance bin x $/kW-km x representative distance of the bin, levelized with the
    # FCR and the capacity factor. The shares are computed WITHIN each resource (fixed / floating).
    NREL_offshore_energy %>%
      select(IAM_country, distance_to_shore, depth_class, total) %>%
      nrel_to_europe_region() %>%
      left_join_error_no_match(A20.offshore_wind_types_EUR %>% select(depth_class, resource), by = "depth_class") %>%
      group_by(GCAM_region_ID, resource, distance_to_shore) %>%
      summarise(total = sum(total), .groups = "drop") %>%
      # keep only the region x resource pairs that exist (also removes the zero-total groups)
      semi_join(L120.offshore_wind_fit, by = c("GCAM_region_ID", "resource")) %>%
      group_by(GCAM_region_ID, resource) %>%
      mutate(share = total / sum(total)) %>%
      ungroup() %>%
      left_join_error_no_match(offshore_wind_grid_cost %>% select(-technology),
                               by = c("distance_to_shore" = "distance_bin")) %>%
      mutate(cost = share * cost_per_kW_km * rep_dist_for_bin) %>%
      group_by(GCAM_region_ID, resource) %>%
      summarise(cost = sum(cost), .groups = "drop") %>%
      left_join_error_no_match(L120.RegCapFactor_offshore_wind_EUR, by = c("GCAM_region_ID", "resource")) %>%
      left_join_error_no_match(L120.offshore_wind_params %>% select(resource, fcr), by = "resource") %>%
      # convert 2013 $/kW to 1975 $/GJ
      mutate(grid.cost = fcr * cost / (CONV_YEAR_HOURS * CFmax * CONV_KWH_GJ) * gdp_deflator(1975, 2013)) %>%
      left_join_error_no_match(europe_regions, by = "GCAM_region_ID") %>%
      select(region, resource, grid.cost) %>%
      arrange(region, resource) -> L120.GridCost_offshore_wind_EUR

    # -----------------------------------------------------------------------------
    # Produce outputs

    L120.RsrcCurves_EJ_R_offshore_wind_EUR %>%
      add_title("Offshore wind resource curves by type (fixed / floating)") %>%
      add_units("maxSubResource: EJ; mid.price: $1975/GJ") %>%
      add_comments("Europe-specific: resource curve by region and offshore wind resource type") %>%
      add_comments("Only region x resource pairs with enough offshore potential to fit a curve are included") %>%
      add_precursors("common/iso_GCAM_regID", "common/GCAM_region_names", "energy/mappings/NREL_wind_ctry",
                     "energy/NREL_offshore_energy", "energy/A20.wind_class_CFs",
                     "energy/A20.offshore_wind_depth_cap_cost", "gcam-europe/A20.offshore_wind_types_EUR",
                     "L1233.globaltech_capital_ATB_offshore_EUR", "L1233.globaltech_OMfixed_ATB_offshore_EUR") ->
      L120.RsrcCurves_EJ_R_offshore_wind_EUR

    L120.TechChange_offshore_wind_EUR %>%
      add_title("Technological change of the offshore wind resources by type") %>%
      add_units("Unitless") %>%
      add_comments("Technological change from the capital cost trajectory of the technology that consumes each resource type") %>%
      add_precursors("gcam-europe/A20.offshore_wind_types_EUR", "L1233.globaltech_capital_ATB_offshore_EUR",
                     "L1233.globaltech_OMfixed_ATB_offshore_EUR") ->
      L120.TechChange_offshore_wind_EUR

    L120.GridCost_offshore_wind_EUR %>%
      add_title("Grid connectivity cost adder for offshore wind by type") %>%
      add_units("$1975/GJ") %>%
      add_comments("Adder by region and offshore wind resource type") %>%
      add_precursors("common/iso_GCAM_regID", "common/GCAM_region_names", "energy/mappings/NREL_wind_ctry",
                     "energy/NREL_offshore_energy", "energy/A20.wind_class_CFs",
                     "energy/A20.offshore_wind_depth_cap_cost", "energy/offshore_wind_grid_cost",
                     "gcam-europe/A20.offshore_wind_types_EUR", "L1233.globaltech_capital_ATB_offshore_EUR",
                     "L1233.globaltech_OMfixed_ATB_offshore_EUR") ->
      L120.GridCost_offshore_wind_EUR

    L120.RegCapFactor_offshore_wind_EUR %>%
      add_title("Region-specific capacity factors for offshore wind by type") %>%
      add_units("Unitless") %>%
      add_comments("Maximum capacity factor among the wind classes with potential, by region and resource type") %>%
      same_precursors_as(L120.RsrcCurves_EJ_R_offshore_wind_EUR) ->
      L120.RegCapFactor_offshore_wind_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
