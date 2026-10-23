# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_all_fgas_emissions_xml
#'
#' Construct XML data structure for \code{all_fgas_emissions_EUR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_fgas_emissions_EUR.xml}, \code{all_fgas_emissions_MAC_EUR.xml}.
#' The corresponding file in the
#' original data system was \code{batch_all_fgas_emissions.xml} (emissions XML).
module_gcameurope_all_fgas_emissions_xml <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "gcam-europe/mappings/grid_regions",
                     "L241.hfc_all_EUR",
                     "L241.pfc_all_EUR",
                     "L241.hfc_future_EUR",
                     "L241.fgas_all_units_EUR",
                     "L252.MAC_higwp_EUR",
                     "L252.MAC_higwp_phaseInTime_EUR",
                     "L252.MAC_higwp_tc_average_EUR",
                     "L244.DeleteThermalService_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_fgas_emissions_EUR.xml",
             XML = "all_fgas_emissions_MAC_EUR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    tech.change <- tech.change.year <- NULL #Silence package check

    # Load required inputs
    grid_regions <- get_data(all_data, "gcam-europe/mappings/grid_regions")
    L241.hfc_all_EUR <- get_data(all_data, "L241.hfc_all_EUR")
    L241.pfc_all_EUR <- get_data(all_data, "L241.pfc_all_EUR")
    L241.hfc_future_EUR <- get_data(all_data, "L241.hfc_future_EUR")
    L241.fgas_all_units_EUR <- get_data(all_data, "L241.fgas_all_units_EUR")
    L252.MAC_higwp_EUR <- get_data(all_data, "L252.MAC_higwp_EUR")
    L252.MAC_higwp_phaseInTime_EUR <- get_data(all_data, "L252.MAC_higwp_phaseInTime_EUR")
    L252.MAC_higwp_tc_average_EUR <- get_data(all_data, "L252.MAC_higwp_tc_average_EUR")
    L244.DeleteThermalService_EUR <- get_data(all_data, "L244.DeleteThermalService_EUR")

    # ===================================================
    # 1. Building cooling sectors that are deleted in a region (e.g. Iceland, no cooling demand) -----------------
    # The building XML deletes these services (L244.DeleteThermalService_EUR), so their emissions, units and MAC
    # curves are dropped here, otherwise GCAM discards them with a warning.
    if (is.null(L244.DeleteThermalService_EUR) || nrow(L244.DeleteThermalService_EUR) == 0) {
      deleted_services <- character(0)
    } else {
      deleted_services <- with(L244.DeleteThermalService_EUR, paste(region, thermal.building.service.input))
    }
    keep_existing_cooling <- function(df) {
      df %>% filter(!(paste(region, supplysector) %in% deleted_services))
    }
    # Safety check: only a few regions can lack cooling services
    dropped_regions <- unique(L241.hfc_all_EUR$region[paste(L241.hfc_all_EUR$region, L241.hfc_all_EUR$supplysector) %in% deleted_services])
    stopifnot(length(dropped_regions) <= 3)

    L241.hfc_all_EUR <- keep_existing_cooling(L241.hfc_all_EUR)
    L241.hfc_future_EUR <- keep_existing_cooling(L241.hfc_future_EUR)
    L241.fgas_all_units_EUR <- keep_existing_cooling(L241.fgas_all_units_EUR)
    L252.MAC_higwp_EUR <- keep_existing_cooling(L252.MAC_higwp_EUR)
    L252.MAC_higwp_phaseInTime_EUR <- keep_existing_cooling(L252.MAC_higwp_phaseInTime_EUR)
    L252.MAC_higwp_tc_average_EUR <- keep_existing_cooling(L252.MAC_higwp_tc_average_EUR)

    # 2. SF6 of electricity_net_ownuse -------------------------------------------------------------------------------
    # The electricity_net_ownuse sector only exists in the grid regions (and in the countries outside any grid, i.e.
    # Iceland and Turkey), not in the countries that belong to a grid region. The country SF6 emissions are added to
    # their grid region (the totals are conserved), and the MAC curves are weighted by emissions so that the
    # grid curve has the same effect as applying each country curve to its own emissions.
    NOU <- "electricity_net_ownuse"
    grid_map <- distinct(grid_regions, region, grid_region)
    is_nou_country <- function(df) df$supplysector == NOU & df$region %in% grid_map$region

    # weights: SF6 emissions of the last year in the table, by country
    nou_weights <- L241.pfc_all_EUR %>%
      filter(supplysector == NOU, Non.CO2 == "SF6", region %in% grid_map$region) %>%
      filter(year == max(year)) %>%
      group_by(region) %>% summarise(w = sum(input.emissions), .groups = "drop")

    # 2a. emissions
    nou_total_before <- sum(L241.pfc_all_EUR$input.emissions[L241.pfc_all_EUR$supplysector == NOU])
    L241.pfc_all_EUR <- bind_rows(
      L241.pfc_all_EUR[!is_nou_country(L241.pfc_all_EUR), ],
      L241.pfc_all_EUR[is_nou_country(L241.pfc_all_EUR), ] %>%
        left_join(grid_map, by = "region") %>%
        mutate(region = grid_region) %>% select(-grid_region) %>%
        group_by(across(-input.emissions)) %>%
        summarise(input.emissions = sum(input.emissions), .groups = "drop"))
    nou_total_after <- sum(L241.pfc_all_EUR$input.emissions[L241.pfc_all_EUR$supplysector == NOU])
    stopifnot(abs(nou_total_before - nou_total_after) < 1e-8)

    # 2b. emission units
    L241.fgas_all_units_EUR <- bind_rows(
      L241.fgas_all_units_EUR[!is_nou_country(L241.fgas_all_units_EUR), ],
      L241.fgas_all_units_EUR[is_nou_country(L241.fgas_all_units_EUR), ] %>%
        left_join(grid_map, by = "region") %>%
        mutate(region = grid_region) %>% select(-grid_region) %>%
        distinct())

    # 2c. MAC curves: emission-weighted (countries without MAC curve count as no reduction)
    nou_grid_totals <- nou_weights %>%
      left_join(grid_map, by = "region") %>%
      group_by(grid_region) %>% summarise(w_total = sum(w), n_total = n(), .groups = "drop")
    mac_nou <- L252.MAC_higwp_EUR[is_nou_country(L252.MAC_higwp_EUR), ] %>%
      left_join(grid_map, by = "region") %>%
      left_join(nou_weights, by = "region") %>%
      left_join(nou_grid_totals, by = "grid_region") %>%
      mutate(w = if_else(is.na(w), 0, w)) %>%
      group_by(across(-c(region, w, mac.reduction, w_total, n_total))) %>%
      summarise(mac.reduction = if_else(first(w_total) > 0,
                                        sum(mac.reduction * w) / first(w_total),
                                        sum(mac.reduction) / first(n_total)),
                .groups = "drop") %>%
      rename(region = grid_region) %>%
      arrange(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control, tax)
    L252.MAC_higwp_EUR <- bind_rows(
      L252.MAC_higwp_EUR[!is_nou_country(L252.MAC_higwp_EUR), ],
      mac_nou[, names(L252.MAC_higwp_EUR)])

    # 2d. MAC parameters (time change, phase-in): same values for all countries, kept once per grid region
    agg_nou_param <- function(df) {
      if (!"supplysector" %in% names(df)) return(df)
      val_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], c("year", "tax"))
      nou <- df[is_nou_country(df), ] %>%
        left_join(grid_map, by = "region") %>%
        mutate(region = grid_region) %>% select(-grid_region) %>%
        group_by(across(-all_of(val_cols))) %>%
        summarise(across(all_of(val_cols), mean), .groups = "drop")
      bind_rows(df[!is_nou_country(df), ], nou[, names(df)])
    }
    L252.MAC_higwp_phaseInTime_EUR <- agg_nou_param(L252.MAC_higwp_phaseInTime_EUR)
    L252.MAC_higwp_tc_average_EUR <- agg_nou_param(L252.MAC_higwp_tc_average_EUR)

    # ===================================================

    # Produce outputs
    create_xml("all_fgas_emissions_EUR.xml") %>%
      add_xml_data(L241.hfc_all_EUR, "StbTechOutputEmissions") %>%
      add_xml_data(L241.pfc_all_EUR, "StbTechOutputEmissions") %>%
      add_xml_data(L241.hfc_future_EUR, "OutputEmissCoeff") %>%
      add_xml_data(L241.fgas_all_units_EUR, "StubTechEmissUnits") %>%
      add_precursors("L241.hfc_all_EUR", "L241.pfc_all_EUR",
                     "L241.hfc_future_EUR", "L241.fgas_all_units_EUR",
                     "gcam-europe/mappings/grid_regions", "L244.DeleteThermalService_EUR") ->
      all_fgas_emissions_EUR.xml

    create_xml("all_fgas_emissions_MAC_EUR.xml") %>%
      add_xml_data(L252.MAC_higwp_EUR, "MAC") %>%
      add_xml_data(L252.MAC_higwp_tc_average_EUR, "MACTC") %>%
      add_xml_data(L252.MAC_higwp_phaseInTime_EUR, "MACPhaseIn") %>%
      add_precursors("L252.MAC_higwp_EUR", "L252.MAC_higwp_tc_average_EUR", "L252.MAC_higwp_phaseInTime_EUR",
                     "gcam-europe/mappings/grid_regions", "L244.DeleteThermalService_EUR") ->
      all_fgas_emissions_MAC_EUR.xml

    return_data(all_fgas_emissions_EUR.xml,
                all_fgas_emissions_MAC_EUR.xml)
  } else {
    stop("Unknown command")
  }
}
