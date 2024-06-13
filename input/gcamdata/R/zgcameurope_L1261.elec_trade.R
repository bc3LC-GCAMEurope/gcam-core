# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L1261.elec_trade
#'
#' Adjust electricity ownuse by net trade with regions outside of grid. Unadjusted trade balances.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by left_join mutate select summarise
#' @importFrom tidyr replace_na pivot_longer
#' @author RH June 2024
module_gcameurope_L1261.elec_trade <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "common/GCAM32_to_EU",
                     FILE = "gcam-europe/mappings/grid_regions",
                     FILE = "gcam-europe/eurostat_elec_exports",
                     FILE = "gcam-europe/eurostat_elec_imports",
                     "L126.in_EJ_R_elecownuse_F_Yh_EUR",
                     "L126.out_EJ_R_elecownuse_F_Yh_EUR",
                     "L126.in_EJ_R_electd_F_Yh_EUR",
                     "L126.out_EJ_R_electd_F_Yh_EUR")
  MODULE_OUTPUTS <- c("L1261.out_EJ_R_elecownuse_F_Yh_EUR",
                      "L1261.IO_R_elecownuse_F_Yh_EUR",
                      "L1261.elec_trade_R_EJ_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # 0. Mappings to eurostat trade balance ----------------------
    # need to map grid regions to eurostat data
    country_code_mapping <- distinct(eurostat_elec_exports, code = code_exporter, region = export_ctry) %>%
      bind_rows(distinct(eurostat_elec_imports, code = code_importer, region = import_ctry)) %>%
      distinct() %>%
      # make some adjustments to certain countries
      mutate(region = gsub("Czechia", "Czech Republic", region),
             region = gsub("Netherlands \\(the\\)", "Netherlands", region),
             region = gsub("North Macedonia", "Macedonia", region),
             region = gsub("^Serbia", "Serbia and Montenegro", region),
             region = gsub("^Montenegro", "Serbia and Montenegro", region),
             region = gsub("United Kingdom", "UK", region))

    grid_region_mapping <- grid_regions %>%
      left_join(country_code_mapping, by = "region") %>%
      # add Andorra and Liechtenstein
      bind_rows(tibble(grid_region = c("Iberian_Peninsula", "Central_Western_Europe"),
                       code = c("AD", "LI"),
                       region = c("Andorra", "Liechtenstein")))

    # 1a. L126 Net exports outside of grid regions -------------------------
    # check difference between total generation and total demand
    # This difference should be equivalent to net exports to countries outside of european grid regions
    # Difference will need to be taken up by ownuse - we will use the net exports in the eurostat trade balances
    # to decide which countries to adjust
    total_gen <- L126.in_EJ_R_elecownuse_F_Yh_EUR %>%
      group_by(year) %>%
      summarise(gen = sum(value)) %>%
      ungroup

    total_demand <- L126.in_EJ_R_electd_F_Yh_EUR %>%
      # Demand includes ownuse, calculating here the net ownuse consumption as in - out
      bind_rows(L126.in_EJ_R_elecownuse_F_Yh_EUR,
                L126.out_EJ_R_elecownuse_F_Yh_EUR %>%  mutate(value = -1 * value)) %>%
      group_by(year) %>%
      summarise(demand = sum(value)) %>%
      ungroup

    excess_gen <- total_gen %>%
      left_join_error_no_match(total_demand, by = "year") %>%
      mutate(net_export = gen - demand)

    # 1b. Eurostat trade balance net exports --------------------------
    # combine exports and imports and remove duplicated rows
    L1261.elec_trade_GWH <- bind_rows(eurostat_elec_exports, eurostat_elec_imports) %>%  distinct() %>%
      left_join(grid_region_mapping %>%  rename(grid_region_exporter = grid_region, region_exporter = region), by = c("code_exporter" = "code")) %>%
      left_join(grid_region_mapping %>%  rename(grid_region_importer = grid_region, region_importer = region), by = c("code_importer" = "code")) %>%
      mutate(region_exporter = if_else(is.na(region_exporter), export_ctry, region_exporter),
             region_importer = if_else(is.na(region_importer), import_ctry, region_importer)) %>%
      # If trade partner is not part of a grid region, label it as EXTERIOR
      tidyr::replace_na(list(grid_region_exporter = "EXTERIOR", grid_region_importer = "EXTERIOR")) %>%
      gather_years() %>%
      filter(!is.na(value), value != 0) %>%
      # often the import and export balances have slight differences (e.g. exports from Spain to Portugal != imports in Portugal from Spain)
      # so when there are two values for same importers and exporters, just take average
      group_by(region_importer, region_exporter, grid_region_exporter, grid_region_importer, year) %>%
      summarise(value = mean(value)) %>%
      ungroup

    # isolate to trade with exterior regions
    L1261.elec_trade_grid_EJ <- L1261.elec_trade_GWH %>%
      filter(grid_region_exporter == "EXTERIOR" | grid_region_importer == "EXTERIOR",
             # not interested in, for example, trade between turkey and iraq, two regions outside of the grid regions
             !(grid_region_exporter == "EXTERIOR" & grid_region_importer == "EXTERIOR")) %>%
      # summarise and convert from GWH to EJ
      group_by(region_importer, grid_region_importer, region_exporter, grid_region_exporter, year) %>%
      summarise(value = sum(value) * CONV_GWH_EJ) %>%
      ungroup

    L1261.net_exterior_exports_EJ <- L1261.elec_trade_grid_EJ %>%
      # set imports to negative value
      mutate(value = if_else(grid_region_exporter == "EXTERIOR", -1 * value, value),
             region_importer = if_else(grid_region_importer == "EXTERIOR", "EXTERIOR", region_importer),
             region_exporter = if_else(grid_region_exporter == "EXTERIOR", "EXTERIOR", region_exporter)) %>%
      select(region_importer, region_exporter, year, value) %>%
      tidyr::pivot_longer(cols = c(region_importer, region_exporter), names_to = "trade", values_to = "region") %>%
      filter(region != "EXTERIOR") %>%
      group_by(year, region) %>%
      summarise(net_exports = sum(value)) %>%
      ungroup %>%
      # need to fill in 1971-1989, using 1990 values, but don't want to interpolate other missing values, just set to 0
      complete(region, year = excess_gen$year, fill = list(net_exports = 0)) %>%
      group_by(region) %>%
      mutate(net_exports = if_else(year < min(L1261.elec_trade_grid_EJ$year), net_exports[year == min(L1261.elec_trade_grid_EJ$year)], net_exports)) %>%
      ungroup

    # 1c Ownuse adjust ------------------------------
    L1261.ownuse_adjustments <- L1261.net_exterior_exports_EJ %>%
      filter(year %in% excess_gen$year) %>%
      left_join_error_no_match(excess_gen %>%  select(year, cal_net_exports = net_export), by = "year") %>%
      # only sharing with countries with same sign as global
      # this minimizes impacts of the adjustments
      # now calculate share of net exports to allow allocation to countries
      group_by(year) %>%
      # if the sign of the sum of net exports and the calibrated net exports are not the same, let's filter out the opposite sign
      filter(!(sign(sum(net_exports)) != sign(cal_net_exports) & sign(net_exports) != sign(cal_net_exports))) %>%
      mutate(ownuse_adj = net_exports * cal_net_exports / sum(net_exports)) %>%
      ungroup %>%
      left_join_error_no_match(distinct(GCAM32_to_EU, country_name, GCAM_region_ID) , by = c("region" = "country_name")) %>%
      select(year, GCAM_region_ID, ownuse_adj)

    L1261.out_EJ_R_elecownuse_F_Yh_EUR <- L126.out_EJ_R_elecownuse_F_Yh_EUR %>%
      left_join(L1261.ownuse_adjustments, by = c("GCAM_region_ID", "year")) %>%
      tidyr::replace_na(list(ownuse_adj = 0)) %>%
      mutate(value = value - ownuse_adj) %>%
      select(-ownuse_adj)

    # 1d Check that net_exports outside of grids are now 0 ---------------
    total_demand_ownuse_adj <- L126.in_EJ_R_electd_F_Yh_EUR %>%
      # Demand includes ownuse, calculating here the net ownuse consumption as in - out
      bind_rows(L126.in_EJ_R_elecownuse_F_Yh_EUR,
                L1261.out_EJ_R_elecownuse_F_Yh_EUR %>%  mutate(value = -1 * value)) %>%
      group_by(year) %>%
      summarise(demand = sum(value)) %>%
      ungroup

    excess_gen_ownuse_adj <- total_gen %>%
      left_join_error_no_match(total_demand_ownuse_adj, by = "year") %>%
      mutate(net_export = gen - demand)

    stopifnot(all(round(excess_gen_ownuse_adj$net_export, energy.DIGITS_CALOUTPUT) == 0))

    # 1e Adjust IO ---------------------------
    L1261.IO_R_elecownuse_F_Yh_EUR <- L126.in_EJ_R_elecownuse_F_Yh_EUR %>%
      left_join_error_no_match(L1261.out_EJ_R_elecownuse_F_Yh_EUR, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(value = value.x / value.y) %>%
      select(-value.x, -value.y)

    # 2. Produce unadjusted trade balances between grid regions ---------------
    L1261.elec_trade_R_EJ_EUR_pre <- L1261.elec_trade_GWH %>%
      # Only want trade between grid_regions
      filter(grid_region_exporter != "EXTERIOR", grid_region_importer != "EXTERIOR",
             grid_region_exporter != grid_region_importer) %>%
      group_by(grid_region_exporter, grid_region_importer, year) %>%
      summarise(value = sum(value, na.rm = T) * CONV_GWH_EJ) %>%
      ungroup

    # pulling out exports and imports
    # if we want bilateral trade, would want to leave L1261.elec_trade_R_EJ_EUR_pre, with extrapolation to pre-1990
    L1261.elec_exports_R_EJ_EUR <- L1261.elec_trade_R_EJ_EUR_pre %>%
      group_by(grid_region = grid_region_exporter, year) %>%
      summarise(exports = sum(value)) %>%
      ungroup

    L1261.elec_imports_R_EJ_EUR <- L1261.elec_trade_R_EJ_EUR_pre %>%
      group_by(grid_region = grid_region_importer, year) %>%
      summarise(imports = sum(value)) %>%
      ungroup

    L1261.elec_trade_R_EJ_EUR <- L1261.elec_exports_R_EJ_EUR %>%
      left_join_error_no_match(L1261.elec_imports_R_EJ_EUR, by = c("grid_region", "year")) %>%
      mutate(net_exports = exports - imports) %>%
      # need to fill in 1971-1989, using 1990 values, but don't want to interpolate other missing values, just set to 0
      complete(grid_region, year = L126.in_EJ_R_elecownuse_F_Yh_EUR$year, fill = list(exports = 0, imports = 0, net_exports = 0)) %>%
      group_by(grid_region) %>%
      mutate(net_exports = if_else(year < min(L1261.elec_trade_R_EJ_EUR_pre$year), net_exports[year == min(L1261.elec_trade_R_EJ_EUR_pre$year)], net_exports),
             exports = if_else(year < min(L1261.elec_trade_R_EJ_EUR_pre$year), exports[year == min(L1261.elec_trade_R_EJ_EUR_pre$year)], exports),
             imports = if_else(year < min(L1261.elec_trade_R_EJ_EUR_pre$year), imports[year == min(L1261.elec_trade_R_EJ_EUR_pre$year)], imports)) %>%
      ungroup

    # OUTPUTS ===================================================
    L1261.out_EJ_R_elecownuse_F_Yh_EUR %>%
      add_title("Ownuse output adjusted to take into account net trade outside of grid regions") %>%
      add_units("EJ") %>%
      add_precursors(MODULE_INPUTS) ->
      L1261.out_EJ_R_elecownuse_F_Yh_EUR

    L1261.IO_R_elecownuse_F_Yh_EUR %>%
      add_title("IO adjusted to take into account net trade outside of grid regions") %>%
      add_units("NA") %>%
      add_precursors(MODULE_INPUTS) ->
      L1261.IO_R_elecownuse_F_Yh_EUR

    L1261.elec_trade_R_EJ_EUR %>%
      add_title("Gross and net exports/imports by grid region") %>%
      add_units("EJ") %>%
      add_precursors(MODULE_INPUTS) ->
      L1261.elec_trade_R_EJ_EUR


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
