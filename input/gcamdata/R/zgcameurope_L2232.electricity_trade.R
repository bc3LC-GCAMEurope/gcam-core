# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L2232.electricity_trade
#'
#' Generate GCAM-Europe model inputs for electrcity trade sectors at the level of grid regions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs
#' @details This chunk generates input files to create electricity trade and passthrough sectors for the grid regions,
#' and balances electricity supply and demand for each grid region.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter if_else group_by left_join mutate select summarise
#' @author RC Oct 2017
module_gcameurope_L2232.electricity_trade <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "gcam-europe/mappings/grid_regions",
                     FILE = "common/GCAM_region_names",
                     FILE = "energy/A23.sector",
                     FILE = "gcam-europe/A232.structure",
                     FILE = "gcam-europe/elecS_horizontal_to_vertical_map",
                     "L126.in_EJ_R_elecownuse_F_Yh_EUR",
                     "L1261.out_EJ_R_elecownuse_F_Yh_EUR",
                     "L126.in_EJ_R_elecownuse_F_Yh",
                     "L126.in_EJ_R_electd_F_Yh_EUR_grid",
                     "L126.in_EJ_R_electd_F_Yh",
                     "L123.out_EJ_R_indchp_F_Yh_EUR",
                     "L123.out_EJ_R_indchp_F_Yh",
                     "L1232.out_EJ_sR_elec_EUR",
                     "L1235.elecS_demand_fraction_EUR",
                     "L1235.elecS_horizontal_vertical_GCAM_coeff_EUR",
                     "L1261.elec_trade_R_EJ_EUR",
                     "L223.GlobalIntTechBackup_elec",
                     "L223.StubTechCost_offshore_wind")
  MODULE_OUTPUTS <- c("L2232.ElecReserve_elecS_grid_vertical_EUR",
                      "L2232.SubsectorShrwtFllt_elecS_grid_vertical_EUR",
                      "L2232.SubsectorShrwtInterp_elecS_grid_vertical_EUR",
                      "L2232.TechShrwt_elecS_grid_vertical_EUR",
                      "L2232.TechCoef_elecS_grid_vertical_EUR",
                      "L2232.Supplysector_EURelec",
                      "L2232.SubsectorShrwtFllt_EURelec",
                      "L2232.SubsectorInterp_EURelec",
                      "L2232.SubsectorLogit_EURelec",
                      "L2232.TechShrwt_EURelec",
                      "L2232.TechCoef_EURelec",
                      "L2232.Production_exports_EURelec",
                      "L2232.Supplysector_elec_EUR_trade",
                      "L2232.ElecReserve_EUR_trade",
                      "L2232.SubsectorShrwtFllt_elec_EUR_trade",
                      "L2232.SubsectorInterp_elec_EUR_trade",
                      "L2232.SubsectorLogit_elec_EUR_trade",
                      "L2232.TechShrwt_elec_EUR_trade",
                      "L2232.TechCoef_elec_EUR_trade",
                      "L2232.TechCoef_elecownuse_EUR_trade",
                      "L2232.Production_imports_EUR_trade",
                      "L2232.Production_elec_gen_EUR_trade",
                      "L2232.StubTechElecMarket_backup_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    GRID_REGIONS <- unique(grid_regions$grid_region)
    # Add ID to grid regions
    grid_regions <- grid_regions %>% left_join_error_no_match(GCAM_region_names, by = "region")

    # 0. Add in switzerland to eurostat data ---------------------
    L126.in_EJ_R_elecownuse_F_Yh_EUR <- replace_with_eurostat(L126.in_EJ_R_elecownuse_F_Yh, L126.in_EJ_R_elecownuse_F_Yh_EUR) %>%
      filter_regions_europe(regions_to_keep_name = grid_regions$region, region_ID_mapping = GCAM_region_names)

    L126.in_EJ_R_electd_F_Yh_EUR_grid <- replace_with_eurostat(L126.in_EJ_R_electd_F_Yh, L126.in_EJ_R_electd_F_Yh_EUR_grid) %>%
      filter_regions_europe(regions_to_keep_name = grid_regions$region, region_ID_mapping = GCAM_region_names)

    L123.out_EJ_R_indchp_F_Yh_EUR <- replace_with_eurostat(L123.out_EJ_R_indchp_F_Yh, L123.out_EJ_R_indchp_F_Yh_EUR) %>%
      filter_regions_europe(regions_to_keep_name = grid_regions$region, region_ID_mapping = GCAM_region_names)

    # 1a. Vertical sector info --------------
    # Elec reserve margin and average.grid.capacity.factor
    L2232.ElecReserve_elecS_grid_vertical_EUR <- A23.sector %>%
      filter(supplysector == "electricity") %>%
      repeat_add_columns(distinct(grid_regions, grid_region))

    L2232.ElecReserve_elecS_grid_vertical_EUR <- L2232.ElecReserve_elecS_grid_vertical_EUR %>%
      repeat_add_columns(elecS_horizontal_to_vertical_map %>%  distinct(vertical_segment)) %>%
      mutate(supplysector = vertical_segment) %>%
      bind_rows(L2232.ElecReserve_elecS_grid_vertical_EUR) %>%
      select(region = grid_region, supplysector, electricity.reserve.margin, average.grid.capacity.factor)

    # Subsector Shareweight Fillout
    L2232.SubsectorShrwtFllt_elecS_grid_vertical_EUR <- L2232.ElecReserve_elecS_grid_vertical_EUR %>%
      distinct(region, supplysector) %>%
      mutate(subsector = supplysector,
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1)

    # Subsector Shareweight Interp
    L2232.SubsectorShrwtInterp_elecS_grid_vertical_EUR <- L2232.SubsectorShrwtFllt_elecS_grid_vertical_EUR %>%
      distinct(region, supplysector, subsector) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_FUTURE_YEARS),
             interpolation.function = "fixed")

    # Tech sharweight
    L2232.TechShrwt_elecS_grid_vertical_EUR <- L2232.SubsectorShrwtFllt_elecS_grid_vertical_EUR %>%
      distinct(region, supplysector, subsector, share.weight) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subsector) %>%
      select(region, supplysector, subsector, technology, year, share.weight)

    # Tech coef
    L2232.TechCoefMap <- L1235.elecS_demand_fraction_EUR %>%
      mutate(supplysector = "electricity",
             subsector = supplysector,
             technology = supplysector) %>%
      rename(minicam.energy.input = vertical_segment,
             coefficient = demand_fraction) %>%
      bind_rows(L1235.elecS_horizontal_vertical_GCAM_coeff_EUR) %>%
      rename(region = grid_region)

    L2232.TechCoef_elecS_grid_vertical_EUR <- L2232.TechShrwt_elecS_grid_vertical_EUR %>%
      distinct(region, supplysector, subsector, technology, year) %>%
      mutate(market.name = region) %>%
      left_join(L2232.TechCoefMap, by = c("region", "supplysector", "subsector", "technology"))

    L2232.ElecReserve_elecS_grid_vertical_EUR %>%
      distinct(region, supplysector) %>%
      mutate(subsector = supplysector,
             logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcamusa.DEFAULT_LOGITEXP,
             logit.type = gcamusa.GRID_REGION_LOGIT_TYPE) -> L2232.SubsectorLogit_elecS_grid_vertical_EUR

    # 1b: Trade region -----------------------------------------------------------
    # L2232.Supplysector_EURelec: supplysector for electricity trade sector
    # including logit exponent between grid regions
    # All of the supplysector information is the same as before, except the logit exponent
    A232.structure %>%
      filter(region == gcameurope.TRADE_REGION) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = subsector.logit,
             logit.type = subsector.logit.type) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME)) ->
      L2232.Supplysector_EURelec

    # Append vertical segments to the supplysector and subsector logit tables
    L2232.Supplysector_EURelec <- L2232.ElecReserve_elecS_grid_vertical_EUR %>%
      distinct(region, supplysector) %>%
      repeat_add_columns(L2235.Supplysector_elec_USA %>%  select(-region, -supplysector) %>%  distinct) %>%
      bind_rows(L2232.Supplysector_EURelec)

    # L2232.SubsectorShrwtFllt_EURelec: subsector (grid region) share-weights in EUR electricity trade
    # No need to read in subsector logit exponents, which are applied to the technology competition
    A232.structure %>%
      filter(region == gcameurope.TRADE_REGION) %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      repeat_add_columns(tibble(grid_region = GRID_REGIONS)) %>%
      mutate(subsector = paste(grid_region, subsector, sep = " "),
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) ->
      L2232.SubsectorShrwtFllt_EURelec

    # L2232.SubsectorInterp_EURelec: temporal interpolation of subsector share-weights in EUR electricity trade
    # NOTE: this just carries the base year share-weights forward;
    # regions that don't export in the base year don't export at all
    L2232.SubsectorShrwtFllt_EURelec %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L2232.SubsectorInterp_EURelec

    # L2232.SubsectorLogit_EURelec: logit exponent of subsector in EUR electricity trade
    # NOTE: There is only one tech per subsector in the EURelec markets so the logit choice does not matter
    L2232.SubsectorShrwtFllt_EURelec %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      left_join(select(A232.structure, region,
                       logit.exponent = technology.logit,
                       logit.type = technology.logit.type),
                by = "region") %>%
      bind_rows(L2232.SubsectorLogit_elecS_grid_vertical_EUR) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) ->
      L2232.SubsectorLogit_EURelec

    # L2232.TechShrwt_EURelec: technology share-weights in EUR electricity trade
    A232.structure %>%
      filter(region == gcameurope.TRADE_REGION) %>%
      select(LEVEL2_DATA_NAMES[["Tech"]]) %>%
      repeat_add_columns(tibble(grid_region = GRID_REGIONS)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(subsector = paste(grid_region, subsector, sep = " "),
             technology = paste(grid_region, technology, sep = " "),
             share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]], "share.weight", "grid_region") ->
      L2232.TechShrwt_EURelec

    # L2232.TechCoef_EURelec: technology coefficients and market names in EUR electricity trade
    L2232.TechShrwt_EURelec %>%
      left_join_error_no_match(select(A232.structure, region, minicam.energy.input), by = "region") %>%
      mutate(coefficient = 1, market.name = grid_region) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L2232.TechCoef_EURelec

    # 1c: Trade volumes ------------------------
    # Compile flows of electricity in each grid region:
    # generation, cogeneration, ownuse, and consumption by all sectors
    # to calculate exports, imports, and net supply

    # Generation by grid region
    L1232.out_EJ_sR_elec_EUR %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(grid_region, year, generation = value) ->
      L2232.out_EJ_sR_elec_EUR

    # Cogeneration is not included in the grid region totals; need to add it here for balance
    L123.out_EJ_R_indchp_F_Yh_EUR %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(grid_regions, by = "GCAM_region_ID") %>%
      group_by(grid_region, year) %>%
      summarise(cogeneration = sum(value)) %>%
      ungroup ->
      L2232.out_EJ_sR_indchp_F_EUR

    # Calculate net own use in each grid region
    L126.in_EJ_R_elecownuse_F_Yh_EUR %>%
      rename(in_ownuse = value) %>%
      left_join_error_no_match(L1261.out_EJ_R_elecownuse_F_Yh_EUR, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      # Net own use is calculated as total generation minus net outputs
      mutate(net_ownuse = in_ownuse - value) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(grid_regions, by = "GCAM_region_ID") %>%
      group_by(grid_region, year) %>%
      summarise(ownuse = sum(net_ownuse)) %>%
      ungroup ->
      L2232.net_EJ_sR_ownuse_elec_EUR

    # Consumption: the sum of all demands in each grid region, equal to the input to the elect_td sectors
    L126.in_EJ_R_electd_F_Yh_EUR_grid %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(grid_regions, by = "GCAM_region_ID") %>%
      group_by(grid_region, year) %>%
      summarise(consumption = sum(value)) %>%
      ungroup ->
      L2232.in_EJ_sR_td_elec_EUR

    # Complie all flows and calculate exports, imports and net supply
    L2232.TechShrwt_EURelec %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L2232.out_EJ_sR_elec_EUR, by = c("grid_region", "year")) %>%
      left_join_error_no_match(L2232.out_EJ_sR_indchp_F_EUR, by = c("grid_region", "year")) %>%
      left_join_error_no_match(L2232.net_EJ_sR_ownuse_elec_EUR, by = c("grid_region", "year")) %>%
      left_join_error_no_match(L2232.in_EJ_sR_td_elec_EUR, by = c("grid_region", "year")) %>%
      # Calculate net exports: generation + cogeneration - ownuse - consumption
      mutate(net.exports.CAL = generation + cogeneration - ownuse - consumption) %>%
      # add in trade balance and scale gross imports/exports based on net export scaling
      left_join_error_no_match(L1261.elec_trade_R_EJ_EUR, by = c("grid_region", "year")) %>%
      # if signs are correct, simply scale exports/imports by net.exports.CAL / net_exports
      # otherwise, only scale one of exports/imports
      mutate(
        exports = case_when(
          sign(net.exports.CAL) == sign(net_exports) ~ exports * net.exports.CAL / net_exports,
          net.exports.CAL > 0 & net_exports < 0 ~ exports + net.exports.CAL - net_exports,
          TRUE ~ exports),
        imports = case_when(
          sign(net.exports.CAL) == sign(net_exports) ~ imports * net.exports.CAL / net_exports,
          net.exports.CAL < 0 & net_exports > 0 ~ imports + net_exports - net.exports.CAL,
          TRUE ~  imports),
        net_exports_recalc = exports - imports,
        # Calculate consumption from domestic sources: total consumption minus gross imports
        net.supply = consumption - imports) ->
      L2232.elec_flows_EUR

    stopifnot(all(round(L2232.elec_flows_EUR$net.exports.CAL,
                        energy.DIGITS_CALOUTPUT) == round(L2232.elec_flows_EUR$net_exports_recalc,
                                                          energy.DIGITS_CALOUTPUT)))

    # L2232.Production_exports_EURelec: calibrated exports of electricity from grid regions to shared EUR region
    L2232.elec_flows_EUR %>%
      mutate(calOutputValue = round(exports, digits = energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L2232.Production_exports_EURelec

    # 2a: GRID REGIONS sector/subsector ----------------------------------------------
    # Some of the information read in about these regions is in the primary electricity_EUR code file
    # Create the grid region structure tibble
    A232.structure %>%
      filter(region == "grid_region") %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = GRID_REGIONS)) %>%
      mutate(market.name = if_else(market.name == "grid_region", region,market.name)) ->
      A232.GRIDstructure

    # L2232.Supplysector_elec_EUR_trade: supplysector information for electricity passthrough sectors in the EURelec regions
    A232.GRIDstructure %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = subsector.logit,
             logit.type = subsector.logit.type) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME)) ->
      L2232.Supplysector_elec_EUR_trade

    # L2232.ElecReserve_EUR_trade: electricity reserve margin and avg grid capacity factor in the grid regions
    A23.sector %>%
      filter(supplysector == "electricity") %>%
      repeat_add_columns(tibble(region = GRID_REGIONS)) %>%
      select(LEVEL2_DATA_NAMES[["ElecReserve"]]) ->
      L2232.ElecReserve_EUR_trade

    # L2232.SubsectorShrwtFllt_elec_EUR_trade: subsector (states) share-weights
    # for electricity passthrough sectors in grid regions
    A232.GRIDstructure %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS), share.weight = 1) ->
      L2232.SubsectorShrwtFllt_elec_EUR_trade

    # L2232.SubsectorInterp_elec_EUR_trade: temporal interpolation of subsector (states) share-weights
    # for electricity passthrough sectors in grid regions
    L2232.SubsectorShrwtFllt_elec_EUR_trade %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L2232.SubsectorInterp_elec_EUR_trade

    # L2232.SubsectorShrwtFllt_elec_EUR_trade: logit exponent of subsector (states) in grid regions
    # NOTE: There is only one tech per subsector in the EUR_trade markets so the logit choice does not matter
    L2232.SubsectorShrwtFllt_elec_EUR_trade %>%
      left_join(A232.GRIDstructure %>%
                  select(region, technology.logit, technology.logit.type) %>%
                  unique, by = "region") %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = technology.logit,
             logit.type = technology.logit.type) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) ->
      L2232.SubsectorLogit_elec_EUR_trade

    # 2b: GRID REGIONS technology ----------------------------------------------
    # L2232.TechShrwt_elec_EUR_trade: technology share-weights in grid regions
    A232.GRIDstructure %>%
      select(LEVEL2_DATA_NAMES[["Tech"]]) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) ->
      L2232.TechShrwt_elec_EUR_trade

    # L2232.TechCoef_elec_EUR_trade: technology coefficients and market names for domestic supply in grid regions
    A232.GRIDstructure %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # Own use coefficients will be done separately; delete from the table here
      filter(supplysector != "electricity_net_ownuse") %>%
      mutate(coefficient = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L2232.TechCoef_elec_EUR_trade

    # L2232.TechCoef_elecownuse_EUR_trade: own use coefficients in the grid regions
    L2232.elec_flows_EUR %>%
      # Own use coefficients are total generation divided by total generation minus own use
      mutate(ownuse_coef = (generation + cogeneration) / (generation + cogeneration - ownuse)) ->
      L2232.elec_flows_EUR

    A232.GRIDstructure %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(supplysector == "electricity_net_ownuse") %>%
      left_join(select(L2232.elec_flows_EUR, grid_region, year, coefficient = ownuse_coef),
                by = c("region" = "grid_region", "year")) %>%
      group_by(region) %>%
      # Set future year own use coefficients the same as the base year coefficients
      mutate(coefficient = replace(coefficient, year %in% MODEL_FUTURE_YEARS, coefficient[year == max(MODEL_BASE_YEARS)])) %>%
      ungroup %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L2232.TechCoef_elecownuse_EUR_trade

    # 2c. GRID regions calibrations ------------------
    # L2232.Production_imports_EUR_trade: calibrated electricity imports (from EUR region)
    L2232.TechCoef_elec_EUR_trade %>%
      filter(year %in% MODEL_BASE_YEARS, market.name == gcameurope.TRADE_REGION) %>%
      left_join_error_no_match(select(L2232.elec_flows_EUR, grid_region, year, imports),
                               by = c("region" = "grid_region", "year")) %>%
      mutate(calOutputValue = round(imports, digits = energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L2232.Production_imports_EUR_trade

    # L2232.Production_elec_gen_EUR_trade: calibrated net electricity generation (from within grid region)
    L2232.TechCoef_elec_EUR_trade %>%
      filter(year %in% MODEL_BASE_YEARS, market.name != gcameurope.TRADE_REGION) %>%
      left_join_error_no_match(select(L2232.elec_flows_EUR, grid_region, year, net.supply),
                               by = c("region" = "grid_region", "year")) %>%
      mutate(calOutputValue = round(net.supply, digits = energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L2232.Production_elec_gen_EUR_trade

    # 3: L2232.StubTechElecMarket_backup_EUR ----------------------------------
    # L2232.StubTechElecMarket_backup_EUR: electric sector name for states
    # Reset the electric sector market to the grid regions (for backup calculations)
    L223.GlobalIntTechBackup_elec %>%
      distinct(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
      repeat_add_columns(select(grid_regions, region)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_error_no_match(select(grid_regions, electric.sector.market = grid_region, region),
                               by = c("region")) %>%
      filter(!(stub.technology %in% L223.StubTechCost_offshore_wind$stub.technology &
               !region %in% L223.StubTechCost_offshore_wind$region),
             !(stub.technology %in% L223.StubTechCost_offshore_wind$stub.technology &
                 region == "Slovenia")) ->
      L2232.StubTechElecMarket_backup_EUR


    # Produce outputs -----------------------------------------------
    L2232.ElecReserve_elecS_grid_vertical_EUR  %>%
      add_title("Vertical load segments GCAM_Europe") %>%
      add_units("unitless") %>%
      add_precursors("energy/A23.sector", "gcam-europe/elecS_horizontal_to_vertical_map") ->
      L2232.ElecReserve_elecS_grid_vertical_EUR

    L2232.SubsectorShrwtFllt_elecS_grid_vertical_EUR  %>%
      add_title("Vertical load segments GCAM_Europe") %>%
      add_units("unitless") %>%
      same_precursors_as(L2232.ElecReserve_elecS_grid_vertical_EUR) ->
      L2232.SubsectorShrwtFllt_elecS_grid_vertical_EUR

    L2232.SubsectorShrwtInterp_elecS_grid_vertical_EUR  %>%
      add_title("Vertical load segments GCAM_Europe") %>%
      add_units("unitless") %>%
      same_precursors_as(L2232.ElecReserve_elecS_grid_vertical_EUR) ->
      L2232.SubsectorShrwtInterp_elecS_grid_vertical_EUR

    L2232.TechShrwt_elecS_grid_vertical_EUR  %>%
      add_title("Vertical load segments GCAM_Europe") %>%
      add_units("unitless") %>%
      same_precursors_as(L2232.ElecReserve_elecS_grid_vertical_EUR) ->
      L2232.TechShrwt_elecS_grid_vertical_EUR

    L2232.TechCoef_elecS_grid_vertical_EUR  %>%
      add_title("Vertical load segments GCAM_Europe") %>%
      add_units("unitless") %>%
      same_precursors_as(L2232.ElecReserve_elecS_grid_vertical_EUR) ->
      L2232.TechCoef_elecS_grid_vertical_EUR

    L2232.Supplysector_EURelec %>%
      add_title("Supplysector for electricity sector in the EUR regions") %>%
      add_units("Unitless") %>%
      add_comments("All of the supplysector information is the same as before") %>%
      add_comments("except including logit exponent between grid regions") %>%
      add_precursors("gcam-europe/A232.structure") ->
      L2232.Supplysector_EURelec

    L2232.SubsectorShrwtFllt_EURelec %>%
      add_title("Subsector (grid region) share-weights in EUR electricity trade") %>%
      add_units("Unitless") %>%
      add_comments("No need to read in subsector logit exponents, which are applied to the technology competition") %>%
      add_precursors("gcam-europe/mappings/grid_regionss",
                     "gcam-europe/A232.structure") ->
      L2232.SubsectorShrwtFllt_EURelec

    L2232.SubsectorInterp_EURelec %>%
      add_title("Table headers for temporal interpolation of subsector (grid region) share-weights in EUR electricity trade") %>%
      add_units("Unitless") %>%
      add_comments("This just carries the base year share-weights forward") %>%
      add_comments("Regions that don't export in the base year don't export at all") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_EURelec") ->
      L2232.SubsectorInterp_EURelec

    L2232.SubsectorLogit_EURelec %>%
      add_title("Logit exponent of subsector (grid region) in EUR electricity trade") %>%
      add_units("Unitless") %>%
      add_comments("There is only one tech per subsector, so the logit choice does not matter") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_EURelec") ->
      L2232.SubsectorLogit_EURelec

    L2232.TechShrwt_EURelec %>%
      add_title("Technology share-weights in the EUR electricity trade") %>%
      add_units("Unitless") %>%
      add_comments("Set the same value across all model years") %>%
      add_precursors("gcam-europe/A232.structure",
                     "gcam-europe/mappings/grid_regionss") ->
      L2232.TechShrwt_EURelec

    L2232.TechCoef_EURelec %>%
      add_title("Technology coefficients and market names in the EUR electricity trade") %>%
      add_units("Unitless") %>%
      add_comments("Set the same value across all model years") %>%
      add_comments("Set grid region as market name") %>%
      same_precursors_as("L2232.TechShrwt_EURelec") ->
      L2232.TechCoef_EURelec

    L2232.Production_exports_EURelec %>%
      add_title("Calibrated exports of electricity from grid regions to shared EUR region") %>%
      add_units("EJ") %>%
      add_comments("Electricity net exports are calulated as total generation minus consumption and own use") %>%
      add_comments("Cogeneration is included in total generation") %>%
      add_comments("exports are scaled from L1261.elec_trade_R_EJ_EUR") %>%
      add_precursors(MODULE_INPUTS) ->
      L2232.Production_exports_EURelec

    L2232.Supplysector_elec_EUR_trade %>%
      add_title("Supplysector information for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_precursors("gcam-europe/A232.structure",
                     "gcam-europe/mappings/grid_regionss") ->
      L2232.Supplysector_elec_EUR_trade

    L2232.ElecReserve_EUR_trade %>%
      add_title("Electricity reserve margin and avg grid capacity factor in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("For electricity sector") %>%
      add_precursors("energy/A23.sector",
                     "gcam-europe/mappings/grid_regionss") ->
      L2232.ElecReserve_EUR_trade

    L2232.SubsectorShrwtFllt_elec_EUR_trade %>%
      add_title("Subsector (states) share-weights for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_precursors("gcam-europe/A232.structure",
                     "gcam-europe/mappings/grid_regionss") ->
      L2232.SubsectorShrwtFllt_elec_EUR_trade

    L2232.SubsectorInterp_elec_EUR_trade %>%
      add_title("Table header of temporal interpolation of subsector (states) share-weights for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_elec_EUR_trade") ->
      L2232.SubsectorInterp_elec_EUR_trade

    L2232.SubsectorLogit_elec_EUR_trade %>%
      add_title("Logit exponent of subsector (states) for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_elec_EUR_trade") ->
      L2232.SubsectorLogit_elec_EUR_trade

    L2232.TechShrwt_elec_EUR_trade %>%
      add_title("Technology share-weights for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_precursors("gcam-europe/A232.structure",
                     "gcam-europe/mappings/grid_regionss") ->
      L2232.TechShrwt_elec_EUR_trade

    L2232.TechCoef_elec_EUR_trade %>%
      add_title("Technology coefficients and market names for electricity domestic supply in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Coefficients for electricity domestic supply") %>%
      same_precursors_as("L2232.TechShrwt_elec_EUR_trade") ->
      L2232.TechCoef_elec_EUR_trade

    L2232.TechCoef_elecownuse_EUR_trade %>%
      add_title("Electricity own use coefficients in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Coefficients are calculated as total generation devided by total generation minus own use") %>%
      add_comments("Cogeneration is included in total generation") %>%
      add_comments("Set the coefficients for future years the same as in the model base year") %>%
      same_precursors_as("L2232.Production_exports_EURelec") ->
      L2232.TechCoef_elecownuse_EUR_trade

    L2232.Production_imports_EUR_trade %>%
      add_title("Calibrated electricity imports (from other grid regions)") %>%
      add_units("EJ") %>%
      add_comments("Electricity net exports are calulated as total generation minus consumption and own use") %>%
      add_comments("Cogeneration is included in total generation") %>%
      add_comments("Net own use is calculated as total generation minus net outputs") %>%
      add_comments("Consumption equals to the sum of input to the elect_td sectors") %>%
      same_precursors_as("L2232.Production_exports_EUR_trade") ->
      L2232.Production_imports_EUR_trade

    L2232.Production_elec_gen_EUR_trade %>%
      add_title("Calibrated domestic electricity supply (from within grid region)") %>%
      add_units("EJ") %>%
      add_comments("Net electricity generation is calculated as total consumption minus imports") %>%
      same_precursors_as("L2232.Production_exports_EUR_trade") ->
      L2232.Production_elec_gen_EUR_trade

    L2232.StubTechElecMarket_backup_EUR %>%
      add_title("Electric sector name for states") %>%
      add_units("Unitless") %>%
      add_comments("Reset the electric sector market to the grid regions (for backup calculations)") %>%
      add_precursors("L223.GlobalIntTechBackup_elec",
                     "gcam-europe/mappings/grid_regions") ->
      L2232.StubTechElecMarket_backup_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
