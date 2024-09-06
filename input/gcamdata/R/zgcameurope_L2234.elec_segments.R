# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L2234.elec_segments
#'
#' Generates GCAM-Europe model inputs for multiple load segments electricity sector by country
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs
#' @details This chunk generates input files to create an electricity generation sector with multiple load segments
#' for each state and creates the demand for the state-level electricity sectors in the grid regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join distinct filter if_else mutate select semi_join summarise_if bind_rows
#' @importFrom tidyr complete nesting replace_na pivot_longer
#' @importFrom tibble tibble
#' @author MTB Aug 2018
module_gcameurope_L2234.elec_segments <- function(command, ...) {
  # these are the inputs that don't need any adjustment other than to add segments and change tech name
  AUTO_EXPAND_INPUTS <- c("L223.PrimaryRenewKeyword_elec", "L223.PrimaryRenewKeywordInt_elec", "L223.AvgFossilEffKeyword_elec",
                          "L223.GlobalTechCapital_elec", "L223.GlobalIntTechCapital_elec",
                          "L223.GlobalTechOMfixed_elec", "L223.GlobalIntTechOMfixed_elec", "L223.GlobalTechOMvar_elec", "L223.GlobalIntTechOMvar_elec",
                          "L223.GlobalTechEff_elec", "L223.GlobalIntTechEff_elec",
                          "L223.GlobalTechLifetime_elec", "L223.GlobalIntTechLifetime_elec",
                          "L223.GlobalTechSCurve_elec", "L223.GlobalTechProfitShutdown_elec",
                          "L223.GlobalTechCapture_elec",
                          "L223.GlobalIntTechBackup_elec")
  MODULE_INPUTS <- c(FILE = "gcam-europe/mappings/grid_regions",
                     FILE = "common/GCAM_region_names",
                     FILE = "energy/A23.sector",
                     FILE = "energy/A23.globaltech_shrwt",
                     FILE = "gcam-europe/A23.elecS_subsector_logit",
                     FILE = "gcam-europe/A23.elecS_subsector_shrwt",
                     FILE = "gcam-europe/A23.elecS_subsector_shrwt_interp",
                     FILE = "gcam-europe/A23.elecS_subsector_shrwt_interpto",
                     FILE = "gcam-europe/A23.elecS_globaltech_shrwt",
                     FILE = "gcam-europe/A23.elecS_globalinttech_shrwt",
                     FILE = "gcam-europe/A23.elecS_stubtech_energy_inputs",
                     FILE = "gcam-europe/A23.elecS_naming",
                     FILE = "gcam-europe/elecS_time_fraction",
                     "L113.elecS_globaltech_capital_battery_ATB",
                     "L1239.R_elec_supply",
                     "L223.StubTechEff_elec_EUR",
                     "L223.StubTechCalInput_elec_EUR",
                     "L223.StubTechProd_elec_EUR",
                     "L223.StubTechFixOut_elec_EUR",
                     "L223.StubTechFixOut_hydro_EUR",
                     "L223.StubTechCapFactor_elec_EUR",
                     "L223.GlobalTechCapFac_elec",
                     "L123.eff_R_elec_F_Yh",
                     "L223.StubTechCost_offshore_wind_EUR",
                     AUTO_EXPAND_INPUTS)
  MODULE_OUTPUTS <- c("L2234.Supplysector_elecS_EUR",
                      "L2234.ElecReserve_elecS_EUR",
                      "L2234.SubsectorLogit_elecS_EUR",
                      "L2234.SubsectorShrwtInterp_elecS_EUR",
                      "L2234.SubsectorShrwtInterpTo_elecS_EUR",
                      "L2234.SubsectorShrwt_elecS_EUR",
                      "L2234.SubsectorShrwtFllt_elecS_grid_EUR",
                      "L2234.SubsectorShrwtInterp_elecS_grid_EUR",
                      "L2234.GlobalTechShrwt_elecS_EUR",
                      "L2234.PrimaryRenewKeyword_elecS_EUR",
                      "L2234.PrimaryRenewKeywordInt_elecS_EUR",
                      "L2234.AvgFossilEffKeyword_elecS_EUR",
                      "L2234.GlobalTechCapital_elecS_EUR",
                      "L2234.GlobalIntTechCapital_elecS_EUR",
                      "L2234.GlobalTechOMfixed_elecS_EUR",
                      "L2234.GlobalIntTechOMfixed_elecS_EUR",
                      "L2234.GlobalTechOMvar_elecS_EUR",
                      "L2234.GlobalIntTechOMvar_elecS_EUR",
                      "L2234.GlobalTechCapFac_elecS_EUR",
                      "L2234.GlobalTechEff_elecS_EUR",
                      "L2234.GlobalIntTechEff_elecS_EUR",
                      "L2234.GlobalTechLifetime_elecS_EUR",
                      "L2234.GlobalIntTechLifetime_elecS_EUR",
                      "L2234.GlobalTechProfitShutdown_elecS_EUR",
                      "L2234.GlobalTechSCurve_elecS_EUR",
                      "L2234.GlobalIntTechSCurve_elecS_EUR",
                      "L2234.GlobalTechCapture_elecS_EUR",
                      "L2234.GlobalIntTechBackup_elecS_EUR",
                      "L2234.PassThroughSector_elecS_EUR",
                      "L2234.PassThroughTech_elecS_grid_EUR",
                      "L2234.StubTechEff_elecS_EUR",
                      "L2234.StubTechEff_battery_elecS_EUR",
                      "L2234.StubTechCapFactor_elecS_EUR",
                      "L2234.StubTechCalInput_elecS_EUR",
                      "L2234.StubTechProd_elecS_EUR",
                      "L2234.StubTechFixOut_elecS_EUR",
                      "L2234.StubTechFixOut_hydro_elecS_EUR",
                      "L2234.StubTechCost_offshore_wind_elecS_EUR",
                      "L2234.StubTechElecMarket_backup_elecS_EUR",
                      "L2234.TechShrwt_elecS_grid_EUR",
                      "L2234.TechCoef_elecS_grid_EUR",
                      "L2234.TechProd_elecS_grid_EUR",
                      "L2234.TechFixSegOut_elecS_grid_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    L2234.load_segments <- unique(A23.elecS_naming$supplysector)
    L2234.R_elec_supply <- L1239.R_elec_supply %>% left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, supplysector = segment, subsector = fuel, year, fraction)

    # filter all stub tech inputs to only grid region countries
    for(input_nm in MODULE_INPUTS){
      if(grepl("StubTech", input_nm)){
        assign(input_nm, get(input_nm) %>% filter(region %in% grid_regions$region))
      }
    }

    # 0. functions -------------------
    # Want to expand electricity supplysector to all segments, except for any present for same subsector/tech
    expand_globaldb <- function(df){
      if("technology" %in% names(df)){
        expand_to_segments(df, sector = "sector.name", group_by_cols = c("subsector.name", "technology"), segments = L2234.load_segments) %>%
          tech_name_expansion(sector = "sector.name", mapping = A23.elecS_naming)
      } else if ("intermittent.technology" %in% names(df)){
        expand_to_segments(df, sector = "sector.name", group_by_cols = c("subsector.name", "intermittent.technology"), segments = L2234.load_segments) %>%
          tech_name_expansion(sector = "sector.name", tech = "intermittent.technology", mapping = A23.elecS_naming)
      } else {  warning("No technology column found") }
    }

    expand_stubtech <- function(df){
      expand_to_segments(df, group_by_cols = c("subsector", "stub.technology"), segments = L2234.load_segments) %>%
          tech_name_expansion(tech = "stub.technology", mapping = A23.elecS_naming)
    }

    # 1. Supplysector information    -----------------------------------------------------------------------------
    # Create horizontal generation supplysectors
    A23.horiz_sectors <- A23.sector %>%
      # repeat electricity row for all load segments
      filter(supplysector == "electricity") %>%
      select(-supplysector) %>%
      repeat_add_columns(tibble(supplysector = L2234.load_segments)) %>%
      bind_rows(filter(A23.sector, supplysector != "electricity"))

    L2234_sector_elecS <- A23.horiz_sectors %>%
      # then repeat for all regions
      repeat_add_columns(tibble(region = grid_regions$region)) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS))

    L2234.Supplysector_elecS_EUR <-L2234_sector_elecS %>% select(LEVEL2_DATA_NAMES[["Supplysector"]])
    L2234.ElecReserve_elecS_EUR <- L2234_sector_elecS %>% select(LEVEL2_DATA_NAMES[["ElecReserve"]])

    # 2. Subsector information  -----------------------------------------------------------------------------
    # Filter out hydro since it is dealt with separately in the fixed output tables
    L2234.SubsectorLogit_elecS_EUR <- expand_to_segments(A23.elecS_subsector_logit, group_by_cols = "subsector", segments = L2234.load_segments) %>%
      write_to_all_states(c("region", "supplysector", "subsector", "logit.year.fillout", "logit.exponent" , "logit.type"),
                          region_list = grid_regions$region)

    # Note that the subsector share-weights are updated after processing calibration year output values.
    L2234.SubsectorShrwt_elecS_EUR_preadj <- expand_to_segments(A23.elecS_subsector_shrwt, group_by_cols = "subsector", segments = L2234.load_segments) %>%
      write_to_all_states(c("region", "supplysector","subsector","year", "share.weight"),
                          region_list = grid_regions$region) %>%
      filter(subsector != "hydro")

    L2234.SubsectorShrwtInterp_elecS_EUR <- expand_to_segments(A23.elecS_subsector_shrwt_interp, group_by_cols = "subsector", segments = L2234.load_segments) %>%
      write_to_all_states( c("region", "supplysector","subsector","apply.to", "from.year","to.year", "interpolation.function"),
                           region_list = grid_regions$region) %>%
        filter(subsector != "hydro")

    L2234.SubsectorShrwtInterpTo_elecS_EUR_preadj  <- expand_to_segments(A23.elecS_subsector_shrwt_interpto, group_by_cols = "subsector", segments = L2234.load_segments) %>%
      write_to_all_states(c("region", "supplysector","subsector", "apply.to", "from.year","to.year","to.value", "interpolation.function"),
                          region_list = grid_regions$region) %>%
      filter(subsector != "hydro")

    # 3. Global Technology information -----------------------------------------------------------------------------
    # 3a. Shareweights ----------------
    # combine core assumptions with elecS adjustments
    L2234.GlobalTechShrwt_elecS_EUR <- A23.globaltech_shrwt %>%
      anti_join(A23.elecS_globaltech_shrwt, by = c("supplysector", "subsector", "technology")) %>%
      bind_rows(A23.elecS_globaltech_shrwt) %>%
      expand_to_segments(group_by_cols = c("subsector", "technology"), segments = L2234.load_segments) %>%
      gather_years("share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS,MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      tech_name_expansion(mapping = A23.elecS_naming) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year, share.weight)

    # 3b. Capacity factors ----------------------------
    # Read in lower capacity factors for non-baseload technologies. The fractions are based on the
    # elecS_time_fraction data on the fraction of demand supplied by vertical segment
    # assumes that columns in elecS_time_fraction are in order from base to peak
    # and that rows in A23.elecS_naming are in order from base to peak
    L2234.CF_adj <- elecS_time_fraction %>%
      tidyr::pivot_longer(-grid_region, names_to = "segment") %>%
      group_by(segment) %>%
      summarise(value = mean(value)) %>%
      ungroup %>%
      # set factors based on order of columns in csv
      mutate(segment = factor(segment, levels = names(elecS_time_fraction)[-1])) %>%
      # arrange from peak to base to take the cumulative sum from top to bottom
      arrange(desc(segment)) %>%
      mutate(adj_factor = cumsum(value)) %>%
      mutate(sector.name = rev(L2234.load_segments)) %>%
      select(-value, -segment)

    L2234.GlobalTechCapFac_elecS_EUR <- filter(L223.GlobalTechCapFac_elec, technology != "hydro") %>%
      expand_globaldb %>%
      left_join(L2234.CF_adj, by = "sector.name") %>%
      mutate(capacity.factor = adj_factor * capacity.factor) %>%
      select(-adj_factor)

    # 3c. Automatic assignment for global tech db ------------------
    # primary energy keywords, cap costs, O&M costs, efficiencies
    # tech lifetimes, shutdown deciders, CCS capture
    purrr::walk(AUTO_EXPAND_INPUTS, function(input_nm){
      assign(paste0(gsub("L223", "L2234", input_nm), "S_EUR"),
             value = expand_globaldb(get(input_nm)) %>%
               add_title(paste0(attributes(get(input_nm))$title, " - european grid regions"), overwrite = T) %>%
               add_units(attributes(get(input_nm))$unit) %>%
               add_precursors(c(input_nm, "gcam-europe/A23.elecS_naming")),
             envir = globalenv() )})

    # 4. Calibration Year Outputs ---------------------------------
    # 4a. L2234.StubTechProd_elecS_EUR ----------------------------
    # Need to explicitly add zero elect_td_bld production
    elect_td_bld_StubTech <- L223.PrimaryRenewKeywordInt_elec %>%
      filter(sector.name == "elect_td_bld",
             year %in% L223.StubTechProd_elec_EUR$year) %>%
      select(supplysector = sector.name, subsector = subsector.name, stub.technology = intermittent.technology, year) %>%
      repeat_add_columns(L223.StubTechProd_elec_EUR %>% distinct(region)) %>%
      anti_join(L223.StubTechProd_elec_EUR, by = c("supplysector", "subsector", "stub.technology", "year", "region")) %>%
      mutate(calOutputValue  = 0,
             share.weight.year = year,
             subs.share.weight = 0,
             share.weight = 0)

    L2234.StubTechProd_NA <- L223.StubTechProd_elec_EUR %>%
      bind_rows(elect_td_bld_StubTech) %>%
      expand_stubtech %>%
      # using left_join because there are some zeros in L223.StubTechProd_elec_EUR but not in L1239.R_elec_supply
      # below we confirm that all NAs are associated with a 0 calOutputValue
      left_join(L2234.R_elec_supply, by = c("region", "supplysector", "subsector", "year"))

    stopifnot(all(filter(L2234.StubTechProd_NA, is.na(fraction))$calOutputValue == 0))

    L2234.StubTechProd_elecS_EUR <- L2234.StubTechProd_NA %>%
      tidyr::replace_na(list(fraction = 0)) %>%
      mutate(calOutputValue = round(calOutputValue * fraction, energy.DIGITS_CALPRODUCTION)) %>%
      mutate(share.weight = if_else(calOutputValue > 0, 1, 0))

    # 4b. L2234.StubTechCalInput_elecS_EUR ----------------------------
    L2234.StubTechCalInput_NA <- L223.StubTechCalInput_elec_EUR %>%
      expand_stubtech %>%
      # using left_join because there are some zeros in L223.StubTechProd_elec_EUR but not in L1239.R_elec_supply
      # below we confirm that all NAs are associated with a 0 calOutputValue
      left_join(L2234.R_elec_supply, by = c("region", "supplysector", "subsector", "year"))

    stopifnot(all(filter(L2234.StubTechCalInput_NA, is.na(fraction))$calibrated.value == 0))

    L2234.StubTechCalInput_elecS_EUR <- L2234.StubTechCalInput_NA %>%
      tidyr::replace_na(list(fraction = 0)) %>%
      mutate(calibrated.value = round(calibrated.value * fraction, energy.DIGITS_CALPRODUCTION)) %>%
      mutate(tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      group_by(region, supplysector, subsector, year) %>%
      mutate(subs.share.weight = if_else(any(calibrated.value > 0), 1, 0)) %>%
      ungroup

    # 4c. Subsector shareweight update -----------------------------------
    # Update nuclear subsector share-weights - use zero  shareweights if there is no calibration year value.
    # Interpolate to a fixed value for states that have nuclear.
    L2234.StubTechProd_elecS_EUR_final_cal_year <- L2234.StubTechProd_elecS_EUR %>%
      filter(year == max(MODEL_BASE_YEARS))  %>%
      # summarize calibrated production by subsector
      group_by(region, subsector) %>%
      summarize(subsector.cal.value = sum(calOutputValue)) %>%
      ungroup()

    # Adjusting nuclear subsector shareweights - states with no historical nuclear power generation
    # receive zero  shareweights.
    L2234.SubsectorShrwt_elecS_EUR <- L2234.SubsectorShrwt_elecS_EUR_preadj %>%
      # left_join_error_no_match throws an error because the grid_storage and rooftop_pv subsector do not exist historically
      # and fossil fuels missing from L2234.StubTechProd_elecS_EUR_final_cal_year, but only adjust nuclear here
      left_join(L2234.StubTechProd_elecS_EUR_final_cal_year, by = c("region", "subsector")) %>%
      mutate(share.weight = as.double(share.weight),
             share.weight = if_else(subsector == "nuclear" & subsector.cal.value == 0, 0, share.weight)) %>%
      select(region, supplysector, subsector, year, share.weight)

    L2234.SubsectorShrwtInterpTo_elecS_EUR <- L2234.SubsectorShrwtInterpTo_elecS_EUR_preadj %>%
      # left_join_error_no_match throws an error because the grid_storage and rooftop_pv subsector do not exist historically
      # and fossil fuels missing from L2234.StubTechProd_elecS_EUR_final_cal_year, but only adjust nuclear here
      left_join(L2234.StubTechProd_elecS_EUR_final_cal_year, by = c("region", "subsector")) %>%
      mutate(to.value = as.double(to.value),
             to.value = if_else(subsector == "nuclear" & subsector.cal.value == 0, 0, to.value)) %>%
      select(region, supplysector, subsector, apply.to, from.year, to.year, to.value, interpolation.function)

    # 4d. Fixed Output calibration for hydro ----------------------------------------------------------------
    L2234.StubTechFixOut_elecS_EUR <- L223.StubTechFixOut_elec_EUR %>%
      expand_stubtech %>%
      left_join_error_no_match(L2234.R_elec_supply, by = c("region", "supplysector", "subsector", "year")) %>%
      mutate(fixedOutput = fixedOutput * fraction) %>%
      select(region, supplysector, subsector, stub.technology, year, fixedOutput,
             share.weight.year, subs.share.weight = subsector.share.weight, tech.share.weight = share.weight)

    # Fixed Output for hydro in future years.
    # We apply the same fule fractions in future years as in the final calibration year
    L2234.fuelfractions_segment_EUR_hydro_final_calibration_year <- L2234.R_elec_supply %>%
      filter(year == max(MODEL_BASE_YEARS), subsector == "hydro")

    L2234.StubTechFixOut_hydro_elecS_EUR <- L223.StubTechFixOut_hydro_EUR %>%
      expand_stubtech %>%
      left_join_error_no_match(L2234.fuelfractions_segment_EUR_hydro_final_calibration_year,
                               by = c("region", "supplysector", "subsector")) %>%
      mutate(fixedOutput = fixedOutput * fraction) %>%
      select(region, supplysector, subsector, stub.technology, year = year.x, fixedOutput,
             share.weight.year,  subs.share.weight, tech.share.weight)

    # 4e. Copy efficiency and cap factor-----------------
    L2234.StubTechEff_elecS_EUR <- expand_stubtech(L223.StubTechEff_elec_EUR)
    L2234.StubTechCapFactor_elecS_EUR <- expand_stubtech(L223.StubTechCapFactor_elec_EUR)

    # Create State-specific non-energy cost adder for offshore wind grid connection cost
    L2234.StubTechCost_offshore_wind_elecS_EUR <- L2234.StubTechCapFactor_elecS_EUR %>%
      filter(grepl("_offshore", stub.technology)) %>%
      select(-capacity.factor) %>%
      left_join_error_no_match(L223.StubTechCost_offshore_wind_EUR %>% select(-supplysector, -stub.technology),
                               by = c("region", "subsector", "year"))

    # 4f. Backup markets --------------
    L2234.StubTechElecMarket_backup_elecS_EUR <- L2234.StubTechCapFactor_elecS_EUR %>%
      select(-capacity.factor) %>%
      left_join_error_no_match(grid_regions %>% rename(electric.sector.market = grid_region), by = "region")

    # 5. Inputs for any new technologies such as battery -----------------------
    # and append them with corresponding tables
    grid_storage_elecS <- A23.elecS_globaltech_shrwt %>%
      filter(subsector == "grid_storage") %>%
      left_join_error_no_match(A23.elecS_naming, by = "supplysector") %>%
      mutate(technology = paste(technology, name_adder, sep = "_")) %>%
      distinct(supplysector,subsector, elecS_tech = technology)


    L2234.elecS_globaltech_capital_battery_ATB <- L113.elecS_globaltech_capital_battery_ATB %>%
      left_join_error_no_match(grid_storage_elecS, by = c("supplysector", "subsector")) %>%
      select(-technology) %>%
      rename(technology = elecS_tech)

    L2234.GlobalTechCapFac_elecS_EUR <- L2234.elecS_globaltech_capital_battery_ATB %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year = period, capacity.factor) %>%
      bind_rows(L2234.GlobalTechCapFac_elecS_EUR)

    L2234.GlobalTechCapital_elecS_EUR <- L2234.elecS_globaltech_capital_battery_ATB %>%
      mutate(input.capital = "capital") %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year = period, input.capital,
             capital.overnight = capital.cost, fixed.charge.rate = fcr)  %>%
      bind_rows(L2234.GlobalTechCapital_elecS_EUR)

    L2234.GlobalTechOMfixed_elecS_EUR <-  L2234.elecS_globaltech_capital_battery_ATB %>%
      mutate(input.OM.fixed = "OM-fixed") %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year = period,
             input.OM.fixed, OM.fixed = fixed.om) %>%
      bind_rows(L2234.GlobalTechOMfixed_elecS_EUR)

    L2234.GlobalTechOMvar_elecS_EUR <- L2234.elecS_globaltech_capital_battery_ATB %>%
      mutate(input.OM.var = "OM-var") %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year = period,
             input.OM.var, OM.var = variable.om)  %>%
      bind_rows(L2234.GlobalTechOMvar_elecS_EUR)

    L2234.GlobalTechLifetime_elecS_EUR <- L2234.elecS_globaltech_capital_battery_ATB %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year = period, lifetime) %>%
      bind_rows(L2234.GlobalTechLifetime_elecS_EUR)

    L2234.GlobalTechSCurve_elecS_EUR <-  L2234.elecS_globaltech_capital_battery_ATB %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year = period, lifetime, steepness, half.life) %>%
      bind_rows(L2234.GlobalTechSCurve_elecS_EUR)

    # Adding an scurve behavior for renewables in 2015 so they don't all retire at once
    L2234.GlobalIntTechSCurve_elecS_EUR <- L2234.GlobalIntTechLifetime_elecS_EUR %>%
      filter(subsector.name %in% c("solar", "wind"),
             year == MODEL_FINAL_BASE_YEAR) %>%
      # extend lifetime since we are adding s-curve
      mutate(lifetime = lifetime + 10,
             steepness = 0.1,
             half.life = (lifetime / 2) + 5)

    # Energy Inputs for additional technologies such as battery
    L2234.StubTechEff_battery_elecS_EUR <- write_to_all_states(A23.elecS_stubtech_energy_inputs,
                                                           c("region", "supplysector","subsector","stub.technology",
                                                             "period", "minicam.energy.input", "market.name", "efficiency"),
                                                           region_list = grid_regions$region) %>%
      left_join_error_no_match(grid_regions, by = "region") %>%
      mutate(market.name = if_else(market.name == "grid_region", grid_region, region)) %>%
      select(region, supplysector, subsector, stub.technology, year = period,
             minicam.energy.input, efficiency, market.name)


    # 6a. Create horizontal and vertical supplysectors in grid regions. -------------------------------
    L2234.Supplysector_elecS_grid <- write_to_all_states(A23.horiz_sectors,
                                                         c("region", "supplysector", "output.unit", "input.unit", "price.unit",
                                                            "logit.year.fillout", "logit.exponent", "logit.type" ),
                                                         # NOTE: writing to all grid regions, rather than states
                                                         region_list = unique(grid_regions$grid_region))

    L2234.Supplysector_elecS_EUR <-  bind_rows(L2234.Supplysector_elecS_EUR, L2234.Supplysector_elecS_grid)

    L2234.ElecReserve_elecS_grid <-  write_to_all_states(A23.horiz_sectors,
                          c("region", "supplysector","electricity.reserve.margin", "average.grid.capacity.factor"),
                          region_list = unique(grid_regions$grid_region))


    L2234.ElecReserve_elecS_EUR <- bind_rows(L2234.ElecReserve_elecS_EUR, L2234.ElecReserve_elecS_grid)

    # Logits for subsectors in grid regions
    L2234.SubsectorLogit_elecS_grid <- L2234.Supplysector_elecS_EUR %>%
      filter(region %in% grid_regions$region) %>%
      left_join_error_no_match(grid_regions, by = c("region")) %>%
      mutate(subsector = paste(region,supplysector, sep = " ")) %>%
      select(grid_region, supplysector, subsector) %>%
      rename(region = grid_region) %>%
      mutate(logit.year.fillout = as.integer(min(MODEL_BASE_YEARS)),
             logit.exponent = gcamusa.GRID_REGION_LOGIT,
             logit.type = gcamusa.GRID_REGION_LOGIT_TYPE)

    L2234.SubsectorLogit_elecS_EUR <- bind_rows(L2234.SubsectorLogit_elecS_EUR, L2234.SubsectorLogit_elecS_grid)

    # Shareweights for subsectors in grid regions
    L2234.SubsectorShrwtFllt_elecS_grid_EUR <- L2234.SubsectorLogit_elecS_grid %>%
      select(region, supplysector, subsector) %>%
      mutate(year.fillout = min(MODEL_FUTURE_YEARS),
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT)

    L2234.SubsectorShrwtInterp_elecS_grid_EUR <-  L2234.SubsectorLogit_elecS_grid %>%
      select(region, supplysector, subsector) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_FUTURE_YEARS),
             interpolation.function = "fixed" )

    # 6b. Technology shareweights, coefficients --------------------------------
    # Shareweights for technologies in grid region sectors. This is a new table that needs to created.
    # Shareweights for state-level technologies are read in the global-technology-database.
    L2234.TechShrwt_elecS_grid_EUR <- L2234.SubsectorLogit_elecS_grid %>%
      select(region, supplysector, subsector) %>%
      mutate(technology = subsector,
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      repeat_add_columns((tibble(year = MODEL_YEARS)))

    # Specify inputs for technologies in grid regions
    L2234.TechCoef_elecS_grid_EUR <- L2234.TechShrwt_elecS_grid_EUR %>%
      distinct(region, supplysector, year) %>%
      mutate(minicam.energy.input = supplysector) %>%
      left_join(grid_regions %>%  rename(market.name = region), by = c("region" = "grid_region"))  %>%
      # Coefficients for technologies in grid region sectors.
      # Coefficients for generation sectors are 1.
      mutate(subsector = paste(market.name, supplysector, sep = " "),
             technology = subsector,
             coefficient = gcamusa.DEFAULT_COEFFICIENT) %>%
      select(region, supplysector, subsector, technology, year,
             minicam.energy.input, coefficient, market.name)

    # 6c. Calibration years outputs for technologies in grid regions -------------------------------
    # Combine hydro, renewable production and calculated fossil output
    L2234.TechFixOut_elecS_grid_EUR <- L2234.StubTechFixOut_elecS_EUR %>%
      left_join_error_no_match(grid_regions, by = c("region")) %>%
      group_by (region, grid_region, supplysector, year) %>%
      summarise(calOutputValue = sum(fixedOutput)) %>%
      ungroup()

    L2234.TechProd_elecS_renew_grid_EUR <- L2234.StubTechProd_elecS_EUR %>%
      left_join_error_no_match(grid_regions, by = c("region")) %>%
      group_by(region, grid_region, supplysector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup()

    L2234.TechProd_elecS_fossil_grid_EUR <- L2234.StubTechCalInput_elecS_EUR %>%
      left_join_error_no_match(grid_regions, by = c("region")) %>%
      left_join_error_no_match(L2234.StubTechEff_elecS_EUR,
                               by = c("region", "supplysector", "subsector", "stub.technology",
                                      "year", "minicam.energy.input")) %>%
      group_by(region, grid_region, supplysector, year) %>%
      summarise(calOutputValue = sum(calibrated.value * efficiency)) %>%
      ungroup()

    L2234.TechProd_elecS_grid_EUR <- bind_rows(L2234.TechFixOut_elecS_grid_EUR,
                                               L2234.TechProd_elecS_renew_grid_EUR,
                                               L2234.TechProd_elecS_fossil_grid_EUR) %>%
      group_by(region, grid_region, supplysector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup %>%
      mutate(subsector = paste(region, supplysector, sep = " "),
             technology = subsector,
             share.weight.year = as.numeric(year),
             # for now, since subsectors and technologies are the same, can keep shareweights the same as well
             subs.share.weight = if_else(calOutputValue > 0, gcamusa.DEFAULT_SHAREWEIGHT, 0),
             tech.share.weight = subs.share.weight) %>%
      select(-region) %>%
      rename(region = grid_region)

    # 6d. Pass through sectors
    # 6d. Fixed Segment Output ---------------------------

    # if there is a year/segment that only has output of hydro, need to add a fixed output to the grid region
    # For example, in Albania, all base load generation comes from hydro. Since it is a fixed output, this means
    # that all the subsector shareweights for baseload gen are zero. This creates problems when the grid then tries
    # to use Albania baseload generation. So the fix here is to have a fixed output in the grid with a zero subsector shareweight
    L2234.TechFixSegOut_elecS_grid_EUR <- bind_rows(L2234.StubTechProd_elecS_EUR %>% rename(prod = calOutputValue),
                                                        L2234.StubTechCalInput_elecS_EUR %>% rename(prod = calibrated.value),
                                                        L2234.StubTechFixOut_elecS_EUR %>% rename(prod = fixedOutput)) %>%
      filter(prod > 0) %>%
      group_by(region, supplysector, year) %>%
      filter(dplyr::n() == 1 & subsector == "hydro") %>%
      ungroup %>%
      mutate(subsector = paste(region, supplysector, sep = " "),
             technology = subsector) %>%
      select(supplysector, subsector, technology, year, fixedOutput = prod) %>%
      left_join_error_no_match(L2234.TechProd_elecS_grid_EUR, by = c("supplysector", "subsector", "technology", "year")) %>%
      mutate(subs.share.weight = 0,
             tech.share.weight = 0) %>%
      select(LEVEL2_DATA_NAMES[["TechFixOutSW"]])

    # need to remove from L2234.TechProd_elecS_grid_EUR
    L2234.TechProd_elecS_grid_EUR <- L2234.TechProd_elecS_grid_EUR %>%
      anti_join(L2234.TechFixSegOut_elecS_grid_EUR, by = c("region", "supplysector", "subsector", "technology", "year"))

    # 6e. PassThrough tables ------------------
    # The marginal revenue sector is the region's electricity sector whereas the marginal revenue market is the grid region.
    L2234.PassThroughSector_elecS_EUR <- L2234.Supplysector_elecS_EUR %>%
      inner_join(grid_regions, by = c("region")) %>%
      mutate(marginal.revenue.sector = supplysector) %>%
      select(region, pass.through.sector = supplysector, marginal.revenue.sector, marginal.revenue.market = grid_region)

    # Create a L2234.PassThroughTech_elecS_grid_EUR dataframe (to be converted into a csv table later).
    # This one should contain region, supplysector, subsector, technology for the grid regions
    # to which electricity produced in countries is passed through.
    # Note that the "technology" in this data-frame will be called "passthrough technology"
    L2234.TechShrwt_elecS_grid_EUR %>%
      select(-year, -share.weight, pass.through.technology = technology) %>%
      distinct() -> L2234.PassThroughTech_elecS_grid_EUR

    # Produce outputs ===================================================
    L2234.Supplysector_elecS_EUR %>%
      add_title("Supply Sector Information for Electricity Load Segments") %>%
      add_units("unitless") %>%
      add_comments("Supplysector information for horizontal generation supplysectors at state and grid region level") %>%
      add_precursors("energy/A23.sector", "gcam-europe/mappings/grid_regions") ->
      L2234.Supplysector_elecS_EUR

    L2234.ElecReserve_elecS_EUR %>%
      add_title("Supply Sector Information for Horizontal Electricity Load Segments") %>%
      add_units("unitless") %>%
      add_comments("Supplysector information for horizontal generation supplysectors at state and grid region level") %>%
      same_precursors_as(L2234.Supplysector_elecS_EUR) ->
      L2234.ElecReserve_elecS_EUR

    L2234.SubsectorLogit_elecS_EUR %>%
      add_title("Subsector Information for Horizontal Electricity Load Segments") %>%
      add_units("unitless") %>%
      add_comments("Subsector information for horizontal generation supplysectors at state and grid region level") %>%
      add_precursors("gcam-europe/mappings/grid_regions",
                     "gcam-europe/A23.elecS_subsector_logit",
                     "energy/A23.sector") ->
      L2234.SubsectorLogit_elecS_EUR

    L2234.SubsectorShrwtInterp_elecS_EUR %>%
      add_title("Electricity Load Segments Subsector Shareweights") %>%
      add_units("NA") %>%
      add_comments("Subsector shareweights for electricity load segment subsectors that are fixed at calibration values") %>%
      add_precursors("gcam-europe/mappings/grid_regions",
                     "gcam-europe/A23.elecS_tech_availability",
                     "gcam-europe/A23.elecS_subsector_shrwt_interp") ->
      L2234.SubsectorShrwtInterp_elecS_EUR

    L2234.SubsectorShrwtInterpTo_elecS_EUR %>%
      add_title("Electricity Load Segments Subsector Shareweights") %>%
      add_units("NA") %>%
      add_comments("Subsector shareweights for electricity load segment subsectors which need a to-value") %>%
      add_precursors("gcam-europe/mappings/grid_regions",
                     "gcam-europe/A23.elecS_tech_availability",
                     "gcam-europe/A23.elecS_subsector_shrwt_interpto",
                     "L223.StubTechProd_elec_EUR",
                     "L1239.R_elec_supply") ->
      L2234.SubsectorShrwtInterpTo_elecS_EUR

    L2234.SubsectorShrwt_elecS_EUR %>%
      add_title("Electricity Load Segments Subsector Shareweights") %>%
      add_units("NA") %>%
      add_comments("Subsector shareweights for electricity load segment subsectors at points of inflexion") %>%
      add_precursors("gcam-europe/mappings/grid_regions",
                     "gcam-europe/A23.elecS_tech_availability",
                     "gcam-europe/A23.elecS_subsector_shrwt",
                     "gcam-europe/A23.elecS_subsector_shrwt_state_adj",
                     "gcam-europe/NREL_us_re_technical_potential",
                     "gcam-europe/A10.renewable_resource_delete",
                     "L223.StubTechProd_elec_EUR",
                     "L1239.R_elec_supply") ->
      L2234.SubsectorShrwt_elecS_EUR

    L2234.StubTechEff_elecS_EUR %>%
      add_title("Electricity Load Segments Base Year Efficiencies") %>%
      add_units("unitless") %>%
      add_comments("Efficiencies of multiple load segments coal, oil and gas technologies in base years") %>%
      add_precursors("gcam-europe/mappings/grid_regions",
                     "gcam-europe/A23.elecS_tech_availability",
                     "gcam-europe/A23.elecS_stubtech_energy_inputs",
                     "gcam-europe/NREL_us_re_technical_potential",
                     "L123.eff_R_elec_F_Yh",
                     "L223.StubTechEff_elec_EUR") ->
      L2234.StubTechEff_elecS_EUR

    L2234.SubsectorShrwtFllt_elecS_grid_EUR %>%
      add_title("Electricity Load Segments Grid Region Subsector Shareweights") %>%
      add_units("unitless") %>%
      add_comments("Grid region electricity load segment subsector shareweights") %>%
      add_precursors("gcam-europe/mappings/grid_regions",
                     "energy/A23.sector") ->
      L2234.SubsectorShrwtFllt_elecS_grid_EUR

    L2234.SubsectorShrwtInterp_elecS_grid_EUR %>%
      add_title("Electricity Load Segments Grid Region Subsector Shareweights") %>%
      add_units("unitless") %>%
      add_comments("Grid region electricity load segment subsector shareweights that are fixed at calibration values") %>%
      same_precursors_as("L2234.SubsectorShrwtFllt_elecS_grid_EUR") ->
      L2234.SubsectorShrwtInterp_elecS_grid_EUR

    L2234.PassThroughSector_elecS_EUR %>%
      add_title("Electricity Load Segments Passthrough Sectors") %>%
      add_units("NA") %>%
      add_comments("The marginal revenue sector is the region's electricity sector; the marginal revenue market is the grid region") %>%
      same_precursors_as("L2234.SubsectorShrwtFllt_elecS_grid_EUR") ->
      L2234.PassThroughSector_elecS_EUR

    L2234.PassThroughTech_elecS_grid_EUR %>%
      add_title("Electricity Load Segments Passthrough Technologies - Grid Region") %>%
      add_units("NA") %>%
      add_comments("Grid region electricity load segment passthrough technologies") %>%
      same_precursors_as("L2234.SubsectorShrwtFllt_elecS_grid_EUR") ->
      L2234.PassThroughTech_elecS_grid_EUR

    L2234.GlobalTechShrwt_elecS_EUR %>%
      add_title("Electricity Load Segments Technology Shareweights") %>%
      add_units("NA") %>%
      add_comments("Electricity load segment technology shareweight assumptions") %>%
      add_precursors("gcam-europe/A23.elecS_globaltech_shrwt") ->
      L2234.GlobalTechShrwt_elecS_EUR

    L2234.GlobalTechCapFac_elecS_EUR %>%
      add_title("Global Capacity Factors for Electricity Load Segments Technologies") %>%
      add_units("unitless") %>%
      add_comments("Global capacity factors for electricity load segments generation technologies") %>%
      add_precursors("gcam-europe/elecS_time_fraction",
                     "L113.elecS_globaltech_capital_battery_ATB",
                     "L223.GlobalTechCapFac_elec") ->
      L2234.GlobalTechCapFac_elecS_EUR

    L2234.StubTechCalInput_elecS_EUR %>%
      add_title("Electricity Load Segments Technology Calibration Inputs") %>%
      add_units("EJ (calibrated.value); unitless") %>%
      add_precursors("gcam-europe/mappings/grid_regions",
                     "L223.StubTechCalInput_elec_EUR") ->
      L2234.StubTechCalInput_elecS_EUR

    L2234.StubTechProd_elecS_EUR %>%
      add_title("Electricity Load Segments Technology Calibration Outputs") %>%
      add_units("EJ (cal.Output.value); unitless") %>%
      add_comments("Calibration outputs for electricity load segments technologies") %>%
      add_precursors("gcam-europe/mappings/grid_regions",
                     "L223.StubTechProd_elec_EUR",
                     "L1239.R_elec_supply") ->
      L2234.StubTechProd_elecS_EUR

    L2234.StubTechFixOut_elecS_EUR %>%
      add_title("Electricity Load Segments Hydro Fixed Outputs") %>%
      add_units("EJ (fixedOutput); unitless") %>%
      add_comments("Calibration year fixed outputs for hydro electricity load segments technologies") %>%
      add_precursors("L223.StubTechFixOut_elec_EUR",
                     "L1239.R_elec_supply") ->
      L2234.StubTechFixOut_elecS_EUR

    L2234.StubTechFixOut_hydro_elecS_EUR %>%
      add_title("Electricity Load Segments Hydro Fixed Outputs") %>%
      add_units("EJ (fixedOutput); unitless") %>%
      add_comments("Future year fixed outputs for hydro electricity load segments technologies") %>%
      add_precursors("L223.StubTechFixOut_hydro_EUR",
                     "L1239.R_elec_supply") ->
      L2234.StubTechFixOut_hydro_elecS_EUR

    L2234.StubTechCost_offshore_wind_elecS_EUR %>%
      add_title("Cost adders for offshore wind grid connection in USA") %>%
      add_units("Unitless") %>%
      add_comments("State-specific non-energy cost adder for offshore wind grid connection cost") %>%
      add_precursors("L223.StubTechCost_offshore_wind_EUR") ->
      L2234.StubTechCost_offshore_wind_elecS_EUR

    L2234.StubTechElecMarket_backup_elecS_EUR %>%
      add_title("Electricity Load Segments Sector Name for Backup Markets") %>%
      add_units("NA") %>%
      add_precursors("L223.GlobalIntTechBackup_elec") ->
      L2234.StubTechElecMarket_backup_elecS_EUR

    L2234.TechShrwt_elecS_grid_EUR %>%
      add_title("Electricity Load Segments Grid Technology Shareweights") %>%
      add_units("unitless") %>%
      add_comments("Electricity load segment grid technology shareweight assumptions") %>%
      add_precursors("gcam-europe/mappings/grid_regions",
                     "energy/A23.sector") ->
      L2234.TechShrwt_elecS_grid_EUR

    L2234.TechCoef_elecS_grid_EUR %>%
      add_title("Electricity Load Segments Grid Technology Coefficients") %>%
      add_units("unitless") %>%
      add_comments("Electricity load segment grid technology coefficients") %>%
      same_precursors_as("L2234.TechShrwt_elecS_grid_EUR") ->
      L2234.TechCoef_elecS_grid_EUR

    L2234.TechProd_elecS_grid_EUR %>%
      add_title("Electricity Load Segments Grid Technology Calibration Outputs") %>%
      add_units("EJ (cal.Output.value); unitless") %>%
      add_comments("Calibration outputs for electricity load segments grid technologies") %>%
      add_precursors("gcam-europe/mappings/grid_regions",
                     "L223.StubTechProd_elec_EUR",
                     "L223.StubTechFixOut_elec_EUR",
                     "L1239.R_elec_supply") ->
      L2234.TechProd_elecS_grid_EUR

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
