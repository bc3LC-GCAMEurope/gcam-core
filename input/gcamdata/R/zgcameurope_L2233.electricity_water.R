# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcameurope_L2233.electricity_water
#'
#' Generates GCAM model inputs for electricity sector with cooling system types disaggregated.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2233.StubTech_elecPassthru_EUR}, \code{L2233.StubTechProd_elecPassthru_EUR},
#'  \code{L2233.PassThroughSector_elec_cool_EUR},
#' \code{L2233.Supplysector_elec_cool_EUR}, \code{L2233.ElecReserve_elec_cool_EUR},
#' \code{L2233.SubsectorShrwtFllt_elec_cool_EUR}, \code{L2233.SubsectorLogit_elec_cool_EUR},
#'  \code{L2233.StubTech_elec_cool_EUR}, \code{L2233.StubTechEff_elec_cool_EUR},
#' \code{L2233.StubTechProd_elec_cool_EUR}, \code{L2233.StubTechCapFactor_elec_cool_EUR}, \code{L2233.StubTechFixOut_hydro_EUR},
#'   \code{L2233.StubTechShrwt_elec_cool_EUR}
#' @details Disaggregates electricity sector for all cooling system types.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter first if_else group_by left_join mutate right_join select summarise first
#' @importFrom tidyr complete gather nesting
#' @author RH Feb 2024
module_gcameurope_L2233.electricity_water <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "common/GCAM_region_names",
                     FILE = "energy/calibrated_techs",
                     FILE = "energy/A23.globalinttech",
                     FILE = "energy/A23.globaltech_shrwt",
                     FILE = "energy/A23.sector",
                     FILE = "water/elec_tech_water_map",
                     FILE = "water/water_td_sectors",
                     FILE = "water/A23.CoolingSystemCosts",
                     FILE = "water/Macknick_elec_water_m3MWh",
                     "L1231.out_EJ_R_elec_F_tech_Yh_EUR",
                     "L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR",
                     "L1233.shrwt_R_elec_cool_Yf_EUR",
                     "L223.StubTechEff_elec_EUR",
                     "L223.StubTech_elec_EUR",
                     "L223.StubTechCapFactor_elec_EUR",
                     "L223.StubTechFixOut_hydro_EUR",
                     "L223.Supplysector_elec_EUR",
                     "L223.GlobalIntTechCapital_elec")

  MODULE_OUTPUTS <- c("L2233.StubTech_elecPassthru_EUR",
                      "L2233.StubTechProd_elecPassthru_EUR",
                      "L2233.PassThroughSector_elec_cool_EUR",
                      "L2233.Supplysector_elec_cool_EUR",
                      "L2233.ElecReserve_elec_cool_EUR",
                      "L2233.SubsectorShrwtFllt_elec_cool_EUR",
                      "L2233.SubsectorLogit_elec_cool_EUR",
                      "L2233.StubTechTrackCapital_elec_EUR",
                      "L2233.StubTech_elec_cool_EUR",
                      "L2233.StubTechEff_elec_cool_EUR",
                      "L2233.StubTechProd_elec_cool_EUR",
                      "L2233.StubTechCapFactor_elec_cool_EUR",
                      "L2233.StubTechFixOut_hydro_EUR",
                      "L2233.StubTechShrwt_elec_cool_EUR")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    from.supplysector <- from.subsector <- from.technology <- to.supplysector <-
      to.subsector <- to.technology <- year <- share.weight <- supplysector <-
      subsector <- technology <- minicam.energy.input <- calibration <-
      secondary.output <- value <- region <- sector <- calOutputValue <-
      subs.share.weight <- average.grid.capacity.factor <- plant_type <-
      cooling_system <- water_type <- fuel <- sector.name <- subsector.name <-
      input.capital <- capital.overnight_USD2005 <- capital.overnight <-
      water_withdrawals <- water_consumption <- coefficient <- water_sector <-
      share.weight.year <- emiss.coef <- efficiency <- emiss.coeff <- NULL  # silence package check notes

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    GCAM_region_names <- GCAM_region_names %>%  filter(region %in% gcameurope.EUROSTAT_COUNTRIES)

    ## BUILD TWO TABLES WITH ALL POSSIBLE TECHNOLOGIES FROM THE OLD AND NEW STRUCTURES -----------------
    # First table (L2233.TechMap) includes all technologies
    elec_tech_water_map %>%
      select(from.supplysector, from.subsector, from.technology,
             to.supplysector, to.subsector, to.technology) -> L2233.TechMap

    # Second table (L2233.TechMapYr) repeats for all model years
    L2233.TechMap %>%
      repeat_add_columns(tibble(year = as.integer(MODEL_YEARS))) -> L2233.TechMapYr

    ## PART 1: PASS-THOUGH TECHNOLOGIES IN THE EXISTING ELECTRICITY SECTOR ---------------
    L1231.out_EJ_R_elec_F_tech_Yh_EUR %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(calibrated_techs,
                                      - minicam.energy.input,
                                      -calibration, -secondary.output),
                               by = c("sector", "fuel", "technology")) %>%
      rename(stub.technology = technology, calOutputValue = value) %>%
      mutate(share.weight.year = year) -> StubTechProd_elecPassthru

    # The following replaces the "set_subsector_shrwt" function from the legacy code.
    # We essentially aggregate output for subsectors with multiple technologies and then assign a
    # shareweight of 1 for any technology residing in a subsector that has aggregated output > 0.
    StubTechProd_elecPassthru %>%
      group_by(region, sector, subsector, year) %>%
      summarise(subs.share.weight = sum(calOutputValue)) %>% ungroup %>%
      left_join(StubTechProd_elecPassthru, by = c("region", "sector", "subsector", "year")) %>%
      mutate(subs.share.weight = if_else(subs.share.weight > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      filter(subsector != "hydro") %>%  # << hydro is fixed output (doesn't need calibrating here)
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2233.StubTechProd_elecPassthru_EUR # --OUTPUT--

    # Upstream electricity sector that includes pass-thru technologies for calling pass-thru sectors
    L223.StubTech_elec_EUR %>% mutate(region = region) -> L2233.StubTech_elecPassthru_EUR

    ## PART 2: SUPPLYSECTOR AND SUBSECTOR INFORMATION IN THE NEW ELEC SECTORS ------------------
    L2233.TechMap %>%
      select(to.supplysector) %>%
      anti_join(L223.StubTech_elec_EUR, by = c("to.supplysector" = "supplysector")) %>%
      distinct() %>%
      rename(supplysector = to.supplysector) -> L2233.elec_cool_supplysectors
    # ^^ generates single column of elec technologies for expansion of Supplysector_elec...

    L223.Supplysector_elec_EUR %>%
      filter(supplysector == "electricity") %>%
      select(-supplysector) %>%
      repeat_add_columns(L2233.elec_cool_supplysectors) %>%
      mutate(logit.type = NA) -> L2233.supplysector_info
    # ^^ note: logit exponent doesn't matter here because there's no subsector competition

    L2233.supplysector_info %>%
      rename(pass.through.sector = supplysector) %>%
      mutate(marginal.revenue.sector = "electricity",
             marginal.revenue.market = region) %>%
      select(LEVEL2_DATA_NAMES[["PassThroughSector"]]) ->
      L2233.PassThroughSector_elec_cool_EUR # --OUTPUT--

    L2233.supplysector_info -> L2233.Supplysector_elec_cool_EUR # --OUTPUT--

    L2233.TechMap %>%
      select(from.supplysector, from.subsector, from.technology, to.supplysector) %>%
      filter(from.supplysector %in% A23.globalinttech$supplysector,
             from.subsector %in% A23.globalinttech$subsector,
             from.technology %in% A23.globalinttech$technology) %>%
      select(to.supplysector) %>% unique -> L2233.elec_cool_Int_supplysectors

    L2233.supplysector_info %>%
      filter(supplysector %in% L2233.elec_cool_Int_supplysectors$to.supplysector) %>%
      mutate(electricity.reserve.margin = unique(A23.sector$electricity.reserve.margin),
             average.grid.capacity.factor = unique(A23.sector$average.grid.capacity.factor)) %>%
      # ^^ Margin and capacity factor assumed to be same as for electricity and elect_td_bld
      select(LEVEL2_DATA_NAMES[["ElecReserve"]]) ->
      L2233.ElecReserve_elec_cool_EUR # --OUTPUT--

    elec_tech_water_map %>%
      select(to.supplysector, to.subsector) %>%
      unique %>%
      rename(supplysector = to.supplysector, subsector = to.subsector) ->
      supply_sub_elec_mapping
    # ^^ sets up simple mapping for supply to subsector for following section

    L2233.supplysector_info %>%
      left_join_error_no_match(supply_sub_elec_mapping, by = "supplysector") %>%
      mutate(year.fillout = first(MODEL_BASE_YEARS),
             share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) ->
      L2233.SubsectorShrwtFllt_elec_cool_EUR # --OUTPUT--

    L2233.SubsectorShrwtFllt_elec_cool_EUR %>%
      mutate(logit.year.fillout = first(MODEL_BASE_YEARS),
             logit.exponent = water.COOLING_SYSTEM_LOGIT) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]]) %>%
      mutate(logit.type = NA) -> L2233.SubsectorLogit_elec_cool_EUR # --OUTPUT--


    # some adjustments for rooftop_pv because it does not have vintaging we need to switch
    # from a input.capital to a standard non-energy input and add assumptions about
    # depreciation to be able to properly calculate capital demands
    L223.StubTechCapFactor_elec_EUR %>%
      filter(stub.technology == "rooftop_pv") %>%
      left_join_error_no_match(L223.GlobalIntTechCapital_elec, by=c("supplysector" = "sector.name",
                                                                     "subsector" = "subsector.name",
                                                                     "stub.technology" = "intermittent.technology",
                                                                     "year")) %>%
      mutate(input.cost = capital.overnight * fixed.charge.rate / (capacity.factor * CONV_YEAR_HOURS * CONV_KWH_GJ),
             capital.coef = 1 / fixed.charge.rate,
             tracking.market = "capital",
             depreciation.rate = 1 / 15) %>%
      select(-capacity.factor, -capital.overnight, -fixed.charge.rate) %>%
      rename(minicam.non.energy.input = input.capital) ->
      L2233.StubTechTrackCapital_elec_EUR

    ## PART 3: STUB TECHNOLOGY INFORMATION IN THE NEW SECTOR STRUCTURE -------------

    # Stub technologies of cooling system options
    L2233.TechMap %>%
      repeat_add_columns(GCAM_region_names) %>%
      rename(supplysector = to.supplysector,
             subsector = to.subsector,
             stub.technology = to.technology) %>%
      # no more offshore wind for land-locked countries
      filter(!(stub.technology == "wind_offshore" &
                 !region %in% filter(L223.StubTech_elec_EUR, stub.technology == "wind_offshore")$region)) %>%
      select(LEVEL2_DATA_NAMES[["StubTech"]]) ->
      L2233.StubTech_elec_cool_EUR  # --OUTPUT--

    # Stub technololgy shareweights for cooling system options
    L1233.shrwt_R_elec_cool_Yf_EUR %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(elec_tech_water_map,
                               by = c("sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      select(-from.supplysector, -from.subsector, -from.technology, -plant_type, -minicam.energy.input) %>%
      rename(share.weight = value,supplysector = to.supplysector,
             subsector = to.subsector, stub.technology = to.technology) %>%
      mutate(year = as.integer(year)) ->
      L2233.shrwt_R_elec_cool_Yf
    L2233.StubTech_elec_cool_EUR %>% repeat_add_columns(tibble(year = as.integer(MODEL_FUTURE_YEARS))) %>%
      left_join(L2233.shrwt_R_elec_cool_Yf,
                by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      # ^^ non-restrictive join required for NA values associated with technologies without cooling systems (e.g., wind)
      select(LEVEL2_DATA_NAMES[["StubTechShrwt"]]) %>%
      na.omit -> L2233.StubTechShrwt_elec_cool_EUR  # --OUTPUT--

    # Calibrated efficiencies of the cooling system options
    L2233.TechMapYr %>%
      repeat_add_columns(GCAM_region_names) %>%
      filter(from.supplysector %in% L223.StubTechEff_elec_EUR$supplysector,
             from.subsector %in% L223.StubTechEff_elec_EUR$subsector,
             from.technology %in% L223.StubTechEff_elec_EUR$stub.technology,
             year %in% L223.StubTechEff_elec_EUR$year) %>%
      left_join_error_no_match(L223.StubTechEff_elec_EUR,
                               by = c("region", "from.supplysector" = "supplysector",
                                      "from.subsector" = "subsector", "from.technology" = "stub.technology", "year")) %>%
      rename(supplysector = to.supplysector, subsector = to.subsector, stub.technology = to.technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechEff"]]) ->
      L2233.StubTechEff_elec_cool_EUR  # --OUTPUT--

    # Electricity technology calibration
    L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR %>%
      filter(year %in% MODEL_BASE_YEARS) %>% rename(calOutputValue = value) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(elec_tech_water_map,
                                      -from.supplysector, -from.subsector, -from.technology, -minicam.energy.input),
                               by = c("sector", "fuel", "technology", "cooling_system", "water_type", "plant_type")) %>%
      rename(supplysector = to.supplysector, subsector = to.subsector, stub.technology = to.technology) %>%
      mutate(share.weight.year = year,
             calOutputValue = round(calOutputValue, 7)) ->
      L2233.out_EJ_R_elec_F_tech_Yh_cool

    L2233.out_EJ_R_elec_F_tech_Yh_cool %>%
      group_by(region, supplysector, subsector, share.weight.year) %>%
      summarise(value = sum(calOutputValue)) %>% ungroup %>%
      mutate(subs.share.weight = if_else(value > 0, 1, 0)) %>%
      select(-value) -> out_EJ_R_elec_F_tech_Yh_cool_agg
    # ^^ replaces set_subsector_shrwt function in old system. Used to assign 0/1...
    # ...shareweight to each region/supplysector/subsector/year group

    L2233.out_EJ_R_elec_F_tech_Yh_cool %>%
      left_join_error_no_match(out_EJ_R_elec_F_tech_Yh_cool_agg,
                               by =  c("share.weight.year", "region", "supplysector", "subsector")) %>%
      mutate(tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2233.StubTechProd_elec_cool_EUR  # --OUTPUT-- (note: hydro is removed when written to output)

    L2233.TechMapYr %>%
      # we only need techs with cooling systems from this list;
      # L223.StubTechCapFactor_elec_EUR already contains capacity factors for techs without cooling systems
      filter(from.supplysector != to.supplysector) %>%
      repeat_add_columns(GCAM_region_names) %>%
      filter(from.supplysector %in% L223.StubTechCapFactor_elec_EUR$supplysector,
             from.subsector %in% L223.StubTechCapFactor_elec_EUR$subsector,
             from.technology %in% L223.StubTechCapFactor_elec_EUR$stub.technology,
             year %in% L223.StubTechCapFactor_elec_EUR$year) %>%
      left_join_error_no_match(L223.StubTechCapFactor_elec_EUR,
                               by = c("region", "from.supplysector" = "supplysector",
                                      "from.subsector" = "subsector", "from.technology" = "stub.technology", "year")) %>%
      rename(supplysector = to.supplysector, subsector = to.subsector, stub.technology = to.technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCapFactor"]]) ->
      L2233.StubTechCapFactor_elec_cool_EUR  # --OUTPUT--

    # Hydropower fixed output for base and future periods
    L2233.StubTechProd_elec_cool_EUR %>%
      filter(subsector == "hydro") %>%
      rename(fixedOutput = calOutputValue) %>%
      mutate(subs.share.weight = 0, tech.share.weight = 0) %>%
      bind_rows(L223.StubTechFixOut_hydro_EUR) %>%
      # ^^ binds base years data onto future years
      select(LEVEL2_DATA_NAMES[["StubTechFixOut"]]) ->
      L2233.StubTechFixOut_hydro_EUR  # --OUTPUT--

    # OUTPUTS ===================================================
    L2233.StubTechProd_elecPassthru_EUR %>%
      add_title("Calibrated electricity flow through the pass-through technologies") %>%
      add_units("EJ") %>%
      add_comments("Composed directly from input data") %>%
      add_precursors("energy/calibrated_techs",
                     "L1231.out_EJ_R_elec_F_tech_Yh_EUR",
                     "common/GCAM_region_names") ->
      L2233.StubTechProd_elecPassthru_EUR

    L2233.PassThroughSector_elec_cool_EUR %>%
      add_title("Pass-through sectors with marginal revenue sector and markets", overwrite = TRUE) %>%
      add_units("NA") %>%
      add_comments("Marginal revenue set to electricity; market set to region") %>%
      add_precursors("L223.Supplysector_elec_EUR",
                     "water/elec_tech_water_map",
                     "L223.StubTech_elec_EUR") ->
      L2233.PassThroughSector_elec_cool_EUR

    L2233.Supplysector_elec_cool_EUR %>%
      add_title("Supplysector information for elec cooling", overwrite = TRUE) %>%
      add_units("NA") %>%
      add_comments("Composed directly from input data") %>%
      add_precursors("L223.Supplysector_elec_EUR",
                     "water/elec_tech_water_map",
                     "L223.StubTech_elec_EUR") ->
      L2233.Supplysector_elec_cool_EUR

    L2233.ElecReserve_elec_cool_EUR %>%
      add_title("Electricity reserve margin and average grid capacity factor", overwrite = TRUE) %>%
      add_units("Unitless") %>%
      add_comments("Factors assumed to be same as for electricity and elect_td_bld") %>%
      add_precursors("energy/A23.sector",
                     "L223.Supplysector_elec_EUR",
                     "water/elec_tech_water_map",
                     "L223.StubTech_elec_EUR") ->
      L2233.ElecReserve_elec_cool_EUR

    L2233.SubsectorShrwtFllt_elec_cool_EUR %>%
      add_title("Subsector information for electricity cooling", overwrite = TRUE) %>%
      add_units("Unitless") %>%
      add_comments("Assumes that the subsectors are just pass-through") %>%
      add_comments("Assumes cooling system competition takes place at the technology level") %>%
      add_precursors("water/elec_tech_water_map",
                     "L223.Supplysector_elec_EUR",
                     "L223.StubTech_elec_EUR") ->
      L2233.SubsectorShrwtFllt_elec_cool_EUR

    L2233.SubsectorLogit_elec_cool_EUR %>%
      add_title("Logit exponent for cooling system choice", overwrite = TRUE) %>%
      add_units("Unitless") %>%
      add_comments("Default cooling system logit applied to all sectors") %>%
      add_precursors("water/elec_tech_water_map",
                     "L223.Supplysector_elec_EUR",
                     "L223.StubTech_elec_EUR") ->
      L2233.SubsectorLogit_elec_cool_EUR

    L2233.StubTech_elecPassthru_EUR %>%
      add_title("Stub technologies for electricity sector", overwrite = TRUE) %>%
      add_units("NA") %>%
      add_comments("Technologies repeated across regions") %>%
      add_precursors("L223.StubTech_elec_EUR") ->
      L2233.StubTech_elecPassthru_EUR

    L2233.StubTechTrackCapital_elec_EUR %>%
      add_title("Stub tech to treat capital tracking for rooftop_pv seperately") %>%
      add_units("1975$/GJ") %>%
      add_comments("Since rooftop_pv does not have vintaging we need to track its capital") %>%
      add_comments("with explicit assumptions about depreciation.") %>%
      add_precursors("L223.StubTechCapFactor_elec_EUR", "L223.GlobalIntTechCapital_elec") ->
      L2233.StubTechTrackCapital_elec_EUR

    L2233.StubTech_elec_cool_EUR %>%
      add_title("Stub technologies for cooling system options") %>%
      add_units("NA") %>%
      add_comments("Technologies repeated across regions") %>%
      add_precursors("common/GCAM_region_names",
                     "water/elec_tech_water_map") ->
      L2233.StubTech_elec_cool_EUR

    L2233.StubTechShrwt_elec_cool_EUR %>%
      add_title("Stub technology shareweights for cooling system options") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights joined to L2233.StubTech_elec_cool_EUR") %>%
      add_precursors("common/GCAM_region_names",
                     "water/elec_tech_water_map",
                     "L1233.shrwt_R_elec_cool_Yf_EUR") ->
      L2233.StubTechShrwt_elec_cool_EUR

    L2233.StubTechEff_elec_cool_EUR %>%
      add_title("Calibrated efficiencies of the cooling system options") %>%
      add_units("Unitless") %>%
      add_comments("Expansion of elec_water technologies and their efficiencies for all regions/years") %>%
      add_precursors("common/GCAM_region_names",
                     "water/elec_tech_water_map", "L223.StubTechEff_elec_EUR") ->
      L2233.StubTechEff_elec_cool_EUR

    filter(L2233.StubTechProd_elec_cool_EUR, subsector != "hydro") %>%
      add_title("Calibrated output of the cooling system options") %>%
      add_units("calOutputValue in EJ; share weights are unitless") %>%
      add_comments("Share weights set to 1 if technology level or aggregated subsector level output > 0") %>%
      add_precursors("common/GCAM_region_names",
                     "water/elec_tech_water_map",
                     "L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR") ->
      L2233.StubTechProd_elec_cool_EUR

    L2233.StubTechCapFactor_elec_cool_EUR %>%
      add_title("Region-specific capacity factors of variable technologies with cooling systems") %>%
      add_units("unitless fraction") %>%
      add_comments("Assumptions copied from L223.StubTechCapFactor_elec_EUR") %>%
      add_precursors("L223.StubTechCapFactor_elec_EUR") ->
      L2233.StubTechCapFactor_elec_cool_EUR

    L2233.StubTechFixOut_hydro_EUR %>%
      add_title("Fixed output for hydropower") %>%
      add_units("EJ") %>%
      add_comments("Created by binding base future year fixed hydro outputs") %>%
      add_precursors("common/GCAM_region_names",
                     "water/elec_tech_water_map",
                     "L1233.out_EJ_R_elec_F_tech_Yh_cool_EUR",
                     "L223.StubTechFixOut_hydro_EUR") ->
      L2233.StubTechFixOut_hydro_EUR

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
